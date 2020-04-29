module Config( AdsConfig(..), Config(..), fileConfig ) where

import Control.Exception( throw )
import Data.Aeson( FromJSON(..), Value(Array), eitherDecodeFileStrict )
import Data.Aeson.BetterErrors
  ( ParseT
  , asIntegral
  , asObject
  , asString
  , asText
  , asValue
  , eachInArray
  , eachInObject
  , keyOrDefault
  , keyMay
  , throwCustomError
  , toAesonParser )
import Data.Functor( ($>) )
import Data.List( find, foldl', isSuffixOf )
import Data.Map.Strict( Map )
import Data.Text( Text )
import Data.Yaml( decodeFileEither )

import qualified Data.Aeson.BetterErrors as A
import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Ast
import Duration

fileConfig :: FilePath -> IO Config
fileConfig path | ".json" `isSuffixOf` path =
  eitherDecodeFileStrict path >>= either fail pure
fileConfig path | any (\s -> s `isSuffixOf` path) [".yaml", ".yml"] =
  decodeFileEither path >>= either throw pure
fileConfig path = fail ("Unknown file extension: " ++ path)

data Config = Config
  { destinations :: Map Text Destination
  , listeners :: Map Text (Listener Routes)
  , adsConfig :: AdsConfig }

instance FromJSON Config where
  parseJSON = toAesonParser id asConfig

data AdsConfig = AdsConfig
  { host :: String
  , port :: Int
  , certificate :: String
  , key :: String }

defaultAdsConfig = AdsConfig "0.0.0.0" 8080 "certificate.pem" "key.pem"

{- keyOrDefault :: Monad m => Text -> a -> ParseT e m a -> ParseT e m a
keyOrDefault n def p = do
  v <- A.keyMay n asValue
  case v of
    Nothing -> pure def
    Just Null -> pure def
    Just _ -> A.key n p

keyMay :: Monad m => Text -> ParseT e m a -> ParseT e m (Maybe a)
keyMay n p = keyOrDefault n Nothing (fmap Just p) -}

allowedKeys :: Monad m => [Text] -> ParseT Text m ()
allowedKeys keys = do
  o <- asObject
  case find (not . flip elem keys) (H.keys o) of
    Nothing -> pure ()
    Just k -> throwCustomError $
      "Unexpected key " <> k <> ", allowed: [" <> T.intercalate "," keys <> "]"

asDuration :: Monad m => ParseT Text m Duration
asDuration = asText >>= f
  where
    f t =
      case parseDuration t of
        Just (Duration ms) | ms > 315_576_000_000_000 -> throwCustomError "Duration is to large"
        Just d -> pure d
        Nothing -> throwCustomError ("can't parse duration: " <> t)

asPercent :: Monad m => ParseT Text m Int
asPercent = asIntegral >>= f
  where
    f n | 0 <= n && n <= 100 = pure n
    f n = throwCustomError ("percent is out of range: " <> T.pack (show n))

asPort :: Monad m => ParseT Text m Int
asPort = asIntegral >>= f
  where
    f n | 0 < n && n < 65536 = pure n
    f n = throwCustomError ("port is out of range: " <> T.pack (show n))

asMap :: Monad m => ParseT e m a -> ParseT e m (Map Text a)
asMap p = fmap M.fromList (eachInObject p)

asOptList :: Monad m => ParseT e m a -> ParseT e m [a]
asOptList p = asValue >>= f
  where
    f (Array _) = eachInArray p
    f _ = fmap (:[]) p

asNonEmptyList :: Monad m => ParseT Text m a -> ParseT Text m (a, [a])
asNonEmptyList p = asOptList p >>= nel
  where
    nel [] = throwCustomError "List is empty"
    nel (h:t) = pure (h, t)

asConfig :: Monad m => ParseT Text m Config
asConfig = do
  allowedKeys ["destinations", "listeners", "ads"]
  destinations <- keyOrDefault "destinations" M.empty (asMap asDestination)
  listeners <-
    keyOrDefault "listeners" M.empty (asMap . asListener  . asRoutes $ M.keysSet destinations)
  adsConfig <- keyOrDefault "ads" defaultAdsConfig asAdsConfig
  pure Config {..}

asAdsConfig :: Monad m => ParseT Text m AdsConfig
asAdsConfig = do
  let AdsConfig{..} = defaultAdsConfig
  allowedKeys ["host", "port", "certificate", "key"]
  h <- keyOrDefault "host" host asString
  p <- keyOrDefault "port" port asPort
  c <- keyOrDefault "certificate" certificate asString
  k <- keyOrDefault "key" key asString
  pure (AdsConfig h p c k)

asLoadBalancer :: Monad m => ParseT Text m LoadBalancer
asLoadBalancer = do
  allowedKeys ["least-request", "random", "round-robin"]
  lr <- A.keyMay "least-request" asChoiceCount
  rd <- A.keyMay "random" (allowedKeys [])
  rr <- A.keyMay "round-robin" (allowedKeys [])
  case () of
    _ | Just lr <- lr -> allowedKeys ["least-request"] $> LeastRequest lr
    _ | Just () <- rd -> allowedKeys ["random"] $> Random
    _ | Just () <- rr -> allowedKeys ["round-robin"] $> RoundRobin
    _ -> throwCustomError "can't parse load-balancer"
  where
    asChoiceCount :: Monad m => ParseT Text m Int
    asChoiceCount = allowedKeys ["choice-count"] *> A.keyOrDefault "choice-count" 2 asIntegral

asDestination :: Monad m => ParseT Text m Destination
asDestination = do
  allowedKeys
    [ "discovery"
    , "hosts"
    , "connect-timeout"
    , "load-balancer"
    , "circuit-breaker"
    , "outlier-detection" ]
  discovery <- A.key "discovery" asDiscovery
  hosts <- A.keyOrDefault "hosts" [] (asOptList asHost)
  connectTimeout <- A.keyOrDefault "connect-timeout" (seconds 5) asDuration
  loadBalancer <- A.keyOrDefault "load-balancer" (LeastRequest 2) asLoadBalancer
  circuitBreaker <- A.keyMay "circuit-breaker" asCircuitBreaker
  outlierDetection <- A.keyMay "outlier-detection" asOutlierDetection
  pure Destination{..}
  where
    asHost = do
      allowedKeys ["host", "port"]
      host <- A.key "host" asText
      port <- A.key "port" asPort
      pure (host, port)

asOutlierDetection :: Monad m => ParseT Text m OutlierDetection
asOutlierDetection = do
  allowedKeys
    [ "interval"
    , "base-ejection-time"
    , "max-ejection-percent"
    , "consecutive-5xx"
    , "consecutive-gateway-failure"
    , "success-rate"
    , "failure-percentage"
    , "local-origin" ]
  interval <- A.keyMay "interval" asDuration
  baseEjectionTime <- A.keyMay "base-ejection-time" asDuration
  maxEjectionPercent <- A.keyMay "max-ejection-percent" asPercent
  consecutive5xx <- A.keyMay "consecutive-5xx" asConsecutive
  consecutiveGatewayFailure <- A.keyMay "consecutive-gateway-failure" asConsecutive
  successRate <- A.keyMay "success-rate" asSuccessRate
  failurePercentage <- A.keyMay "failure-percentage" asFailurePercentage
  localOrigin <- A.keyMay "local-origin" asLocalOrigin
  pure OutlierDetection{..}

asConsecutive :: Monad m => ParseT Text m Consecutive
asConsecutive = do
  allowedKeys ["enforcing", "num"]
  enforcing <- A.key "enforcing" asPercent
  num <- A.key "num" asIntegral
  pure Consecutive{..}

asSuccessRate :: Monad m => ParseT Text m SuccessRate
asSuccessRate = do
  allowedKeys ["minimum-hosts", "request-volume", "stdev-factor", "enforcing"]
  minimumHosts <- A.key "minimum-hosts" asIntegral
  requestVolume <- A.key "request-volume" asIntegral
  stdevFactor <- A.key "stdev-factor" asIntegral
  enforcing <- A.key "enforcing" asPercent
  pure SuccessRate{..}

asFailurePercentage :: Monad m => ParseT Text m FailurePercentage
asFailurePercentage = do
  allowedKeys ["minimum-hosts", "request-volume", "threshold", "enforcing"]
  minimumHosts <- A.key "minimum-hosts" asIntegral
  requestVolume <- A.key "request-volume" asIntegral
  threshold <- A.key "threshold" asPercent
  enforcing <- A.key "enforcing" asPercent
  pure FailurePercentage{..}

asLocalOrigin :: Monad m => ParseT Text m LocalOrigin
asLocalOrigin = do
  allowedKeys ["consecutive", "success-rate", "failure-percentage"]
  consecutive <- A.keyMay "consecutive" asConsecutive
  let asEnforcing = allowedKeys ["enforcing"] >> A.key "enforcing" asPercent
  enforcingSuccessRate <- A.keyMay "success-rate" asEnforcing
  enforcingFailurePercentage <- A.keyMay "failure-percentage" asEnforcing
  pure LocalOrigin{..}

asCircuitBreaker :: Monad m => ParseT Text m CircuitBreaker
asCircuitBreaker = do
  allowedKeys ["max-connections", "max-pending-requests", "max-requests", "max-retries"]
  mc <- A.keyMay "max-connections" asIntegral
  mp <- A.keyMay "max-pending-requests" asIntegral
  mreq <- A.keyMay "max-requests" asIntegral
  mret <- A.keyMay "max-retries" asIntegral
  pure (CircuitBreaker mc mp mreq mret)

asDiscovery :: Monad m => ParseT Text m Discovery
asDiscovery = asText >>= f
  where
    f "static" = pure Static
    f "strict-dns" = pure StrictDns
    f "logical-dns" = pure LogicalDns
    f s = throwCustomError ("can't parse discovery: " <> s)

data ConfigRoute = ConfigDst Text | ConfigWhen Condition [ConfigRoute]

asRoutes :: (Foldable f, Monad m) => f Text -> ParseT Text m Routes
asRoutes dsts = fmap f (asOptList asConfigRoute)
  where
    f :: [ConfigRoute] -> Routes
    f [] = NoRoute
    f (ConfigDst d : _) = Dst d
    f (ConfigWhen c rs : t) = When c (f rs) (f t)

    asConfigRoute :: Monad m => ParseT Text m ConfigRoute
    asConfigRoute = do
      w <- keyMay "when" asCondition
      d <- keyMay "destination" asText
      case () of
        _ | Just w <- w -> do
          t <- A.key "then" (asOptList asConfigRoute)
          allowedKeys ["when", "then"]
          pure (ConfigWhen w t)
        _ | Just d <- d ->
          if elem d dsts
          then allowedKeys ["destination"] $> (ConfigDst d)
          else throwCustomError ("no such destination: " <> d)
        _ -> throwCustomError "can't parse route"

asListener :: Monad m => ParseT Text m r -> ParseT Text m (Listener r)
asListener p = do
  host <- keyOrDefault "host" "0.0.0.0" asText
  port <- A.key "port" asPort
  http <- A.key "http" p
  allowedKeys ["host", "port", "http"]
  pure (Listener host port http)

asCondition :: Monad m => ParseT Text m Condition
asCondition = do
  allowedKeys ["not", "all", "any", "authority", "method", "path", "header"]
  not <- keyMay "not" asCondition
  all <- keyMay "all" (asNonEmptyList asCondition)
  any <- keyMay "any" (asNonEmptyList asCondition)
  authority <- keyMay "authority" (asMatcher [])
  method <- keyMay "method" (asMatcher [])
  path <- keyMay "path" (asMatcher [])
  header <- keyMay "header" (asMatcher ["name"])
  case () of
    _ | Just n <- not -> allowedKeys ["not"] $> Not n
    _ | Just (x, xs) <- all -> allowedKeys ["all"] $> foldl' And x xs
    _ | Just (x, xs) <- any -> allowedKeys ["any"] $> foldl' Or x xs
    _ | Just a <- authority -> allowedKeys ["authority"] $> Match Authority a
    _ | Just m <- method -> allowedKeys ["method"] $> Match Method m
    _ | Just p <- path -> allowedKeys ["path"] $> Match Path p
    _ | Just h <- header -> do
      allowedKeys ["header"]
      n <- A.key "header" . A.key "name" $ asText
      pure (Match (Header n) h)
    _ -> throwCustomError "can't parse condition"

asMatcher :: Monad m => [Text] -> ParseT Text m Matcher
asMatcher flds = do
  v <- keyMay "value" asText
  p <- keyMay "prefix" asText
  case (v, p) of
    (Just v, Nothing) -> allowedKeys ("value":flds) $> Exact v
    (Nothing, Just p) -> allowedKeys ("prefix":flds) $> Prefix p
    _ -> throwCustomError "Exactly one of [value, prefix] should be specified"
