module Config( AdsConfig(..), Config(..), fileConfig ) where

import Control.Exception( throw )
import Data.Aeson( FromJSON(..), eitherDecodeFileStrict )
import Data.Aeson.BetterErrors
import Data.Functor( ($>) )
import Data.List( foldl', isSuffixOf )
import Data.Map.Strict( Map )
import Data.Text( Text )
import Data.Yaml( decodeFileEither )

import qualified Data.Aeson.BetterErrors as A
import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Ast

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

allowedKeys :: Monad m => [Text] -> ParseT Text m ()
allowedKeys keys = do
  o <- asObject
  case filter (not . flip elem keys) (H.keys o) of
    [] -> pure ()
    xs -> throwCustomError ("Unexpected keys: " <> T.intercalate ", " xs)

asMap :: Monad m => ParseT e m a -> ParseT e m (Map Text a)
asMap p = fmap M.fromList (eachInObject p)

asOptList :: Monad m => ParseT e m a -> ParseT e m [a]
asOptList p = fmap (:[]) p <|> eachInArray p

asNonEmptyList :: Monad m => ParseT Text m a -> ParseT Text m (a, [a])
asNonEmptyList p = asOptList p >>= nel
  where
    nel [] = throwCustomError "List is empty"
    nel (h:t) = pure (h, t)

asConfig :: Monad m => ParseT Text m Config
asConfig = do
  _ <- allowedKeys ["destinations", "listeners", "ads"]
  destinations <- keyOrDefault "destinations" M.empty (asMap asDestination)
  listeners <- keyOrDefault "listeners" M.empty (asMap $ asListener asRoutes)
  adsConfig <- keyOrDefault "ads" defaultAdsConfig asAdsConfig
  pure Config {..}

asAdsConfig :: Monad m => ParseT Text m AdsConfig
asAdsConfig = do
  let AdsConfig{..} = defaultAdsConfig
  _ <- allowedKeys ["host", "port", "certificate", "key"]
  h <- keyOrDefault "host" host asString
  p <- keyOrDefault "port" port asIntegral
  c <- keyOrDefault "certificate" certificate asString
  k <- keyOrDefault "key" key asString
  pure (AdsConfig h p c k)

asDestination :: Monad m => ParseT Text m Destination
asDestination = do
  d <- A.key "discovery" asText
  case d of
    "static" -> fmap Static $ allowedKeys["discovery", "hosts"] >> A.key "hosts" (asOptList asHost)
    "strict_dns" -> fmap StrictDns $ allowedKeys ["discovery", "name"] >> A.key "name" asText
    "logical_dns" -> fmap LogicalDns $ allowedKeys ["discovery", "name"] >> A.key "name" asText
    d -> throwCustomError ("can't parse destination: " <> d)
  where
    asHost = do
      _ <- allowedKeys ["host", "port"]
      host <- A.key "host" asText
      port <- A.key "port" asIntegral
      pure (host, port)

data ConfigRoute = ConfigDst Text | ConfigWhen Condition [ConfigRoute]

asRoutes :: Monad m => ParseT Text m Routes
asRoutes = fmap f (asOptList asConfigRoute)
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
          _ <- allowedKeys ["when", "then"]
          pure (ConfigWhen w t)
        _ | Just d <- d -> allowedKeys ["destination"] $> (ConfigDst d)
        _ -> throwCustomError "can't parse route"

asListener :: Monad m => ParseT Text m r -> ParseT Text m (Listener r)
asListener p = do
  host <- keyOrDefault "host" "0.0.0.0" asText
  port <- A.key "port" asIntegral
  http <- A.key "http" p
  _ <- allowedKeys ["host", "port", "http"]
  pure (Listener host port http)

asCondition :: Monad m => ParseT Text m Condition
asCondition = do
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
      _ <- allowedKeys ["header"]
      n <- A.key "header" . A.key "name" $ asText
      pure (Match (Header n) h)
    _ -> throwCustomError "can't parse asCondition"

asMatcher :: Monad m => [Text] -> ParseT Text m Matcher
asMatcher flds = do
  v <- keyMay "value" asText
  p <- keyMay "prefix" asText
  case (v, p) of
    (Just v, Nothing) -> allowedKeys ("value":flds) $> Exact v
    (Nothing, Just p) -> allowedKeys ("prefix":flds) $> Prefix p
    _ -> throwCustomError "Exactly one of [value, prefix] should be specified"
