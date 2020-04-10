module Config( AdsConfig(..), Config(..), fileConfig ) where

import Control.Exception( throw )
import Data.Aeson
  ( FromJSON
  , Value(Array, Object)
  , eitherDecodeFileStrict
  , parseJSON
  , withObject
  , (.:)
  , (.:?) )
import Data.List( isSuffixOf )
import Data.Map.Strict( Map )
import Data.Maybe( fromMaybe )
import Data.Text( Text )
import Data.Vector( toList )
import Data.Yaml( decodeFileEither )

import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M

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
  parseJSON = withObject "Config" $ \o -> do
    dsts <- o .: "destinations" >>= traverse parseDestination
    lsts <- o .: "listeners" >>= traverse parseListener
    ads <- parseAdsConfig (fromMaybe (Object H.empty) (H.lookup "ads" o))
    pure (Config dsts lsts ads)

data AdsConfig = AdsConfig
  { host :: String
  , port :: Int
  , certificate :: String
  , key :: String }

parseField p o k = maybe
  (fail $ "key " ++ show k ++ " not found")
  p
  (H.lookup k o)

parseOptList (Array a) = fmap toList (traverse parseJSON a)
parseOptList x = fmap pure (parseJSON x)

parseAdsConfig = withObject "AdsConfig" $ \o -> do
  h <- o .:? "host"
  p <- o .:? "port"
  c <- o .:? "certificate"
  k <- o .:? "key"
  pure $ AdsConfig
    (fromMaybe "0.0.0.0" h)
    (fromMaybe 3000 p)
    (fromMaybe "certificate.pem" c)
    (fromMaybe "key.pem" k)

parseDestination = withObject "Destination" $ \o -> do
  d <- o .: "discovery"
  case d :: Text of
    "static" -> do
      hosts <- parseField parseOptList o "hosts"
      fmap Static $ traverse
        (withObject "Host" $ \h -> do
          host <- h .: "host"
          port <- h .: "port"
          pure (host, port)
        )
        hosts
    "strict_dns" -> fmap StrictDns (o .: "name")
    "logical_dns" -> fmap LogicalDns (o .: "name")
    _ -> fail ("Unknown discovery type: " ++ show d)

parseListener = withObject "Listener" $ \o -> do
  host <- o .:? "host"
  port <- o .: "port"
  http <- parseField parseOptList o "http"
  let f [] = pure NoRoute
      f (v : vs) = withObject "Routes" (\o ->
        case (H.lookup "when" o, H.lookup "then" o, H.lookup "destination" o) of
          (Just w, Just t, Nothing) -> do
            w <- parseCondition w
            t <- parseOptList t >>= f
            fmap (When w t) (f vs)
          (Nothing, Nothing, Just d) -> fmap Dst (parseJSON d)
          _ -> fail "You should specify either (when/then) or destination"
        ) v
  routes <- f http
  pure (Listener (fromMaybe "0.0.0.0" host) port routes)

parseCondition = withObject "Condition" $ \o ->
  case
    ( H.lookup "not" o
    , H.lookup "all" o
    , H.lookup "any" o
    , H.lookup "openapi" o
    , H.lookup "authority" o
    , H.lookup "method" o
    , H.lookup "path" o
    , H.lookup "header" o ) of
    (Just n, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) ->
      fmap Not (parseCondition n)
    (Nothing, Just a, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) ->
      let f [] = fail "All: empty"
          f [x] = parseCondition x
          f (h:t) = do
            h <- parseCondition h
            fmap (And h) (f t) in
      parseOptList a >>= f
    (Nothing, Nothing, Just a, Nothing, Nothing, Nothing, Nothing, Nothing) ->
      let f [] = fail "Any: empty"
          f [x] = parseCondition x
          f (h:t) = do
            h <- parseCondition h
            fmap (Or h) (f t) in
      parseOptList a >>= f
    (Nothing, Nothing, Nothing, Just o, Nothing, Nothing, Nothing, Nothing) ->
      error "openapi matching isn't implemented"
    (Nothing, Nothing, Nothing, Nothing, Just a, Nothing, Nothing, Nothing) ->
      fmap (Match Authority) (parseMatcher a)
    (Nothing, Nothing, Nothing, Nothing, Nothing, Just m, Nothing, Nothing) ->
      fmap (Match Method) (parseMatcher m)
    (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just p, Nothing) ->
      fmap (Match Path) (parseMatcher p)
    (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just h@(Object o)) -> do
      name <- o .: "name"
      matcher <- parseMatcher h
      pure (Match (Header name) matcher)
    _ -> fail "Can't parse (multiple matching keys?)"

parseMatcher = withObject "Matcher" $ \o ->
  case (H.lookup "value" o, H.lookup "prefix" o) of
    (Just v, Nothing) -> fmap Exact (parseJSON v)
    (Nothing, Just p) -> fmap Prefix (parseJSON p)
    _ -> fail "Exactly one of (value, prefix) should be specified"
