module OpenApi( Endpoint(..), parseFile ) where

import Control.Exception( throw )
import Data.Aeson( FromJSON(..), eitherDecodeFileStrict )
import Data.Aeson.BetterErrors
  ( ParseT
  , asObject
  , asText
  , eachInObject
  , keyOrDefault
  , keyMay
  , throwCustomError
  , toAesonParser )
import Data.List( isSuffixOf )
import Data.Yaml( decodeFileEither )

import qualified Data.Text as T

import Re( Re )
import qualified Re

data Endpoint = Endpoint
  { path :: Re
  , method :: T.Text }
  deriving Show

newtype Endpoints = Endpoints { runEndpoints :: [Endpoint] }

instance FromJSON Endpoints where
  parseJSON = toAesonParser id (fmap Endpoints asEndpoints)

asEndpoints :: Monad m => ParseT T.Text m [Endpoint]
asEndpoints = do
  swagger <- keyMay "swagger" asText
  openapi <- keyMay "openapi" asText
  basePath <- keyOrDefault "basePath" "/" asText
  prefix <-
    case (swagger, openapi, basePath) of
      (Nothing, Just _, "/") -> pure ""
      (Just _, Nothing, "/") -> pure ""
      (Just _, Nothing, p) -> pure p
      _ -> throwCustomError "either (swagger & basePath) or (openapi) should be specified"
  paths <- keyOrDefault "paths" [] (eachInObject . eachInObject $ asObject)
  pure $ do
    (path', methods) <- paths
    let path = prefix <> path'
    (method, _) <- methods
    [Endpoint (re $ segments path) (T.toUpper method)]
  where
    re :: [Maybe T.Text] -> Re
    re =
      let f r Nothing = Re.Any r
          f r (Just t) = T.foldr Re.Chr r t in
      foldl f (Re.Alt Re.Eps (Re.Chr '/' Re.Eps))

    segments :: T.Text -> [Maybe T.Text]
    segments p | not (T.null p) && T.last p == '/' = segments (T.init p)
    segments p = (map (fmap $ T.reverse . T.pack)) (outer (T.unpack p) [])

    outer :: String -> [Maybe String] -> [Maybe String]
    outer "" a = a
    outer ('{':t) a = inner t (Nothing:a)
    outer (h:t) (Just x:a) = outer t (Just (h:x):a)
    outer (h:t) a = outer t (Just [h]:a)

    inner :: String -> [Maybe String] -> [Maybe String]
    inner "" a = a
    inner ('}':t) a = outer t a
    inner (_:t) a = inner t a

parseFile :: FilePath -> IO [Endpoint]
parseFile = fmap runEndpoints . read
  where
    read :: FilePath -> IO Endpoints
    read path | ".json" `isSuffixOf` path =
      eitherDecodeFileStrict path >>= either fail pure
    read path | any (\s -> s `isSuffixOf` path) [".yaml", ".yml"] =
      decodeFileEither path >>= either throw pure
    read path = fail ("Unknown file extension: " ++ path)
