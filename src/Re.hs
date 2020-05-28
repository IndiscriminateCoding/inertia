module Re where

import Data.Maybe( maybeToList )
import Data.Text( Text )

import qualified Data.Text as T

data Re
  = Any
  | Str Text
  | Cat Re Re
  | Alt Re Re
  deriving Show

matches :: Re -> Text -> [Text]
matches Any t = T.tails t
matches (Str s) t = maybeToList (T.stripPrefix s t)
matches (Cat a b) t = matches a t >>= matches b
matches (Alt a b) t = matches a t ++ matches b t

matchExact :: Re -> Text -> Bool
matchExact re txt = matches re txt == [""]

matchPrefix :: Re -> Text -> Bool
matchPrefix re txt = matches re txt /= []
