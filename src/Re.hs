module Re where

import Data.Maybe( listToMaybe )
import Data.Text( Text )

import qualified Data.Text as T

data Re
  = Eps
  | Any Re
  | Chr Char Re
  | Alt Re Re
  deriving (Eq, Show)

lit :: Text -> Re
lit = T.foldr Chr Eps

mergeAlts :: Maybe Re -> Maybe Re -> Maybe Re
mergeAlts Nothing Nothing = Nothing
mergeAlts (Just a) (Just b) = Just (Alt a b)
mergeAlts Nothing a = a
mergeAlts a Nothing = a

merge :: Re -> Re -> Maybe Re
merge r = listToMaybe . mergeL r

mergeL :: Re -> Re -> [Re]
mergeL Eps Eps = [Eps]
mergeL Eps (Any r) = mergeL Eps r
mergeL (Any r) Eps = mergeL r Eps
mergeL Eps (Chr _ _) = []
mergeL (Chr _ _) Eps = []
mergeL x@(Any a) y@(Any b) = map Any (mergeL x b ++ mergeL a y)
mergeL a@(Any _) (Chr c b) = map (Chr c) (mergeL a b)
mergeL b@(Chr _ _) a@(Any _) = mergeL a b
mergeL (Chr c r) (Chr c' r') | c == c' = map (Chr c) (mergeL r r')
mergeL (Chr _ _) (Chr _ _) = []
mergeL (Alt a b) r = do
  a <- mergeL a r
  b <- mergeL b r
  pure (Alt a b)
mergeL a b@(Alt _ _) = mergeL b a
