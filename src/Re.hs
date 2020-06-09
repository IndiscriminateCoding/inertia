module Re where

import Data.Char( isAlphaNum, isAscii )
import Data.Maybe( listToMaybe )
import Data.Text( Text )

import qualified Data.Text as T

data Re
  = Eps
  | Any Re
  | Chr Char Re
  | Alt Re Re
  deriving Eq

instance Show Re where
  show Eps = ""
  show (Any r) = "([.]*)" <> show r
  show (Chr c r) =
    let chr '/' = "/"
        chr c | isAscii c && isAlphaNum c = [c]
        chr c = "\\x" ++ show (fromEnum c) in
    chr c <> show r
  show (Alt Eps r) = "(" <> show r <> ")?"
  show (Alt r Eps) = "(" <> show r <> ")?"
  show (Alt a b) = "(" <> show a <> "|" <> show b <> ")"

literal :: Text -> Re
literal = T.foldr Chr Eps

prefix :: Text -> Re
prefix = T.foldr Chr always

always :: Re
always = Any Eps

alternate :: Re -> Re -> Re
alternate a b | a == b = a
alternate a b | a == always || b == always = always
alternate a b = Alt a b

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
