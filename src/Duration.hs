module Duration where

import Data.Char( isDigit )
import Text.Read( readMaybe )

import qualified Data.Text as T

newtype Duration = Duration { toMillis :: Integer }
  deriving (Eq, Num)

toSeconds :: Duration -> Integer
toSeconds = (`div` 1000) . toMillis

millis :: Integer -> Duration
millis = Duration

seconds :: Integer -> Duration
seconds = Duration . (* 1000)

parseDuration :: T.Text -> Maybe Duration
parseDuration t = do
  ms <- readMaybe (reverse val)
  case reverse unit of
    u | elem u ["seconds", "sec", "s"] -> pure (seconds ms)
    u | elem u ["milliseconds", "millis", "ms"] -> pure (millis ms)
    _ -> Nothing
  where
    (val, unit) = T.foldl split ([], []) t
    split (v, u) ' ' = (v, u)
    split (v, u) c | isDigit c = (c:v, u)
    split (v, u) c = (v, c:u)
