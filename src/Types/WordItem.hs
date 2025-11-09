module Types.WordItem (WordItem (..), parseWordMaybe) where

import Data.List ((!?))
import Text.Read (readMaybe)

data WordItem = WordItem
  { _word :: String
  , _weight :: Float
  }
  deriving (Show)

parseWordMaybe :: String -> Maybe WordItem
parseWordMaybe x = WordItem <$> word <*> weight
 where
  broken = words x
  word = broken !? 0
  weight = (broken !? 1) >>= readMaybe
