module Types.WordItem (WordItem(..), parseWordMaybe) where

import Text.Read (readMaybe)
import Data.List ((!?))

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

