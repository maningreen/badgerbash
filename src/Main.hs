module Main (main) where

import Data.List ((!?))
import Data.Maybe (mapMaybe)
import GHC.IO.Handle (hGetContents)
import GHC.IO.Handle.FD (openFile)
import GHC.IO.IOMode (IOMode (ReadMode))
import Text.Read (readMaybe)

data WordItem = WordItem
  { _word :: String
  , _weight :: Float
  }
  deriving (Show)
data WordBank = WordBank { 
    _items :: [WordItem],
    _totalWeight :: Float
  }
  deriving (Show)

wordBankPath :: FilePath
wordBankPath = "wordbank"

parseWordMaybe :: String -> Maybe WordItem
parseWordMaybe x = WordItem <$> word <*> weight
 where
  broken = words x
  word = broken !? 0
  weight = (broken !? 1) >>= readMaybe

parseWordBank :: String -> WordBank
parseWordBank x = WordBank items total
  where
    items = mapMaybe parseWordMaybe $ lines x
    total = sum . map _weight $ items

readWordBank :: FilePath -> IO WordBank
readWordBank x = fmap (parseWordBank) . hGetContents =<< openFile x ReadMode

main :: IO ()
main = do
  readWordBank wordBankPath >>= print
  return ()
