module Main (main) where

import Data.List ((!?))
import Data.Maybe (mapMaybe)
import GHC.IO.Handle (hGetContents)
import GHC.IO.Handle.FD (openFile)
import GHC.IO.IOMode (IOMode (ReadMode))
import qualified Data.Vector as V
import Text.Read (readMaybe)
import System.Random (StdGen, Random (randomR), getStdGen)

data WordItem = WordItem
  { _word :: String
  , _weight :: Float
  }
  deriving (Show)

data WordBank = WordBank
  { _items :: V.Vector WordItem
  , _totalWeight :: Float
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
  items = V.fromList . mapMaybe parseWordMaybe $ lines x
  total = sum . V.map _weight $ items

readWordBank :: FilePath -> IO WordBank
readWordBank x = fmap parseWordBank . hGetContents =<< openFile x ReadMode

pickItem :: StdGen -> WordBank -> (WordItem, StdGen)
pickItem gen bank = (chosenX, next)
  where
    maxI = V.length $ _items bank
    chosenX = (_items bank) V.! chosenI 
    (chosenI, next) = randomR (0, maxI) gen :: (Int, StdGen)

main :: IO ()
main = do
  x <- readWordBank wordBankPath
  print x
  gen <- getStdGen
  print $ pickItem gen x
  return ()
