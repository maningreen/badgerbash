module Types.WordBank where

import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import GHC.IO.Handle (hGetContents)
import GHC.IO.Handle.FD (openFile)
import GHC.IO.IOMode (IOMode (ReadMode))
import System.Random (Random (randomR), StdGen)
import Types.WordItem (WordItem (..), parseWordMaybe)
import Util (foldFind)

data WordBank = WordBank
  { _items :: V.Vector WordItem,
    _totalWeight :: Float
  }
  deriving (Show)

parseWordBank :: String -> WordBank
parseWordBank x = WordBank items total
  where
    items = V.fromList . mapMaybe parseWordMaybe $ lines x
    total = sum . V.map _weight $ items

getItemFromWeight :: Float -> WordBank -> Maybe WordItem
getItemFromWeight tauWeight bank = snd chosen
  where
    chosen = foldFind f 0 items
    f :: Float -> WordItem -> (Float, Bool)
    f sigmaWeight (WordItem _ weight) = (sigmaWeight', sigmaWeight' >= tauWeight)
      where
        sigmaWeight' = weight + sigmaWeight
    items = V.toList $ _items bank

readWordBank :: FilePath -> IO WordBank
readWordBank x = fmap parseWordBank . hGetContents =<< openFile x ReadMode

pickRandomItemWeighted :: StdGen -> WordBank -> (WordItem, StdGen)
pickRandomItemWeighted gen bank = case getItemFromWeight chosenWeight bank of
  Just x -> (x, next)
  where
    (chosenWeight, next) = randomR (0, _totalWeight bank) gen :: (Float, StdGen)

-- this function never ends, calling itself forever
getRandomWords :: StdGen -> WordBank -> [WordItem]
getRandomWords gen bank = item : (getRandomWords nextGen bank)
  where
    (item, nextGen) = pickRandomItemWeighted gen bank

wordBankPath :: FilePath
wordBankPath = "wordbank"
