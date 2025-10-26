module Main (main) where

import Data.List ((!?))
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import GHC.IO.Handle (hGetContents)
import GHC.IO.Handle.FD (openFile)
import GHC.IO.IOMode (IOMode (ReadMode))
import System.Random (Random (randomR), StdGen, getStdGen)
import Text.Read (readMaybe)

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

foldFind :: (b -> a -> (b, Bool)) -> b -> [a] -> (b, Maybe a)
foldFind _ acc [] = (acc, Nothing)
foldFind f acc (x : xs)
  | done = (acc', Just x)
  | otherwise = foldFind f acc' xs
 where
  (acc', done) = f acc x

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

pickItem :: StdGen -> WordBank -> (WordItem, StdGen)
pickItem gen bank = case getItemFromWeight chosenWeight bank of
  Just x -> (x, next)
 where
  (chosenWeight, next) = randomR (_weight . V.head . _items $ bank, _totalWeight bank) gen :: (Float, StdGen)

main :: IO ()
main = do
  x <- readWordBank wordBankPath
  gen <- getStdGen
  print $ pickItem gen x
  return ()
