module Main (main) where

import System.Random (getStdGen)
import Types.WordBank

main :: IO ()
main = do
  x <- readWordBank wordBankPath
  gen <- getStdGen
  print $ pickRandomItemWeighted gen x
  return ()
