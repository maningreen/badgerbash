module Main (main) where

import System.Random (getStdGen)
import Types.WordBank
import Types.WordItem (WordItem (_word))

import State (State (..))
import qualified State as S

main :: IO ()
main = do
  x <- readWordBank wordBankPath
  gen <- getStdGen
  state <-
    S.main
      State
        { _words = map _word $ getRandomWords gen x
        , _typed = []
        , _seconds = 0
        , _target = Left 15
        }
  putStr "WPM: "
  print $ S.getWpm state
  putStr "How many words you typed: "
  print . length . words $ _typed state
