module Main (main) where

import State (State (..), generateState)
import qualified State as S

main :: IO ()
main = do
  state <- S.main =<< generateState
  putStr "WPM: "
  print $ S.getWpm state
  putStr "How many words you typed: "
  print . length . words $ _typed state
