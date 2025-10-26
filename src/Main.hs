module Main (main) where

import System.Random (getStdGen)
import Types.WordBank
import Brick (App(..), str, halt, neverShowCursor, attrMap, fg, attrName, defaultMain)
import Graphics.Vty (defAttr, white)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)

app :: App () () ()
app = App {
    appDraw = const . return . center . border $ str "cool"
  , appChooseCursor = neverShowCursor
  , appHandleEvent = const halt
  , appStartEvent = return ()
  , appAttrMap = const $ attrMap defAttr [
      (attrName "default", fg white)
    ]
  }

main :: IO ()
main = do
  x <- readWordBank wordBankPath
  gen <- getStdGen
  print $ pickRandomItemWeighted gen x
  defaultMain app ()
  return ()
