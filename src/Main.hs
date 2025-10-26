module Main (main) where

import System.Random (getStdGen, StdGen)
import Types.WordBank
import Brick (App(..), str, halt, neverShowCursor, attrMap, fg, attrName, defaultMain, vBox)
import Graphics.Vty (defAttr, white)
import Control.Monad (void)
import Types.WordItem (WordItem(_word))

data State = State {
    _wordBank :: WordBank
  , _stdGen :: StdGen
  }

app :: App State () ()
app = App {
    appDraw = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent = const halt
  , appStartEvent = return ()
  , appAttrMap = const $ attrMap defAttr [
      (attrName "default", fg white)
    ]
  }
  where
    draw (State x gen) = return . vBox $ take 8 . map (str . _word) $ getRandomWords gen x

main :: IO ()
main = do
  x <- readWordBank wordBankPath
  gen <- getStdGen
  void . defaultMain app $ State {
      _wordBank = x
    , _stdGen = gen
    }
