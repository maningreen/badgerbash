module Main (main) where

import System.Random (getStdGen)
import Types.WordBank
import Brick (App(..), halt, neverShowCursor, attrMap, fg, attrName, defaultMain, strWrap, EventM, BrickEvent (VtyEvent))
import Graphics.Vty (defAttr, white, Key (KChar), Event (EvKey), black, red)
import Control.Monad (void)
import Types.WordItem (WordItem(_word))

data State = State {
    _words :: [String]
  , _typed :: String
  }

app :: App State () ()
app = App {
    appDraw = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const $ attrMap defAttr [
      (attrName "default", fg black)
    , (attrName "wrong", fg red)
    , (attrName "typed", fg white)
    ]
  }
  where
    draw = return . strWrap . unwords . take 100 . _words
      where
    handleEvent :: BrickEvent () () -> EventM () State ()
    handleEvent (VtyEvent (EvKey (KChar _) [])) = return ()
    handleEvent _ = halt

main :: IO ()
main = do
  x <- readWordBank wordBankPath
  gen <- getStdGen
  void . defaultMain app $ State {
      _words = map _word $ getRandomWords gen x
    , _typed = []
    }
