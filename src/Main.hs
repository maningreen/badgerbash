module Main (main) where

import Brick (App (..), BrickEvent (VtyEvent), EventM, ViewportType (Vertical), Widget, attrMap, attrName, defaultMain, fg, hBox, hLimit, halt, modify, str, vBox, vLimit, viewport, withAttr, showCursor, Location (Location), showFirstCursor)
import Brick.Widgets.Center (center)
import Control.Monad (void)
import Graphics.Vty (Event (EvKey), Key (KBS, KChar), black, defAttr, red, white)
import System.Random (getStdGen)
import Types.WordBank
import Types.WordItem (WordItem (_word))
import Util (snoc, breakChunks, zipWithM, initSafe)

data State = State
  { _words :: [String]
  , _typed :: String
  }

app :: App State () ()
app =
  App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap =
        const $
          attrMap
            defAttr
            [ (attrName "default", fg black)
            , (attrName "wrong", fg red)
            , (attrName "typed", fg white)
            ]
    }
 where
  draw s = return wordsWid
   where
    wordsWid = style . viewport () Vertical . vBox . map hBox . breakChunks width . zipWithM f theString $ unpacked :: Widget ()
     where
      style = center . cursor . hLimit width . vLimit height
        where
          cursor :: Widget () -> Widget ()
          cursor x = Brick.showCursor () (Location (length typed `mod` width, length typed `div` width)) x
      f (Just a) (Just b) = withAttr (if a == b then attrName "typed" else attrName "wrong") $ str $ return a
      f (Just a) (Nothing) = withAttr (attrName "default") . str $ return a
      f (Nothing) (Just b) = withAttr (attrName "wrong") . str $ return b
      f _ _ = undefined
    width = 100
    height = 3
    theWords = take 200 $ _words s
    theString = unwords theWords
    typed = _typed s
    unpacked = typed
  handleEvent :: BrickEvent () () -> EventM () State ()
  handleEvent (VtyEvent (EvKey (KChar c) [])) = modify $ addCharToTyped c
  handleEvent (VtyEvent (EvKey KBS [])) = modify dropLastTyped
  handleEvent _ = halt

addCharToTyped :: Char -> State -> State
addCharToTyped c (State w typed) = State w $ snoc typed c

dropLastTyped :: State -> State
dropLastTyped (State w typed) = State w $ initSafe typed

main :: IO ()
main = do
  x <- readWordBank wordBankPath
  gen <- getStdGen
  void . defaultMain app $
    State
      { _words = map _word $ getRandomWords gen x
      , _typed = []
      }
