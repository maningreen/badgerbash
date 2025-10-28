module Main (main) where

import Brick (App (..), BrickEvent (VtyEvent), EventM, attrMap, attrName, defaultMain, fg, hLimit, halt, modify, neverShowCursor, strWrap, vLimit, attrName, AttrName, str, getContext, vBox, hBox, withAttr)
import Brick.Widgets.Center (center)
import Control.Monad (void)
import Graphics.Vty (Event (EvKey), Key (KBS, KChar), black, defAttr, red, white)
import System.Random (getStdGen)
import Types.WordBank
import Types.WordItem (WordItem (_word))
import qualified Data.Text as T
import Data.Text (Text)
import Util (textInitSafe, applyAttrToListW, breakChunks, merge, zipWithM)
import qualified Data.List as Prelude
import qualified Data.List

data State = State
  { _words :: [String]
  , _typed :: Text
  }

app :: App State () ()
app =
  App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
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
    wordsWid = center . hLimit width . vLimit height . vBox . map hBox . breakChunks width . zipWithM f theString $ unpacked
      where
        f (Just a) (Just b) = withAttr (if a == b then attrName "typed" else attrName "wrong") $ str $ return a
        f (Just a) (Nothing) = withAttr (attrName "default") . str $ return a
    width = 100
    height = 3
    theWords = take 100 $ _words s
    theString = unwords theWords 
    typed = _typed s
    unpacked = T.unpack typed
  handleEvent :: BrickEvent () () -> EventM () State ()
  handleEvent (VtyEvent (EvKey (KChar c) [])) = modify $ addCharToTyped c
  handleEvent (VtyEvent (EvKey KBS [])) = modify dropLastTyped
  handleEvent _ = halt

addCharToTyped :: Char -> State -> State
addCharToTyped c (State w typed) = State w $ T.snoc typed c

dropLastTyped :: State -> State
dropLastTyped (State w typed) = State w $ textInitSafe typed

main :: IO ()
main = do
  x <- readWordBank wordBankPath
  gen <- getStdGen
  void . defaultMain app $
    State
      { _words = map _word $ getRandomWords gen x
      , _typed = T.empty 
      }
