module Main (main) where

import Brick (App (..), BrickEvent (VtyEvent), EventM, attrMap, attrName, defaultMain, fg, hLimit, halt, modify, neverShowCursor, str, strWrap, vLimit)
import Brick.Widgets.Center (center)
import Control.Monad (void)
import Graphics.Vty (Event (EvKey), Key (KBS, KChar), black, defAttr, red, white)
import System.Random (getStdGen)
import Types.WordBank
import Types.WordItem (WordItem (_word))
import Util (initSafe)

data State = State
  { _words :: [String]
  , _typed :: String
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
  draw s = typedWid s : [wordsWid s]
   where
    wordsWid = center . vLimit height . hLimit width . strWrap . unwords . take 500 . _words
     where
      width = 100
      height = 3
    typedWid = str . _typed
  handleEvent :: BrickEvent () () -> EventM () State ()
  handleEvent (VtyEvent (EvKey (KChar c) [])) = modify $ addCharToTyped c
  handleEvent (VtyEvent (EvKey KBS [])) = modify dropLastTyped
  handleEvent _ = halt

addCharToTyped :: Char -> State -> State
addCharToTyped c (State w typed) = State w $ typed ++ [c]

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
