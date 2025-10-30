module Main (main) where

import Brick (App (..), BrickEvent (VtyEvent), EventM, Location (Location), ViewportType (Vertical), Widget, attrMap, attrName, defaultMain, fg, hBox, hLimit, halt, modify, showCursor, showFirstCursor, str, vBox, vLimit, viewport, withAttr, emptyWidget)
import Brick.Widgets.Center (center)
import Control.Monad (void)
import Graphics.Vty (Event (EvKey), Key (KBS, KChar), black, defAttr, red, white)
import System.Random (getStdGen)
import Types.WordBank
import Types.WordItem (WordItem (_word))
import Util (breakChunks, initSafe, snoc, zipWithM, merge)
import Data.List (intercalate, intersperse)
import Type.Reflection (rnfSomeTypeRep)

data State = State
  { _words :: [String]
  , _typed :: String
  }

app :: App State () ()
app =
  App
    { appDraw = return . draw
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
  draw s = wordsWid
   where
    wordsWid = center . cursor . hLimit width . vLimit height . vBox . map hBox . breakChunks width . space $ wordsToBeDisplayed 
     where
      cursor :: Widget () -> Widget ()
      cursor x = Brick.showCursor () (Location (length typed `mod` width, length typed `div` width)) x
      space = intercalate [str " "]
      wordsToBeDisplayed = zipWithM g theWords $ words typed
        where
          g (Just a) (Just b) = Just $ zipWithM f a b
          g Nothing (Just a) = Just . map ((withAttr (attrName "wrong")) . str . return) $ a 
          g (Just a) (Nothing) = Just . map (withAttr (attrName "default") . str . return) $ a
          g Nothing Nothing = Nothing

      f (Just a) (Just b) = Just . (withAttr (if a == b then attrName "typed" else attrName "wrong") . str . return) $ a
      f (Just a) (Nothing) = Just . (withAttr (attrName "default") . str . return) $ a
      f (Nothing) (Just b) = Just . (withAttr (attrName "wrong") . str . return) $ b
      f _ _ = Nothing
    width = 100
    height = 3
    theWords = take 200 $ _words s
    -- theString = unwords theWords
    typed = _typed s
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
