module Main (main) where

import Brick (App (..), BrickEvent (VtyEvent, AppEvent), EventM, Location (Location), Widget, attrMap, attrName, fg, hBox, hLimit, halt, modify, showCursor, showFirstCursor, str, vBox, vLimit, customMain, (<=>), (<+>))
import Brick.Widgets.Center (center)
import Control.Monad (void, forever)
import Graphics.Vty (Event (EvKey), Key (KBS, KChar), black, defAttr, red, white)
import System.Random (getStdGen)
import Types.WordBank
import Types.WordItem (WordItem (_word))
import Util (breakChunks, initSafe, snoc, zipWithM, count, (//))
import Data.List (intercalate)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Platform.Unix as V.Vty
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)

data State = State
  { _words :: [String]
  , _typed :: String
  , _seconds :: Int
  }

app :: App State CustomEvents ()
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
  draw s = return . center $ secondsWid <+> str " " <+> wmpWid <=> wordsWid
   where
    secondsWid = str . show $ seconds
    wmpWid = str . show . (// seconds) . count ' ' $ typed 
    wordsWid = cursor . hLimit width . vLimit height . vBox . map hBox . breakChunks width . space $ wordsToBeDisplayed 
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
    typed = _typed s
    seconds = _seconds s
  handleEvent :: BrickEvent () CustomEvents -> EventM () State ()
  handleEvent (AppEvent Tick) = modify tickSeconds 
  handleEvent (VtyEvent (EvKey (KChar c) [])) = modify $ addCharToTyped c
  handleEvent (VtyEvent (EvKey KBS [])) = modify dropLastTyped
  handleEvent _ = halt

addCharToTyped :: Char -> State -> State
addCharToTyped c (State w typed s) = State w (snoc typed c) s

dropLastTyped :: State -> State
dropLastTyped (State w typed s) = State w (initSafe typed) s

tickSeconds :: State -> State
tickSeconds (State w t s) = State w t $ s + 1

data CustomEvents = Tick

myMain :: Ord n => App s CustomEvents n -> s -> IO s
myMain a start = do
  let buildVty = V.Vty.mkVty V.defaultConfig
  vty <- buildVty
  chan <- newBChan 20
  void . forkIO . forever $ do 
    writeBChan chan Tick
    threadDelay 1000000
  customMain vty buildVty (Just chan) a start

main :: IO ()
main = do
  x <- readWordBank wordBankPath
  gen <- getStdGen
  void . myMain app $
    State
      { _words = map _word $ getRandomWords gen x
      , _typed = []
      , _seconds = 0
      }
