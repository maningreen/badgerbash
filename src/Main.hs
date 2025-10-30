module Main (main) where

import Brick (App (..), BrickEvent (AppEvent, VtyEvent), EventM, Location (Location), Widget, attrMap, attrName, customMain, fg, hBox, hLimit, halt, modify, showCursor, showFirstCursor, str, vBox, vLimit, withAttr, (<+>), (<=>))
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Center (center)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.List (intercalate)
import Graphics.Vty (Event (EvKey), Key (KBS, KChar), black, defAttr, red, white)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Platform.Unix as V.Vty
import System.Random (getStdGen)
import Types.WordBank
import Types.WordItem (WordItem (_word))
import Util (breakChunks, count, initSafe, roundTo, snoc, zipWithM, trim)

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
    wmpWid
      | seconds == 0 = str "0"
      | otherwise = str . show . roundTo (1 :: Int) $ wpm
     where
      totalWords = count ' ' typed
      wpm = 60 * (fromIntegral totalWords) / fromIntegral seconds :: Float
    wordsWid = cursor . hLimit width . vLimit height . vBox . map hBox . breakChunks width . space $ wordsToBeDisplayed
     where
      cursor :: Widget () -> Widget ()
      cursor x = Brick.showCursor () (Location (length typed `mod` (width - 1), length typed `div` (width - 1))) x
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
addCharToTyped c (State w typed s) = State w (trim $ snoc typed c) s

dropLastTyped :: State -> State
dropLastTyped (State w typed s) = State w (initSafe typed) s

tickSeconds :: State -> State
tickSeconds (State w t s)
  | null t = State w t 0
  | otherwise = State w t $ s + 1

data CustomEvents = Tick

myMain :: (Ord n) => App s CustomEvents n -> s -> IO s
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
