module Main (main) where

import Brick (App (..), BrickEvent (AppEvent, VtyEvent), EventM, Location (Location), Widget, attrMap, attrName, customMain, fg, get, hBox, hLimit, halt, modify, showCursor, showFirstCursor, str, vBox, vLimit, withAttr, (<+>), (<=>))
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Center (center)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.Function (on)
import Data.List (intercalate)
import Graphics.Vty (Event (EvKey), Key (KBS, KChar), black, defAttr, red, white)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Platform.Unix as V.Vty
import System.Random (getStdGen)
import Types.WordBank
import Types.WordItem (WordItem (_word))
import Util (breakChunks, initSafe, roundTo, snoc, trim, zipWithM)

type Time = Int

data State = State
  { _words :: [String],
    _typed :: String,
    _seconds :: Time,
    _target :: Either Time Int -- Either Time or Words
  }

app :: App State CustomEvents ()
app =
  App
    { appDraw = draw,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap =
        const $
          attrMap
            defAttr
            [ (attrName "default", fg black),
              (attrName "wrong", fg red),
              (attrName "typed", fg white)
            ]
    }
  where
    draw s = return . center $ secondsWid <+> str " " <+> wmpWid <=> wordsWid
      where
        secondsWid = str . show $ seconds
        wmpWid = str . show . roundTo (1 :: Int) $ getWpm s

        wordsWid = cursor . hLimit width . vLimit height . vBox . map hBox . breakChunks width . space $ wordsToBeDisplayed
          where
            cursor :: Widget () -> Widget ()
            cursor x = Brick.showCursor () (Location (xPos, yPos)) x
              where
                xPos = index `mod` (width - 1)
                yPos = index `div` (width - 1) :: Int
                index
                  | null typedWords = 0
                  | otherwise =
                      (length typedWords - 1)
                        + (sum $ zipWith (on max length) (init typedWords) theWords)
                        + length lastTypedWord
                        + if last typed == ' ' then 1 else 0
                  where

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
        typedWords = words typed
        lastTypedWord = last typedWords
        seconds = _seconds s
    handleEvent :: BrickEvent () CustomEvents -> EventM () State ()
    handleEvent (AppEvent Tick) = do
      timesUp <- getGamesUp <$> get
      if not timesUp
        then modify tickSeconds
        else halt
    handleEvent (VtyEvent (EvKey (KChar c) [])) = modify $ addCharToTyped c
    handleEvent (VtyEvent (EvKey KBS [])) = modify dropLastTyped
    handleEvent _ = halt

addCharToTyped :: Char -> State -> State
addCharToTyped c (State w typed s t) = State w (trim $ snoc typed c) s t

dropLastTyped :: State -> State
dropLastTyped (State w typed s t) = State w (initSafe typed) s t

getGamesUp :: State -> Bool
getGamesUp (State _ _ seconds (Left time)) = seconds >= time
getGamesUp (State _ typed _ (Right wordCount)) = length (words typed) >= wordCount

tickSeconds :: State -> State
tickSeconds (State w t s target)
  | null t = State w t 0 target
  | otherwise = State w t (s + 1) target

getWpm :: State -> Float
getWpm state
  | isNaN wpm || isInfinite wpm = 0
  | otherwise = wpm
  where
    wpm = 60 * (fromIntegral . length . words $ _typed state) / (fromIntegral . _seconds $ state)

data CustomEvents = Tick

myMain :: App s CustomEvents () -> s -> IO s
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
  state <-
    myMain app $
      State
        { _words = map _word $ getRandomWords gen x,
          _typed = [],
          _seconds = 0,
          _target = Left 15
        }
  putStr "WPM: "
  print $ getWpm state
  putStr "How many words you typed: "
  print . length . words $ _typed state
