module State (
  State (..),
  Time,
  main,
  getWpm,
  getStateTime,
  delay,
  microDelay,
) where

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
import Util (breakChunks, initSafe, log10, roundTo, snoc, trim, zipWithM)

-- #### TYPES ####

data CustomEvents = Tick

type Time = Float

data State = State
  { _words :: [String]
  , _typed :: String
  , _seconds :: Time
  , _target :: Either Time Int -- Either Time or Words
  }

data WidgetID = WidgetID () | WidgetId Int
  deriving (Eq, Ord)

-- #### CONSTANTS ####

app :: App State CustomEvents WidgetID
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

-- the delay, *in seconds*, for microseconds see microDelay
delay :: Time
delay = 0.1

microDelay :: Int
microDelay = round $ delay * 1000000

-- #### INIT FUNCTIONS ####

main :: State -> IO State
main start = do
  let buildVty = V.Vty.mkVty V.defaultConfig
  vty <- buildVty
  chan <- newBChan 20
  void . forkIO . forever $ writeBChan chan Tick >> threadDelay microDelay
  customMain vty buildVty (Just chan) app start

-- #### STATE INFO ####

-- returns the time truncated
getStateTime :: State -> Float
getStateTime = roundTo digitCount . _seconds
 where
  digitCount = round $ log10 (1 / delay) :: Int

getWpm :: State -> Float
getWpm state
  | isNaN wpm || isInfinite wpm = 0
  | otherwise = wpm
 where
  wpm = 60 * (fromIntegral . length . words $ _typed state) / (getStateTime state)

-- #### STATE MANIPULATIONS #####

addCharToTyped :: Char -> State -> State
addCharToTyped c (State w typed s t) = State w (trim $ snoc typed c) s t

dropLastTyped :: State -> State
dropLastTyped (State w typed s t) = State w (initSafe typed) s t

getGamesUp :: State -> Bool
getGamesUp (State _ _ seconds (Left time)) = seconds >= time
getGamesUp (State _ typed _ (Right wordCount)) = length (words typed) >= wordCount

tick :: State -> State
tick (State w t s target)
  | null t = State w t 0 target
  | otherwise = State w t (s + delay) target

setTarget :: Either Time Int -> State -> State
setTarget target (State a b c _) = State a b c target

-- #### APP FUNCTIONS ####

draw :: State -> [Widget WidgetID]
draw s = return . center $ secondsWid <+> str " " <+> wmpWid <=> wordsWid
 where
  secondsWid = str . show $ seconds
  wmpWid = str . show . roundTo (1 :: Int) $ getWpm s

  wordsWid = cursor . hLimit width . vLimit height . vBox . map hBox . breakChunks width . space $ wordsToBeDisplayed
   where
    cursor :: Widget WidgetID -> Widget WidgetID
    cursor x = Brick.showCursor (WidgetID ()) (Location (xPos, yPos)) x
     where
      xPos = index `mod` (width - 1)
      yPos = index `div` (width - 1) :: Int
      index
        | null typedWords = 0
        | otherwise =
            (length typedWords - 1)
              + (sum $ zipWith (on max length) (init typedWords) theWords)
              + length lastTypedWord
              + if last typed == ' '
                then 1
                else 0
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
  seconds = getStateTime s

handleEvent :: BrickEvent WidgetID CustomEvents -> EventM WidgetID State ()
handleEvent (AppEvent Tick) = do
  timesUp <- getGamesUp <$> get
  if not timesUp
    then modify tick
    else halt
handleEvent (VtyEvent (EvKey (KChar c) [])) = modify $ addCharToTyped c
handleEvent (VtyEvent (EvKey KBS [])) = modify dropLastTyped
handleEvent _ = halt
