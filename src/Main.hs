{-# LANGUAGE OverloadedStrings #-}

import Brick
  ( App(..)
  , BrickEvent(AppEvent, VtyEvent)
  , Widget
  , EventM
  , AttrName
  , attrMap
  , customMain
  , halt
  , neverShowCursor
  , vBox
  , str
  , put
  , get
  , withAttr
  , on
  , attrName
  , defaultMain
  )
import Brick.Widgets.Center(center)
import Brick.Widgets.Border(border)
import Graphics.Vty
  ( Vty
  , Event(EvKey)
  , Key(KChar)
  , defaultConfig
  , defAttr
  , string
  , picForImage
  , update
  , nextEvent
  , shutdown
  )
import Graphics.Vty.CrossPlatform (mkVty)
import Brick.BChan
import qualified Graphics.Vty as V
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad(forever, void)
import Control.Monad.IO.Class(liftIO)
import Lens.Micro ((^.))
import System.Process (callCommand)
import System.Timeout (timeout)

-- Phase definition
data Phase = Work | Break deriving (Eq, Show)

-- Application state
data PomodoroState = PomodoroState
  { phase     :: Phase
  , remaining :: Int -- in seconds
  , total     :: Int -- initial total seconds
  , setCount  :: Int
  }

-- Names
data Name = Pomodoro deriving (Eq, Ord, Show)

-- Circle Template
circleTemplate :: [String]
circleTemplate =
  [ "       ●●●●●       "
  , "     ●       ●     "
  , "   ●           ●   "
  , "  ●             ●  "
  , " ●               ● "
  , " ●               ● "
  , " ●               ● "
  , "  ●             ●  "
  , "   ●           ●   "
  , "     ●       ●     "
  , "       ●●●●●       "
  ]

-- Clock positions manually picked (row, col) -- CLOCKWISE!
clockwiseDots :: [(Int, Int)]
clockwiseDots = reverse
  [ (0,7),(0,8),(0,9),(0,10),(0,11) -- top
  , (1,13)
  , (2,15)
  , (3,16)
  , (4,17)
  , (5,17) -- right side
  , (6,17)
  , (7,16)
  , (8,15)
  , (9,13)
  , (10,11),(10,10),(10,9),(10,8),(10,7) -- bottom
  , (9,5)
  , (8,3)
  , (7,2)
  , (6,1)
  , (5,1) -- left side
  , (4,1)
  , (3,2)
  , (2,3)
  , (1,5)
  ]

-- Render the clock face with progress
renderCircle :: Phase -> Int -> Int -> [Widget Name]
renderCircle ph remaining total =
  let totalDots = length clockwiseDots
      dotsToShow = (remaining * totalDots) `div` total
      dotsVisible = take dotsToShow clockwiseDots
      rendered = [ [ if (row, col) `elem` dotsVisible then '●' else ' '
                   | (col, _) <- zip [0..] line]
                 | (row, line) <- zip [0..] circleTemplate]
      widgetLines = map (withAttr (phaseAttr ph) . str) rendered
  in widgetLines

-- Assign color based on Phase
phaseAttr :: Phase -> AttrName
phaseAttr Work = attrName "workAttr"
phaseAttr Break = attrName "breakAttr"

-- Format remaining time
formatTime :: Int -> String
formatTime n = let m = n `div` 60
                   s = n `mod` 60
               in show m ++ "分 " ++ show s ++ "秒"

-- Draw UI
ui :: PomodoroState -> [Widget Name]
ui s =
  [ center (vBox (
      renderCircle (phase s) (remaining s) (total s)
      ++ [ str (showPhase (phase s))
         , str ("残り: " ++ formatTime (remaining s))
         , str ("セット数: " ++ show (setCount s))
         , str "(q で終了)"
         ]
    ))
  ]

showPhase :: Phase -> String
showPhase Work = "🍅作業中"
showPhase Break = "☕休憩中"

-- Handle events
appEvent :: BrickEvent Name () -> EventM Name PomodoroState ()
appEvent (AppEvent ()) = do
  s <- get
  if remaining s <= 0
    then do
      liftIO $ callCommand "afplay done.wav"
      let (nextPhase, nextTime, nextSetCount) = case phase s of
            Work -> (Break, 5 * 60, setCount s + 1)
            Break -> (Work, 25 * 60, setCount s)
      put $ PomodoroState nextPhase nextTime nextTime nextSetCount
    else
      put (s { remaining = remaining s - 1 })
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
appEvent _ = pure ()

-- Setup periodic ticking every second
tickThread :: BChan () -> IO ()
tickThread chan = forever $ do
  writeBChan chan ()
  threadDelay 1000000

-- App definition
app :: App PomodoroState () Name
app = App
  { appDraw = ui
  , appChooseCursor = neverShowCursor
  , appHandleEvent = appEvent
  , appStartEvent = pure ()
  , appAttrMap = const $ attrMap V.defAttr
    [ (attrName "workAttr", V.withForeColor V.defAttr V.red)
    , (attrName "breakAttr", V.withForeColor V.defAttr V.green)
    ]
  }

-- Initial state
initState :: PomodoroState
initState = PomodoroState Work (1 * 60) (1 * 60) 0

main :: IO ()
main = do
  chan <- newBChan 10
  void $ forkIO $ tickThread chan
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app initState
