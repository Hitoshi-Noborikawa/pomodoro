{-# LANGUAGE OverloadedStrings #-}

import Brick
  ( App(..)
  , BrickEvent(AppEvent, VtyEvent)
  , Widget
  , EventM
  , attrMap
  , customMain
  , halt
  , neverShowCursor
  , vBox
  , str
  , put
  , get
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
import Lens.Micro ((^.))
import System.Process (callCommand)
import System.Timeout (timeout)

-- Phase definition
data Phase = Work | Break deriving (Eq, Show)

-- Application state
data PomodoroState = PomodoroState
  { phase     :: Phase
  , remaining :: Int -- in seconds
  , setCount  :: Int
  }

-- Initial state
initState :: PomodoroState
initState = PomodoroState Work (1 * 60) 0

-- Names
data Name = Pomodoro deriving (Eq, Ord, Show)

-- Draw UI
ui :: PomodoroState -> [Widget Name]
ui s =
  [ center . border . vBox $
    [ str $ "状態: " ++ showPhase (phase s)
    , str $ "残り: " ++ formatTime (remaining s)
    , str $ "セット数: " ++ show (setCount s)
    , str "(q で終了)"
    ]
  ]

showPhase :: Phase -> String
showPhase Work = "🍅作業中"
showPhase Break = "☕休憩中"

formatTime :: Int -> String
formatTime n = let m = n `div` 60
                   s = n `mod` 60
               in show m ++ "分 " ++ show s ++ "秒"

-- Handle events
appEvent :: BrickEvent Name () -> EventM Name PomodoroState ()
appEvent (AppEvent ()) = do
  s <- get
  if remaining s <= 0
    then do
      let (nextPhase, nextTime, nextSetCount) = case phase s of
            Work -> (Break, 5 * 60, setCount s + 1)
            Break -> (Work, 25 * 60, setCount s)
      put (PomodoroState nextPhase nextTime nextSetCount)
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
  , appAttrMap = const $ attrMap V.defAttr []
  }

main :: IO ()
main = do
  chan <- newBChan 10
  void $ forkIO $ tickThread chan
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app initState
