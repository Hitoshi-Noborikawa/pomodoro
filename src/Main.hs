{-# LANGUAGE OverloadedStrings #-}

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
import Control.Concurrent (threadDelay)
import Control.Monad()
import System.Process (callCommand)
import System.Timeout (timeout)

data Phase = Work | Break deriving (Eq, Show)

main :: IO ()
main = do
  vty <- mkVty defaultConfig
  runPomodoro vty Work 1 0
  shutdown vty

runPomodoro :: Vty -> Phase -> Int -> Int -> IO ()
runPomodoro vty phase minutes setCount = do
  let totalSeconds = minutes * 60
  countdown vty phase totalSeconds setCount

countdown :: Vty -> Phase -> Int -> Int -> IO ()
countdown vty phase seconds setCount = loop seconds
  where
    loop n = do
      let mins = n `div` 60
          secs = n `mod` 60
          status = case phase of
            Work -> "作業中 🍅"
            Break -> "休憩中 ☕"

          timeText =
            status ++
            " - セット数:" ++ show setCount ++
            " - 残り: " ++ show mins ++ "分 " ++ show secs ++ "秒" ++
            "（qで終了）"
          img = string defAttr timeText

      update vty (picForImage img)

      -- 1秒以内に何かキーが押されたらチェックする
      mEvent <- timeout 1000000 (nextEvent vty)
      case mEvent of
        Just (EvKey (KChar 'q') []) -> return () -- qで終了
        _ -> if n <= 0
              then do
                callCommand "afplay done.wav"
                nextPhase vty phase setCount
              else loop (n - 1)

nextPhase :: Vty -> Phase -> Int -> IO ()
nextPhase vty Work setCount  = runPomodoro vty Break 5 (setCount + 1)  -- 作業→休憩
nextPhase vty Break setCount = runPomodoro vty Work 25 setCount        -- 休憩→作業
