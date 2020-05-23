{-# LANGUAGE NumericUnderscores #-}

-- |
-- Copyright: (c) 2020 Tristan de Cacqueray
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Tristan de Cacqueray <tristanC@wombatt.eu>
--
-- Glumpy in Haskell
module Hadertoy
  ( Window,
    window,
    render,
    run,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

data Window = Window
  { _glWindow :: GLFW.Window
  }

instance Show Window where
  show _ = "Window[]"

window :: Int -> Int -> IO (Maybe Window)
window width height =
  do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init -- TODO: how to manage this globally?
    unless r (error "GLFW.init failed")
    m <- GLFW.createWindow width height "Hadertoy" Nothing Nothing
    case m of
      (Just win') -> do
        let win = Window win'
        contextCurrent win
        return $ Just win
      Nothing -> return Nothing
  where
    simpleErrorCallback e s =
      putStrLn $ unwords [show e, show s]

contextCurrent :: Window -> IO ()
contextCurrent win = GLFW.makeContextCurrent $ Just (_glWindow win)

render :: Window -> IO Bool
render win = do
  contextCurrent win
  GLFW.swapBuffers (_glWindow win)
  GL.flush
  GLFW.windowShouldClose (_glWindow win)

getTime :: IO Double
getTime = do
  t <- GLFW.getTime
  case t of
    Just tv -> return tv
    Nothing -> print "Error getting time?" >> return 0

-- | An example main loop
run :: Int -> IO Bool -> IO ()
run fps game =
  do
    cur_time <- getTime
    run' 0 (0, cur_time)
  where
    run' :: Int -> (Int, Double) -> IO ()
    run' frame tick = do
      last_time <- getTime
      quit <- game
      unless quit $ do
        -- Ensure we are not running too fast
        cur_time <- getTime
        let elapsed = cur_time - last_time
        when (elapsed < freq) $ threadDelay $ toMicro (freq - elapsed)
        -- Count fps
        let (tick_frame, tick_time) = tick
        new_tick <-
          if (cur_time - tick_time) > 1
            then do
              print $ "fps: " <> show (frame - tick_frame)
              return (frame, cur_time)
            else return tick
        run' (frame + 1) new_tick
    freq :: Double
    freq = 1 / fromIntegral fps
    toMicro :: Double -> Int
    toMicro x = round $ x * 1_000_000
