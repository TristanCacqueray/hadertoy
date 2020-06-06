-- | A demo hadertoy app
module Main (main) where

import Data.Maybe
import Hadertoy
import System.Environment (getArgs)

main :: IO ()
main =
  do
    args <- getArgs
    let fn = fromMaybe "./shaders/demo.glsl" (listToMaybe args)
    (shader, version') <- readShader fn
    let version = fromMaybe "450" version'
    withGLFW version $ \glfw ->
      withWindow glfw 800 800 shader $ \win -> do
        _ <- setEnvCenter win (-0.745, 0.0)
        run fps (update glfw win)
  where
    fps = 25
    update glfw win = do
      paused <- isPaused glfw
      if paused
        then return False
        else render win
