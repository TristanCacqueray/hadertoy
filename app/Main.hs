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
      withWindow glfw 800 800 shader $ \win ->
        run fps (update win)
  where
    fps = 25
    update win = render win
