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
        withMaybeJulia glfw fn shader $ \juliaWin -> run fps (update glfw win juliaWin)
  where
    withMaybeJulia glfw fn shader f =
      if fn == "./shaders/mandelbrot.glsl"
        then withWindow glfw 800 800 ("#define JULIA_MODE 1\n" <> shader) (f . Just)
        else f Nothing
    fps = 25
    update glfw win juliaWin = do
      paused <- isPaused glfw
      if paused
        then return False
        else case juliaWin of
          Just win' -> render win' >>= \c -> if c then return c else render win
          _ -> render win
