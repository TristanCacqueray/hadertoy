-- | A demo hadertoy app
module Main (main) where

import Control.Monad (void)
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
        withMaybeJulia glfw fn shader win $ \juliaWin -> run fps (update glfw win juliaWin)
  where
    withMaybeJulia glfw fn shader mapWin f =
      if fn == "./shaders/mandelbrot.glsl"
        then withWindow glfw 800 800 ("#define JULIA_MODE 1\n" <> shader) $ \win -> do
          _ <- setEnvSeed win (-1.255, 0.03)
          setEnvHookClick
            mapWin
            ( \coord -> do
                print $ "Setting new julia seed to " <> show coord
                void $ setEnvSeed win coord
            )
          f $ Just win
        else f Nothing
    fps = 25
    update glfw win juliaWin = do
      paused <- isPaused glfw
      if paused
        then return False
        else case juliaWin of
          Just win' -> render win' >>= \c -> if c then return c else render win
          _ -> render win
