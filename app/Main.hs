{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A demo hadertoy app
module Main (main) where

import Control.Exception (bracket, bracket_)
import Control.Monad (void)
import Control.Monad.Managed
import Data.Maybe
import DearImGui hiding (render)
import qualified DearImGui
import DearImGui.GLFW
import DearImGui.GLFW.OpenGL
import DearImGui.OpenGL
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Hadertoy
import System.Environment (getArgs)

withControllerWindow :: (GLFW.Window -> IO ()) -> IO ()
withControllerWindow f = bracket mkWindow closeWindow runCallback
  where
    mkWindow :: IO (Maybe GLFW.Window)
    mkWindow = GLFW.createWindow 800 600 "DearController" Nothing Nothing
    closeWindow :: Maybe GLFW.Window -> IO ()
    closeWindow = maybe (return ()) GLFW.destroyWindow
    runCallback :: Maybe GLFW.Window -> IO ()
    runCallback (Just win) = do
      GLFW.makeContextCurrent (Just win)
      GLFW.swapInterval 1
      runManaged $ do
        -- Create an ImGui context
        _ <- managed $ bracket createContext destroyContext

        -- Initialize ImGui's GLFW backend
        _ <- managed_ $ bracket_ (glfwInitForOpenGL win True) glfwShutdown

        -- Initialize ImGui's OpenGL backend
        _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown

        liftIO $ f win

      return ()
    runCallback Nothing = error "GLFW createWindow failed"

renderControllerWindow :: GLFW.Window -> IO ()
renderControllerWindow win = do
  -- Tell ImGui we're starting a new frame
  GLFW.makeContextCurrent (Just win)
  openGL2NewFrame
  glfwNewFrame
  newFrame

  -- Build the GUI
  bracket_ (begin "Hello, ImGui!") end do
    -- Add a text widget
    text "Hello, ImGui!"

    -- Add a button widget, and call 'putStrLn' when it's clicked
    button "Clickety Click" >>= \case
      False -> return ()
      True -> putStrLn "Ow!"

  -- Render
  GL.glClear GL.GL_COLOR_BUFFER_BIT
  DearImGui.render
  openGL2RenderDrawData =<< getDrawData
  GLFW.swapBuffers win

main :: IO ()
main =
  do
    args <- getArgs
    let fn = fromMaybe "./shaders/demo.glsl" (listToMaybe args)
    (shader, version') <- readShader fn
    let version = fromMaybe V120 version'
    withGLFW version $ \glfw ->
      withWindow glfw 800 800 shader $ \win -> do
        withControllerWindow $ \controllerWin -> do
          _ <- setEnvCenter win (-0.745, 0.0)
          withMaybeJulia glfw fn shader win $ \juliaWin -> run fps (update glfw win juliaWin controllerWin)
  where
    withMaybeJulia glfw fn shader mapWin f =
      if fn == "./shaders/mandelbrot.glsl"
        then withWindow glfw 800 800 ("#define JULIA_MODE 1\n" <> shader) $ \win -> do
          setSeed mapWin win defaultJuliaSeed
          setEnvHookClick
            mapWin
            ( \coord -> do
                print $ "Setting new julia seed to " <> show coord
                setSeed mapWin win coord
            )
          f $ Just win
        else f Nothing
    defaultJuliaSeed = (-0.75229, 8.3835006e-2)
    setSeed mapWin juliaWin coord = do
      void $ setEnvSeed mapWin coord
      void $ setEnvSeed juliaWin coord
    fps = 25
    update glfw win juliaWin controllerWin = do
      renderControllerWindow controllerWin
      paused <- isPaused glfw
      if paused
        then return False
        else case juliaWin of
          Just win' -> render win' >>= \c -> if c then return c else render win
          _ -> render win
