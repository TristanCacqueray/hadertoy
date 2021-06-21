{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A demo hadertoy app
module Main (main) where

import Control.Exception (bracket, bracket_)
import Control.Monad (forM_, void)
import Control.Monad.Managed
import Data.IORef
import Data.List (foldl')
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import DearImGui hiding (render, withWindow)
import qualified DearImGui
import DearImGui.GLFW
import DearImGui.GLFW.OpenGL
import DearImGui.OpenGL2
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Hadertoy
import System.Environment (getArgs)

--------------------
-- Params Controller
--------------------
withControllerWindow :: [DearParam] -> (Maybe (GLFW.Window, [DearParam]) -> IO ()) -> IO ()
withControllerWindow [] f = f Nothing
withControllerWindow params f = bracket mkWindow closeWindow runCallback
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

        liftIO $ f (Just (win, params))

      return ()
    runCallback Nothing = error "GLFW createWindow failed"

renderControllerWindow :: GLFW.Window -> [DearParam] -> IO ()
renderControllerWindow win xs = do
  -- Tell ImGui we're starting a new frame
  GLFW.makeContextCurrent (Just win)
  openGL2NewFrame
  glfwNewFrame
  newFrame

  -- Build the GUI
  bracket_ (begin "Hello, ImGui!") end do
    forM_ xs $ \(DearParam name _ controller) -> do
      case controller of
        DearScale r mi ma -> sliderFloat (Text.unpack name) r mi ma

  -- Render
  GL.glClear GL.GL_COLOR_BUFFER_BIT
  DearImGui.render
  openGL2RenderDrawData =<< getDrawData
  GLFW.swapBuffers win

data DearController = DearScale (IORef Float) Float Float

getValue :: DearController -> IO Float
getValue = \case
  DearScale ioRef _ _ -> readIORef ioRef

data DearParam = DearParam Text Param DearController

writeParams :: Window -> [DearParam] -> IO ()
writeParams win params = case params of
  [] -> pure ()
  xs -> GLFW.makeContextCurrent (Just (_glWindow win)) >> mapM_ go xs
  where
    go :: DearParam -> IO ()
    go (DearParam _name param controller) = do
      value <- getValue controller
      -- print ("Setting " <> show _name <> " to " <> show value)
      writeParam param (ParamFloat value)

getParamsFromShader :: Window -> Text -> IO [DearParam]
getParamsFromShader win shader = traverse mkParam (parseParams shader)
  where
    mkParam :: (Text, Text) -> IO DearParam
    mkParam (controller, uniform) = case getParam win uniform of
      Just param -> case Text.words controller of
        ["scale", initialValue, minValue, maxValue] -> do
          let initialValue' = read (Text.unpack initialValue)
              minValue' = read (Text.unpack minValue)
              maxValue' = read (Text.unpack maxValue)
          refValue <- newIORef initialValue'
          pure $ DearParam uniform param $ DearScale refValue minValue' maxValue'
        _ -> error ("Unknown dear params " <> show controller)
      Nothing -> error ("Uniform " <> show uniform <> " is not used")

-- | parseParams
-- >>> parseParams "// dear-scale 0.5 0.0 0.1\nuniform float toto;"
-- [("scale 0.5 0.0 0.1","toto")]
parseParams :: Text -> [(Text, Text)]
parseParams shader = snd $ foldl' go (Nothing, []) (Text.lines shader)
  where
    go :: (Maybe Text, [(Text, Text)]) -> Text -> (Maybe Text, [(Text, Text)])
    go (prevLine, acc) line = case prevLine of
      Nothing -> (parseControllerLine line, acc)
      Just controllerLine -> (Nothing, (controllerLine, parseUniformLine line) : acc)
    parseControllerLine :: Text -> Maybe Text
    parseControllerLine line =
      if Text.isPrefixOf "// dear-" line
        then Just $ Text.drop (Text.length "// dear-") line
        else Nothing
    parseUniformLine :: Text -> Text
    parseUniformLine line = case Text.words line of
      ["uniform", _, name] -> Text.reverse $ Text.drop 1 $ Text.reverse name
      _ -> error $ "Not an uniform: " <> show line

--------------------
-- Demo
--------------------
main :: IO ()
main =
  do
    args <- getArgs
    let fn = fromMaybe "./shaders/demo.glsl" (listToMaybe args)
    (shader, version') <- readShader fn
    let version = fromMaybe V120 version'
    withGLFW version $ \glfw ->
      withWindow glfw 800 800 shader $ \win -> do
        params <- getParamsFromShader win shader
        withControllerWindow params $ \controllerWin -> do
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
      case controllerWin of
        Just (controllerWin', params) -> do
          renderControllerWindow controllerWin' params
          writeParams win params
        Nothing -> pure ()
      paused <- isPaused glfw
      if paused
        then return False
        else case juliaWin of
          Just win' -> render win' >>= \c -> if c then return c else render win
          _ -> render win
