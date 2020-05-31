{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: (c) 2020 Tristan de Cacqueray
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Tristan de Cacqueray <tristanC@wombatt.eu>
--
-- Glumpy in Haskell
--
-- The goal of this library is to provide an abstraction similar to Glumpy,
-- for example:
-- https://github.com/glumpy/glumpy/blob/master/examples/shadertoy-template.py
--
-- Until this is complete, a more simple interface is provided to load and update
-- shader code similar to shadertoy.
--
-- The current implementation is mostly adapted from these sources:
-- https://github.com/ocharles/blog/blob/master/code/2013-12-02-linear-example.hs
-- https://github.com/acowley/GLUtil/blob/master/examples/example1.hs
-- https://github.com/bergey/haskell-OpenGL-examples/blob/master/glfw/Modern.hs
module Hadertoy
  ( Initialized,
    withGLFW,
    Window,
    withWindow,
    getParam,
    writeParam,
    readShader,
    render,
    run,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector.Storable as V
import GHC.Float (double2Float, double2Int)
import qualified Graphics.GL.Core31 as GLR
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import System.Environment (lookupEnv, setEnv)

newtype Initialized = Init T.Text

withGLFW :: String -> (Initialized -> IO ()) -> IO ()
withGLFW [] f = withGLFW "450" f
withGLFW version@(v : vs) f =
  bracket
    initGLFW
    (const GLFW.terminate)
    runCallback
  where
    initGLFW = do
      glv <- lookupEnv "MESA_GL_VERSION_OVERRIDE"
      case glv of
        Nothing -> setEnv "MESA_GL_VERSION_OVERRIDE" ([v] <> "." <> vs)
        _ -> return ()
      GLFW.init
    runCallback initialized =
      if initialized
        then f $ Init (T.pack version)
        else ioError (userError "GLFW init failed")

data Param = Param GL.GLint GL.VariableType
  deriving stock (Show)

type Params = M.Map T.Text Param

data ParamValue
  = ParamFloat Float
  | ParamFloat3 Float Float Float

data DefaultParams = DefaultParams
  { _iRes :: Maybe Param,
    _iTime :: Maybe Param,
    _range :: Maybe Param
  }

data Env = Env
  { _envRange :: IORef Float,
    _envPos :: IORef (Int, Int),
    _envSize :: IORef (Int, Int)
  }

data Window = Window
  { _glWindow :: GLFW.Window,
    _glProgram :: GL.Program,
    _defParams :: DefaultParams,
    _env :: Env,
    getParams :: Params
  }

instance Show Window where
  show _ = "Window[]"

getParam :: Window -> T.Text -> Maybe Param
getParam w n = M.lookup n (getParams w)

writeParam :: Param -> ParamValue -> IO ()
writeParam (Param pid GL.Float') (ParamFloat val) = GLR.glUniform1f pid val
writeParam (Param pid GL.FloatVec3) (ParamFloat3 v1 v2 v3) = GLR.glUniform3f pid v1 v2 v3
writeParam (Param pid GL.FloatVec2) (ParamFloat3 v1 v2 _) = GLR.glUniform2f pid v1 v2
writeParam (Param pid GL.Float') _ = error $ "Invalid param value: " <> show pid
writeParam (Param pid GL.FloatVec3) _ = error $ "Invalid param value: " <> show pid
writeParam (Param _ v) _ = error $ "Unknown param type: " <> show v

-- loadShader :: GL.ShaderType -> FilePath -> IO GL.Shader
-- loadShader st filePath = BS.readFile filePath >>= loadShaderBS st

readShader :: FilePath -> IO (BS.ByteString, Maybe String)
readShader fp =
  do
    fc <- BS.readFile fp
    return (fc, checkVersion $ T.lines $ decodeUtf8 fc)
  where
    checkVersion :: [T.Text] -> Maybe String
    checkVersion [] = Nothing
    checkVersion (x : xs)
      | T.isPrefixOf "#version " x = Just $ T.unpack (T.drop (T.length "#version ") x)
      | otherwise = checkVersion xs

loadShaderBS :: GL.ShaderType -> BS.ByteString -> IO GL.Shader
loadShaderBS st src =
  do
    shader <- GL.createShader st
    GL.shaderSourceBS shader $= src
    GL.compileShader shader
    ok <- GL.get (GL.compileStatus shader)
    unless ok $ do
      infoLog <- GL.get (GL.shaderInfoLog shader)
      putStrLn $ "Compilation failed:" <> show st
      putStrLn infoLog
      ioError (userError "shader compilation failed")
    return shader

linkShaderProgram :: GL.Program -> GL.Shader -> GL.Shader -> IO ()
linkShaderProgram prog vs fs =
  do
    GL.attachShader prog vs
    GL.attachShader prog fs
    GL.linkProgram prog
    ok <- GL.get (GL.linkStatus prog)
    infoLog <- GL.get (GL.programInfoLog prog)
    unless (null infoLog) $ do
      putStrLn "Link log:"
      putStrLn infoLog
    unless ok $ do
      GL.deleteObjectNames [prog]
      ioError (userError "GLSL linking failed")
    GL.validateProgram prog
    status <- GL.get $ GL.validateStatus prog
    unless status $ do
      plog <- GL.get $ GL.programInfoLog prog
      putStrLn plog
      ioError (userError "GLSL validation failed")

positions :: V.Vector Float
positions =
  V.fromList $
    concatMap
      (\(a, b) -> [a, b])
      [ (-1.0, -1.0),
        (-1.0, 1.0),
        (1.0, -1.0),
        (1.0, 1.0)
      ]

setPositions :: IO ()
setPositions = do
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  V.unsafeWith positions $ \ptr ->
    GL.vertexAttribPointer (GL.AttribLocation 0)
      $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 ptr)

setupShader :: T.Text -> BS.ByteString -> IO GL.Program
setupShader version shader =
  do
    prog <- GL.createProgram
    -- compile
    vert <- loadShaderBS GL.VertexShader (GL.packUtf8 vertSrc)
    frag <- loadShaderBS GL.FragmentShader shader
    -- attrs
    GL.attribLocation prog "position" $= GL.AttribLocation 0
    -- link
    linkShaderProgram prog vert frag
    GL.currentProgram $= Just prog
    return prog
  where
    vertSrc =
      unlines
        [ "#version " <> T.unpack version,
          if version == "450"
            then "in vec2 position;"
            else "attribute vec2 position;",
          "void main (void) {",
          "gl_Position = vec4(position, 0.0, 1.0);",
          "};"
        ]

mouseButtonCallback ::
  Window ->
  GLFW.Window ->
  GLFW.MouseButton ->
  GLFW.MouseButtonState ->
  GLFW.ModifierKeys ->
  IO ()
mouseButtonCallback w _ GLFW.MouseButton'1 GLFW.MouseButtonState'Pressed _ =
  do
    posValue <- readIORef (_envPos (_env w))
    sizeValue <- readIORef (_envSize (_env w))
    print $ "click: " <> show posValue <> " in " <> show sizeValue
mouseButtonCallback _ _ _ _ _ = return ()

cursorPosCallback :: Window -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback w _ x y = writeIORef posRef (double2Int x, double2Int y)
  where
    posRef = (_envPos (_env w))

scrollCallback :: Window -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback (Window _ _ (DefaultParams _ _ (Just range)) (Env rangeRef _ _) _) _ 0.0 direction =
  do
    rangeValue' <- readIORef rangeRef
    let rangeValue = rangeValue' - rangeValue' / 10.0 * double2Float direction
    writeIORef rangeRef rangeValue
    writeParam range (ParamFloat rangeValue)
    print $ "scroll: " <> show direction <> " -> " <> show rangeValue
scrollCallback _ _ _ _ = return ()

keyCallback :: Window -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback _ w k _ _ _
  | k `elem` [GLFW.Key'Q, GLFW.Key'Escape] = GLFW.setWindowShouldClose w True
keyCallback _ _ _ _ _ _ = return ()

windowSizeCallback :: Window -> GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback w _ x y =
  do
    writeIORef sizeRef (x, y)
    updateResolutions w x y
  where
    sizeRef = (_envSize (_env w))

withWindow :: Initialized -> Int -> Int -> BS.ByteString -> (Window -> IO ()) -> IO ()
withWindow (Init version) width height shader f =
  do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    bracket mkWindow closeWindow runCallback
  where
    mkWindow :: IO (Maybe GLFW.Window)
    mkWindow = GLFW.createWindow width height "Hadertoy" Nothing Nothing
    closeWindow :: Maybe GLFW.Window -> IO ()
    closeWindow _ = return ()
    runCallback :: Maybe GLFW.Window -> IO ()
    runCallback (Just win) = do
      GLFW.makeContextCurrent (Just win)
      prog <- setupShader version shader
      params <- getUniforms prog
      setPositions
      let defParams = DefaultParams (M.lookup "iResolution" params) (M.lookup "iTime" params) (M.lookup "range" params)
      let startRange = 2.0
      env <- Env <$> newIORef startRange <*> newIORef (0, 0) <*> newIORef (width, height)
      case _range defParams of
        Just p -> writeParam p (ParamFloat startRange)
        _ -> return ()
      let window = Window win prog defParams env params
      updateResolutions window width height
      GLFW.setKeyCallback win (Just $ keyCallback window)
      GLFW.setScrollCallback win (Just $ scrollCallback window)
      GLFW.setCursorPosCallback win (Just $ cursorPosCallback window)
      GLFW.setMouseButtonCallback win (Just $ mouseButtonCallback window)
      GLFW.setCursorPosCallback win (Just $ cursorPosCallback window)
      GLFW.setWindowSizeCallback win (Just $ windowSizeCallback window)
      f window
    runCallback Nothing = ioError (userError "Window creation failed")
    simpleErrorCallback :: GLFW.Error -> String -> IO ()
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]
    getUniforms :: GL.Program -> IO Params
    getUniforms prog = do
      uniforms <- GL.activeUniforms prog
      params <-
        mapM
          ( \(_, t, n) -> do
              GL.UniformLocation x <- GL.uniformLocation prog n
              return (T.pack n, Param x t)
          )
          uniforms
      return $ M.fromList params

contextCurrent :: Window -> IO ()
contextCurrent win = GLFW.makeContextCurrent $ Just (_glWindow win)

updateResolutions :: Window -> Int -> Int -> IO ()
updateResolutions w x y =
  do
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral x) (fromIntegral y))
    case (_iRes (_defParams w)) of
      Just v -> writeParam v (ParamFloat3 (fromIntegral x) (fromIntegral y) 0)
      _ -> return ()

render :: Window -> IO Bool
render win =
  do
    GLFW.pollEvents
    contextCurrent win
    let win' = _glWindow win
    let iTime = _iTime (_defParams win)
    GL.clearColor $= GL.Color4 0.9 0.1 0.1 1
    GL.clear [GL.ColorBuffer]
    -- update position
    setPositions
    -- update time
    case iTime of
      Just v -> do
        t <- getTime
        writeParam v (ParamFloat $ double2Float t)
      _ -> return ()
    -- draw call
    GL.drawArrays GL.TriangleStrip 0 4
    GLFW.swapBuffers win'
    GL.flush
    GLFW.windowShouldClose (_glWindow win)

getTime :: IO Double
getTime =
  do
    t <- GLFW.getTime
    case t of
      Just tv -> return tv
      Nothing -> print ("Error getting time?" :: T.Text) >> return 0

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
