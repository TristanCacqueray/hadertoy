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
    GLSLVersion (..),
    withGLFW,
    Window (..),
    Param (..),
    ParamValue (..),
    withWindow,
    readGLSLVersion,
    isPaused,
    setEnvCenter,
    setEnvSeed,
    setEnvHookClick,
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Vector.Storable as V
import GHC.Float (double2Float, double2Int)
import qualified Graphics.GL.Core31 as GLR
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import System.Environment (lookupEnv, setEnv)

-- | GlobalEnv is available to every window
data Status
  = Paused
  | Running
  deriving stock (Eq, Show)

toggleStatus :: Status -> Status
toggleStatus Paused = Running
toggleStatus Running = Paused

data GlobalEnv = GlobalEnv
  { _envStatus :: IORef Status,
    _envSeed :: IORef (Float, Float)
  }

-- | GLSLVersion data type
data GLSLVersion
  = V120
  | V300ES
  | V450
  deriving stock (Eq, Ord, Show)

mesaOverride :: GLSLVersion -> String
mesaOverride V450 = "4.50"
mesaOverride V300ES = "3.00"
mesaOverride V120 = "1.20"

showVersion :: GLSLVersion -> Text
showVersion V450 = "450"
showVersion V300ES = "300 es"
showVersion V120 = "120"

readGLSLVersion :: Text -> Maybe GLSLVersion
readGLSLVersion "450" = Just V450
readGLSLVersion "120" = Just V120
readGLSLVersion "300 es" = Just V300ES
readGLSLVersion _ = Nothing

-- | Init indicates glfw is ready, the shader version Text and the GlobalEnv record.
data Initialized = Init GLSLVersion GlobalEnv

isPaused :: Initialized -> IO Bool
isPaused (Init _ env) = (== Paused) <$> readIORef (_envStatus env)

withGLFW :: GLSLVersion -> (Initialized -> IO ()) -> IO ()
withGLFW version f =
  bracket
    (fixMesaEnv >> GLFW.init)
    (const GLFW.terminate)
    runCallback
  where
    runCallback True = do
      glEnv <- GlobalEnv <$> newIORef Running <*> newIORef (0.0, 0.0)
      f $ Init version glEnv
    runCallback False = ioError (userError "GLFW init failed")

    fixMesaEnv = do
      glv <- lookupEnv "MESA_GL_VERSION_OVERRIDE"
      case glv of
        Nothing -> setEnv "MESA_GL_VERSION_OVERRIDE" (mesaOverride version)
        _ -> return ()

-- | Param describe an Uniform variable
data Param = Param GL.GLint GL.VariableType
  deriving stock (Show)

type Params = Map Text Param

-- | ParamValue describes an Uniform variable value
data ParamValue
  = ParamFloat Float
  | ParamFloat2 Float Float
  | ParamFloat3 Float Float Float

data DefaultParams = DefaultParams
  { _iRes :: Maybe Param,
    _iTime :: Maybe Param,
    _range :: Maybe Param,
    _center :: Maybe Param,
    _seed :: Maybe Param
  }

data Env = Env
  { _envRange :: IORef Float,
    _envCenter :: IORef (Float, Float),
    _envPos :: IORef (Int, Int),
    _envSize :: IORef (Int, Int),
    _envHookClick :: IORef (Maybe ((Float, Float) -> IO ()))
  }

data Window = Window
  { _glEnv :: GlobalEnv,
    _glWindow :: GLFW.Window,
    _glProgram :: GL.Program,
    _defParams :: DefaultParams,
    _env :: Env,
    _params :: Params
  }

setEnvSeed :: Window -> (Float, Float) -> IO Bool
setEnvSeed w newSeed = do
  writeIORef (_envSeed (_glEnv w)) newSeed
  case _seed (_defParams w) of
    Just p -> do
      contextCurrent w
      writeParam p (uncurry ParamFloat2 newSeed)
      return True
    Nothing -> return False

setEnvCenter :: Window -> (Float, Float) -> IO Bool
setEnvCenter w newCenter = do
  writeIORef (_envCenter (_env w)) newCenter
  case _center (_defParams w) of
    Just p -> do
      contextCurrent w
      writeParam p (uncurry ParamFloat2 newCenter)
      return True
    Nothing -> return False

setEnvHookClick :: Window -> ((Float, Float) -> IO ()) -> IO ()
setEnvHookClick w cb = writeIORef (_envHookClick (_env w)) (Just cb)

instance Show Window where
  show _ = "Window[]"

getParam :: Window -> Text -> Maybe Param
getParam w n = M.lookup n (_params w)

writeParam :: Param -> ParamValue -> IO ()
writeParam (Param pid GL.Float') (ParamFloat val) = GLR.glUniform1f pid val
writeParam (Param pid GL.FloatVec3) (ParamFloat3 v1 v2 v3) = GLR.glUniform3f pid v1 v2 v3
writeParam (Param pid GL.FloatVec2) (ParamFloat3 v1 v2 _) = GLR.glUniform2f pid v1 v2
writeParam (Param pid GL.FloatVec2) (ParamFloat2 v1 v2) = GLR.glUniform2f pid v1 v2
writeParam (Param pid GL.Float') _ = error $ "Invalid param value: " <> show pid
writeParam (Param pid GL.FloatVec3) _ = error $ "Invalid param value: " <> show pid
writeParam (Param _ v) _ = error $ "Unknown param type: " <> show v

-- loadShader :: GL.ShaderType -> FilePath -> IO GL.Shader
-- loadShader st filePath = BS.readFile filePath >>= loadShaderBS st

readShader :: FilePath -> IO (Text, Maybe GLSLVersion)
readShader fp =
  do
    fc <- BS.readFile fp
    let shader = T.lines $ decodeUtf8 fc
    return (T.unlines $ dropVersion shader, checkVersion shader)
  where
    dropVersion :: [Text] -> [Text]
    dropVersion [] = []
    dropVersion (x : xs)
      | T.isPrefixOf "#version" x = dropVersion xs
      | otherwise = x : dropVersion xs
    checkVersion :: [Text] -> Maybe GLSLVersion
    checkVersion [] = Nothing
    checkVersion (x : xs)
      | T.isPrefixOf "#version " x = readGLSLVersion (T.drop (T.length "#version ") x)
      | otherwise = checkVersion xs

loadShaderBS :: GL.ShaderType -> BS.ByteString -> IO (Maybe GL.Shader)
loadShaderBS st src =
  do
    shader <- GL.createShader st
    GL.shaderSourceBS shader $= src
    GL.compileShader shader
    ok <- GL.get (GL.compileStatus shader)
    if ok
      then return $ Just shader
      else do
        infoLog <- GL.get (GL.shaderInfoLog shader)
        putStrLn $ "Compilation failed: " <> show st
        putStrLn infoLog
        return Nothing

data ShaderStatus
  = Loaded GL.Program
  | Failed [String]

isLoaded :: ShaderStatus -> Bool
isLoaded (Loaded _) = True
isLoaded (Failed _) = False

linkShaderProgram :: GL.Program -> GL.Shader -> GL.Shader -> IO ShaderStatus
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
    case ok of
      True -> validate
      False -> return $ Failed ["GLSL linking failed"]
  where
    validate = do
      GL.validateProgram prog
      status <- GL.get $ GL.validateStatus prog
      case status of
        True -> return $ Loaded prog
        False -> do
          plog <- GL.get $ GL.programInfoLog prog
          putStrLn plog
          return $ Failed ["GLSL validation failed"]

positions :: V.Vector Float
positions =
  V.fromList $ concatMap (\(a, b) -> [a, b]) squarePos
  where
    squarePos :: [(Float, Float)]
    squarePos =
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

setupShader :: GLSLVersion -> Text -> IO ShaderStatus
setupShader version shader =
  do
    prog <- GL.createProgram
    let clean' = clean prog
    -- compile
    vert' <- loadShaderBS GL.VertexShader (GL.packUtf8 vertSrc)
    case vert' of
      Nothing -> clean' ["vert failed"]
      Just vert -> do
        frag' <- loadShaderBS GL.FragmentShader (encodeUtf8 (versionStr <> shader))
        case frag' of
          Nothing -> clean' ["frag failed"]
          Just frag -> do
            -- attrs
            GL.attribLocation prog "position" $= GL.AttribLocation 0
            -- link
            status <- linkShaderProgram prog vert frag
            if isLoaded status
              then GL.currentProgram $= Just prog
              else GL.deleteObjectNames [prog]
            return status
  where
    clean prog msg = do
      GL.deleteObjectNames [prog]
      return $ Failed msg
    versionStr = "#version " <> showVersion version <> "\n"
    vertSrc =
      unlines
        [ T.unpack versionStr,
          if version >= V300ES
            then "in vec2 position;"
            else "attribute vec2 position;",
          "void main (void) {",
          "gl_Position = vec4(position, 0.0, 1.0);",
          "}"
        ]

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

-- | Convert windows pos to plane pos
-- >>> normalizeCoord (400, 200) (800, 800)
-- (0.0,0.5)
normalizeCoord :: (Int, Int) -> (Int, Int) -> (Float, Float)
normalizeCoord (x, y) (winX, winY) = mapTuple (* ratio) (toPlane x winX, (-1) * toPlane y winY)
  where
    toPlane :: Int -> Int -> Float
    toPlane pos win = 2.0 * fromIntegral pos / fromIntegral win - 1
    ratio :: Float
    ratio = fromIntegral winX / fromIntegral winY

addCoord :: Num a => (a, a) -> (a, a) -> (a, a)
addCoord (x, y) (x', y') = (x + x', y + y')

scaleCoord :: Num a => a -> (a, a) -> (a, a)
scaleCoord f = mapTuple (* f)

mouseButtonCallback ::
  Window ->
  GLFW.Window ->
  GLFW.MouseButton ->
  GLFW.MouseButtonState ->
  GLFW.ModifierKeys ->
  IO ()
mouseButtonCallback w _ btn GLFW.MouseButtonState'Pressed _ =
  do
    posValue <- readIORef (_envPos (_env w))
    sizeValue <- readIORef (_envSize (_env w))
    centerValue <- readIORef (_envCenter (_env w))
    rangeValue <- readIORef (_envRange (_env w))
    let coord = normalizeCoord posValue sizeValue
    let newCenter = addCoord centerValue (scaleCoord rangeValue coord)
    case btn of
      GLFW.MouseButton'1 -> updatePos w coord newCenter
      GLFW.MouseButton'3 -> do
        hookCallback <- readIORef (_envHookClick (_env w))
        case hookCallback of
          Just f -> f newCenter
          _ -> return ()
      _ -> return ()
mouseButtonCallback _ _ _ _ _ = return ()

updatePos :: Window -> (Float, Float) -> (Float, Float) -> IO ()
updatePos w coord newCenter =
  do
    update <- setEnvCenter w newCenter
    if update
      then print $ "click: " <> show coord <> " -> " <> show newCenter
      else print $ "click: " <> show coord

cursorPosCallback :: Window -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback w _ x y = writeIORef posRef (double2Int x, double2Int y)
  where
    posRef = _envPos (_env w)

scrollCallback :: Window -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback win@(Window _ _ _ (DefaultParams _ _ (Just range) _ _) (Env rangeRef _ _ _ _) _) _ 0.0 direction =
  do
    rangeValue' <- readIORef rangeRef
    let rangeValue = rangeValue' - rangeValue' / 10.0 * double2Float direction
    writeIORef rangeRef rangeValue
    contextCurrent win
    writeParam range (ParamFloat rangeValue)
    print $ "scroll: " <> show direction <> " -> " <> show rangeValue
scrollCallback _ _ _ _ = return ()

keyCallback :: Window -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback _ w k _ _ _
  | k `elem` stopKeys = GLFW.setWindowShouldClose w True
keyCallback w _ GLFW.Key'Space _ GLFW.KeyState'Pressed _ = togglePause (_envStatus (_glEnv w))
  where
    togglePause pauseRef = do
      pauseValue <- readIORef pauseRef
      writeIORef pauseRef (toggleStatus pauseValue)
      print $ "key: " <> show pauseValue
keyCallback _ _ _ _ _ _ = return ()

stopKeys :: [GLFW.Key]
stopKeys = [GLFW.Key'Q, GLFW.Key'Escape]

windowSizeCallback :: Window -> GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback w _ x y =
  do
    writeIORef sizeRef (x, y)
    updateResolutions w x y
  where
    sizeRef = _envSize (_env w)

withWindow :: Initialized -> Int -> Int -> Text -> (Window -> IO ()) -> IO ()
withWindow (Init version glEnv) width height shader f =
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
      (Loaded prog) <- setupShader version shader
      params <- getUniforms prog
      setPositions
      let defParams =
            DefaultParams
              (M.lookup "iResolution" params)
              (M.lookup "iTime" params)
              (M.lookup "range" params)
              (M.lookup "center" params)
              (M.lookup "seed" params)
      let startRange = 2.0
      env <-
        Env <$> newIORef startRange
          <*> newIORef (0.0, 0.0)
          <*> newIORef (0, 0)
          <*> newIORef (width, height)
          <*> newIORef Nothing
      case _range defParams of
        Just p -> writeParam p (ParamFloat startRange)
        _ -> return ()
      let window = Window glEnv win prog defParams env params
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
    contextCurrent w
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral x) (fromIntegral y))
    case _iRes (_defParams w) of
      Just v -> writeParam v (ParamFloat3 (fromIntegral x) (fromIntegral y) 0)
      _ -> return ()

render :: Window -> IO Bool
render win =
  do
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
      Nothing -> print ("Error getting time?" :: Text) >> return 0

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
        GLFW.pollEvents
        run' (frame + 1) new_tick
    freq :: Double
    freq = 1 / fromIntegral fps
    toMicro :: Double -> Int
    toMicro x = round $ x * 1_000_000
