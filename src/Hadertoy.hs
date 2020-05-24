{-# LANGUAGE NumericUnderscores #-}

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
    render,
    run,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import GHC.Float (double2Float)
import qualified Graphics.GL.Core31 as GLR
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

data Initialized = Init

withGLFW :: (Initialized -> IO ()) -> IO ()
withGLFW f =
  bracket
    GLFW.init
    (const GLFW.terminate)
    runCallback
  where
    runCallback initialized =
      if initialized
        then f Init
        else ioError (userError "GLFW init failed")

data Window = Window
  { _glWindow :: GLFW.Window,
    _glProgram :: GL.Program
  }

instance Show Window where
  show _ = "Window[]"

loadShader :: GL.ShaderType -> FilePath -> IO GL.Shader
loadShader st filePath = BS.readFile filePath >>= loadShaderBS st

loadShaderBS :: GL.ShaderType -> BS.ByteString -> IO GL.Shader
loadShaderBS st src =
  do
    shader <- GL.createShader st
    GL.shaderSourceBS shader $= src
    GL.compileShader shader
    ok <- GL.get (GL.compileStatus shader)
    unless ok $ do
      infoLog <- GL.get (GL.shaderInfoLog shader)
      putStrLn "Compilation failed:"
      putStrLn infoLog
      ioError (userError "shader compilation failed")
    return shader

linkShaderProgram :: GL.Program -> GL.Shader -> GL.Shader -> IO ()
linkShaderProgram prog vs fs =
  do
    GL.attachedShaders prog $= [vs, fs]
    GL.linkProgram prog
    ok <- GL.get (GL.linkStatus prog)
    infoLog <- GL.get (GL.programInfoLog prog)
    unless (null infoLog) $ do
      putStrLn "Link log:"
      putStrLn infoLog
    unless ok $ do
      GL.deleteObjectNames [prog]
      ioError (userError "GLSL linking failed")

setupShader :: FilePath -> IO GL.Program
setupShader shader =
  do
    prog <- GL.createProgram
    -- compile
    vert <- loadShaderBS GL.VertexShader (GL.packUtf8 vertSrc)
    frag <- loadShader GL.FragmentShader shader
    -- attrs
    GL.attribLocation prog "position" $= GL.AttribLocation 0
    -- link
    linkShaderProgram prog vert frag
    GL.currentProgram $= Just prog
    -- set positions
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    V.unsafeWith positions $ \ptr ->
      GL.vertexAttribPointer (GL.AttribLocation 0)
        $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 ptr)
    return prog
  where
    positions :: V.Vector Float
    positions =
      V.fromList
        [ -1.0,
          -1.0,
          -1.0,
          1.0,
          1.0,
          -1.0,
          1.0,
          1.0
        ]
    vertSrc =
      unlines
        [ "attribute vec2 position;",
          "void main (void) {",
          "gl_Position = vec4(position, 0.0, 1.0);",
          "};"
        ]

withWindow :: Initialized -> Int -> Int -> FilePath -> (Window -> IO ()) -> IO ()
withWindow _ width height shader f =
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
      prog <- setupShader shader
      f $ Window win prog
    runCallback Nothing = ioError (userError "Window creation failed")
    simpleErrorCallback :: GLFW.Error -> String -> IO ()
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

contextCurrent :: Window -> IO ()
contextCurrent win = GLFW.makeContextCurrent $ Just (_glWindow win)

render :: Window -> IO Bool
render win =
  do
    contextCurrent win
    let prog = _glProgram win
    let win' = _glWindow win
    GL.clearColor $= GL.Color4 0.9 0.1 0.1 1
    GL.clear [GL.ColorBuffer]
    -- update resolution
    (width, height) <- GLFW.getFramebufferSize win'
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
    GL.UniformLocation resLoc <- GL.get $ GL.uniformLocation prog "iResolution"
    GLR.glUniform3f resLoc (fromIntegral width) (fromIntegral height) 0
    -- update time
    t <- getTime
    GL.UniformLocation timeLoc <- GL.get $ GL.uniformLocation prog "iTime"
    GLR.glUniform1f timeLoc $ double2Float t
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
