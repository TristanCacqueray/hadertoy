-- | A demo hadertoy app
module Main (main) where

import Data.Maybe
import System.Environment (getArgs)
import Hadertoy

main :: IO ()
main =
  do
    args <- getArgs
    let fn = fromMaybe "./shaders/demo.glsl" (listToMaybe args)
    win' <- window 800 800 fn
    case win' of
      Just win -> run fps (update win)
      Nothing  -> print "Oops"
  where
    fps = 25
    update win = render win
