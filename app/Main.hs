-- | A demo hadertoy app
module Main (main) where

import Hadertoy

main :: IO ()
main =
  do
    win' <- window 800 800
    case win' of
      Just win -> run 20 (update win)
      Nothing  -> print "Oops"
  where
    update win = render win
