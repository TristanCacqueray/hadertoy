{- |
Copyright: (c) 2020 Tristan de Cacqueray
SPDX-License-Identifier: Apache-2.0
Maintainer: Tristan de Cacqueray <tristanC@wombatt.eu>

Glumpy in Haskell
-}

module Hadertoy
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
