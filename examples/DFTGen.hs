{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Spiral.Driver
import Spiral.FFT

main :: IO ()
main = defaultMain $ codegenC "dft_4" (f 4)
