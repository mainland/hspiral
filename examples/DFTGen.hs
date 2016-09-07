{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Complex

import Spiral.Driver
import Spiral.Driver.Config
import Spiral.Exp
import Spiral.FFT
import Spiral.SPL

main :: IO ()
main = defaultMain $ \args -> do
    n <- case args of
           [s] -> return (read s)
           _   -> return 4
    let dft_n :: SPL (Exp (Complex Double))
        dft_n = f n
    useComplex <- asksConfig (testDynFlag UseComplex)
    if useComplex
      then codegenC ("dft_" ++ show n) dft_n
      else codegenC ("dft_" ++ show n) (Re dft_n)
