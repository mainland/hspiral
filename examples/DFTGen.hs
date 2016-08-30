{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Spiral.Driver
import Spiral.FFT

main :: IO ()
main = defaultMain $ \args -> do
    n <- case args of
           [s] -> return (read s)
           _   -> return 4
    codegenC ("dft_" ++ show n) (f n)
