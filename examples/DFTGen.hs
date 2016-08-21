{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Foldable (toList)
import Text.PrettyPrint.Mainland

import Spiral
import Spiral.Backend.C (evalCg)
import Spiral.FFT

main :: IO ()
main = do
    defs <- runSpiral $ evalCg $ codegen "dft_4" (f 4)
    putDocLn $ ppr $ toList defs
