{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Foldable (toList)
import Text.PrettyPrint.Mainland

import SPL.Backend.C (evalCg)
import SPL.Cg (cgSPL)
import SPL.FFT
import SPL.Monad

main :: IO ()
main = do
    defs <- runSPLM $ evalCg $ cgSPL "dft_4" (f 4)
    putDocLn $ ppr $ toList defs
