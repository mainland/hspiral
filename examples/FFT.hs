{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Complex
import Text.PrettyPrint.Mainland

import SPL.Exp
import SPL.FFT
import SPL.Syntax

main :: IO ()
main = do
    putDocLn (pprMatrix (w 2 2 :: SPL (Exp (Complex Double))))
    putDocLn (pprMatrix (t 4 2 :: SPL (Exp (Complex Double))))
    putDocLn (ppr (f 4 :: SPL (Exp (Complex Double))))
    putDocLn (pprMatrix (f 4 :: SPL (Exp (Complex Double))))
