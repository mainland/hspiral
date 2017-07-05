{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Modular

import Spiral
import Spiral.Exp
import Spiral.FFT.CooleyTukey
import Spiral.NumberTheory
import Spiral.SPL

main :: IO ()
main = defaultMain $ \args -> do
    liftIO $ setGenerator 2013265921 31
    n <- case args of
           [s] -> return (read s)
           _   -> return 4
    let dft_n :: SPL (Exp (ℤ/2013265921))
        dft_n = dit n
    codegenC ("moddft_" ++ show p ++ "_" ++ show n) dft_n
  where
    p :: Integer
    p = 2013265921
