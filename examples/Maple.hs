{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Complex
import Data.Monoid ((<>))
import Text.PrettyPrint.Mainland
import Control.Monad.IO.Class (liftIO)

import Spiral
import Spiral.Exp
import Spiral.FFT.CooleyTukey
import Spiral.SPL
import Spiral.Util.Pretty.Maple

main :: IO ()
main = defaultMain $ \args -> do
    n <- case args of
           [s] -> return (read s)
           _   -> return 4
    let dft_n :: SPL (Exp (Complex Double))
        dft_n = dit n
    --liftIO $ check dft_n
    liftIO $ putDocLn $ text "with(LinearAlgebra);"
    liftIO $ putDocLn $ text "A :=" <+> ppr (DFT n :: SPL (Exp (Complex Double))) <> semi
    liftIO $ putDocLn $ text "B :=" <+> ppr dft_n <> semi
