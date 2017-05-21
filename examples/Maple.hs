{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Complex
import Text.PrettyPrint.Mainland
import Control.Monad.IO.Class (liftIO)

import Spiral
import Spiral.Exp
import Spiral.FFT.CooleyTukey
import Spiral.SPL
import Spiral.Util.MaplePretty

main :: IO ()
main = defaultMain $ \args -> do
    n <- case args of
           [s] -> return (read s)
           _   -> return 4
    let dft_n :: SPL (Exp (Complex Double))
        dft_n = dit n
    --liftIO $ check dft_n
    liftIO $ putDocLn $ text "with(LinearAlgebra);"
    liftIO $ putDocLn $ text "A :=" <+> pprm (DFT n :: SPL (Exp (Complex Double))) <> semi
    liftIO $ putDocLn $ text "B :=" <+> pprm dft_n <> semi
