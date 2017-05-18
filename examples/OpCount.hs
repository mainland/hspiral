{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Complex (Complex)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral
import Spiral.Exp
import Spiral.FFT
import Spiral.OpCount
import Spiral.SPL
import Spiral.SPL.Run

main :: IO ()
main = defaultMain $ \args -> do
    n <- case args of
           [s] -> return (read s)
           _   -> return 4
    let dft_n :: SPL (Exp (Complex Double))
        dft_n = f n
    prog <- toProgram "f" (Re dft_n)
    pprint prog
    ops <- countProgramOps prog
    let adds = binopCount ops Add + binopCount ops Sub
        muls = binopCount ops Mul
    liftIO $ putDocLn $
        text "      Additions:" <+> ppr adds </>
        text "Multiplications:" <+> ppr muls</>
        text "          Total:" <+> ppr (adds + muls)
  where
    binopCount :: OpCount Int -> Binop -> Int
    binopCount opc op = fromMaybe 0 (Map.lookup op (binops opc))
