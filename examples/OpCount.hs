{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Complex (Complex)
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral
import Spiral.Config
import Spiral.Exp
import Spiral.FFT.CooleyTukey
import Spiral.OpCount
import Spiral.Program
import Spiral.SPL
import Spiral.SPL.Run

main :: IO ()
main = defaultMain $ \args -> do
    useComplexType <- asksConfig $ testDynFlag UseComplex
    n <- case args of
           [s] -> return (read s)
           _   -> return 4
    let dft_n :: SPL (Exp (Complex Double))
        dft_n = dit n
    if useComplexType
      then toProgram "f" dft_n >>= go
      else toProgram "f" (Re dft_n) >>= go
  where
    go :: Typed a => Program a -> Spiral ()
    go prog = do
      pprint prog
      ops <- countProgramOps prog
      liftIO $ putDocLn $
          text "Multiplications:" <+> ppr (mulOps ops) </>
          text "      Additions:" <+> ppr (addOps ops) </>
          text "          Total:" <+> ppr (allOps ops)
