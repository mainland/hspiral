{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Complex (Complex)
import Data.Foldable (toList)
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral
import Spiral.Backend.C
import Spiral.Config
import Spiral.Driver
import Spiral.Exp
import Spiral.Search.OpCount
import Spiral.OpCount
import Spiral.SPL
import Spiral.SPL.Run
import Spiral.Util.Uniq

main :: IO ()
main = defaultMain $ \args -> do
    useComplexType <- asksConfig $ testDynFlag UseComplex
    n <- case args of
           [s] -> return (read s)
           _   -> return 4
    if useComplexType
      then do e <- searchOpCount (DFT n :: SPL (Exp (Complex Double)))
              go n e
      else do e <- searchOpCount (Re (DFT n) :: SPL (Exp Double))
              go n e
  where
    go :: (Typed a, Num (Exp a)) => Int -> SPL (Exp a) -> Spiral ()
    go n e = do
        pprint e
        prog <- toProgram ("hspiral_dft_" ++ show n) e
        pprint prog
        ops <- countProgramOps prog
        resetUnique
        defs <- evalCg $ cgProgram prog
        outp <- asksConfig output
        case outp of
          Nothing -> return ()
          Just{}  -> writeOutput (toList defs)
        liftIO $ putDocLn $
            text "Multiplications:" <+> ppr (mulOps ops) </>
            text "      Additions:" <+> ppr (addOps ops) </>
            text "          Total:" <+> ppr (allOps ops)
