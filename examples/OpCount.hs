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
import Spiral.OpCount
import Spiral.Program
import Spiral.RootOfUnity
import Spiral.SPL
import Spiral.SPL.Run
import Spiral.Util.Uniq

main :: IO ()
main = defaultMain $ \args -> do
    useComplexType <- asksConfig $ testDynFlag UseComplex
    n <- case args of
           [s] -> return (read s)
           _   -> return 4
    let f = formula n
    pprint f
    if useComplexType
      then toProgram "f" f >>= go
      else toProgram "f" (Re f) >>= go
  where
    go :: (Typed a, Num (Exp a)) => Program a -> Spiral ()
    go prog = do
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

-- The SPL formula for which we generate code and count operations.
formula :: Int -> SPL (Exp (Complex Double))
formula _ =
    Pi (L 8 4) ×
    (I 2 ⊗ (Pi (L 4 2) × (I 2 ⊗ F 2 w2) × diag [1.0, 1.0, 1.0, w4] × (F 2 w2 ⊗ I 2))) ×
    diag [1.0, 1.0, 1.0, 1.0, 1.0, w8, w4, w8^(3 :: Int)] ×
    (F 2 w2 ⊗ I 4)
  where
    w2, w4, w8 :: Exp (Complex Double)
    w2 = omega 2
    w4 = omega 4
    w8 = omega 8
