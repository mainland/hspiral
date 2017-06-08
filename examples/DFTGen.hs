{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Spiral.FFT.CooleyTukey
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
formula n = flattenDFT splitRadix (DFT n)

flattenDFT :: (Typed a, Num (Exp a))
           => (forall a . RootOfUnity a => Int -> a -> SPL a)
           -> SPL (Exp a)
           -> SPL (Exp a)
flattenDFT f e = go e
  where
    go :: (Typed a, Num (Exp a))
       => SPL (Exp a)
       -> SPL (Exp a)
    go e@(F n w)
      | n <= 2    = e
      | otherwise = go $ f n w

    go (Kron e1 e2) =
        Kron (go e1) (go e2)

    go (DSum e1 e2) =
        DSum (go e1) (go e2)

    go (Prod e1 e2) =
        Prod (go e1) (go e2)

    go e@E{}     = e
    go e@Diag{}  = e
    go e@KDiag{} = e
    go e@Circ{}  = e
    go e@Toep{}  = e
    go e@I{}     = e
    go e@Rot{}   = e
    go e@Pi{}    = e
    go e@F2{}    = e
    go (Re e)    = Re (go e)
    go (DFT n)   = go $ F n (omega n)
    go (DFT' n)  = go $ F' n (omega n)
    go (F' n w)  = go $ KDiag n (1/fromIntegral n) × F n (1/w)
