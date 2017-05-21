{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.IO.Class (MonadIO(..),
                               liftIO)
import Data.Complex (Complex)
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral
import Spiral.Config
import Spiral.Exp
import Spiral.Search.OpCount
import Spiral.OpCount
import Spiral.SPL
import Spiral.SPL.Run

main :: IO ()
main = defaultMain $ \args -> do
    useComplexType <- asksConfig $ testDynFlag UseComplex
    n <- case args of
           [s] -> return (read s)
           _   -> return 4
    if useComplexType
      then do e <- searchOpCount (DFT n :: SPL (Exp (Complex Double)))
              go e
      else do e <- searchOpCount (Re (DFT n) :: SPL (Exp Double))
              go e
  where
    go :: (Typed a, Num (Exp a)) => SPL (Exp a) -> Spiral ()
    go e = do
        pprint e
        prog <- toProgram "f" e
        pprint prog
        ops <- countProgramOps prog
        liftIO $ putDocLn $
            text "Multiplications:" <+> ppr (mulOps ops) </>
            text "      Additions:" <+> ppr (addOps ops) </>
            text "          Total:" <+> ppr (allOps ops)