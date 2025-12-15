{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Data.Complex (Complex)
import Data.Foldable (toList)
import Data.Typeable (Typeable)
import System.Console.GetOpt
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral
import Spiral.Backend.C
import Spiral.Config
import Spiral.Driver
import Spiral.Exp
import Spiral.FFT.CooleyTukey
import Spiral.Monad
import Spiral.OpCount
import Spiral.Program
import Spiral.SPL
import Spiral.SPL.Run
import Spiral.Search
import Spiral.Search.FFTBreakdowns
import Spiral.Search.OpCountWHT
import Spiral.Util.Uniq

main :: IO ()
main = defaultMainWith' options mempty $ \fs args -> do
    n <- case args of
           [s] -> return (read s)
           _   -> return 4
    f <- formula fs n
    pprint f
    toProgram ("hspiral_wht_" ++ show n) f >>= go
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
formula :: MonadSpiral m => [Flag] -> Int -> m (SPL (Exp (Double)))
formula fs n =
  case fs of
    [Wht]         -> return $ wht n
    [WhtIter]     -> return $ wht_iter n
    -- [WhtSearch]   -> searchOpCountWHT (WHT n) -- Currently returns Prelude.foldl1: empty list
    [WhtSearch]   -> runSearchWHT () whtBreakdown (WHT n)
    _             -> fail "Must specify exactly one of --wht, --iterWht, or --searchWht"
  where
    whtBreakdown :: (Typeable a, Typed a, Floating (Exp a), MonadSpiral m)
                     => SPL (Exp a)
                     -> S s m (SPL (Exp a))
    whtBreakdown (WHT n) = whtBreakdowns n
    whtBreakdown _       = mzero

data Flag = Wht
          | WhtIter
          | WhtSearch
  deriving (Eq, Ord, Show)

options :: [OptDescr Flag]
options =
    [ Option [] ["wht"] (NoArg Wht)                             "Use WHT"
    , Option [] ["iterWht"] (NoArg WhtIter)                     "Use Iterative WHT"
    , Option [] ["searchWht"] (NoArg WhtSearch)                 "Search WHT breakdowns and pick lowest opcount"
    ]
