{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

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
import Spiral.RootOfUnity
import Spiral.SPL
import Spiral.SPL.Run
import Spiral.Search.Monad
import Spiral.Search.Generic
import Spiral.Util.Uniq

main :: IO ()
main = defaultMainWith' options mempty $ \fs args -> do
    useComplexType <- asksConfig $ testDynFlag UseComplex
    n <- case args of
           [s] -> return (read s)
           _   -> return 4
    f <- formula fs n
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
formula :: MonadSpiral m => [Flag] -> Int -> m (SPL (Exp (Complex Double)))
formula fs n =
  case fs of
    [Dif]        -> return $ dif n
    [Dit]        -> return $ dit n
    [SplitRadix] -> runSearch f (DFT n)
    _            -> fail "Must specify exactly on of --dif, --dit, or --splitradix"
  where
    f :: (Typeable a, Typed a, RootOfUnity (Exp a), MonadSpiral m)
      => Int
      -> Exp a
      -> S m (SPL (Exp a))
    f n w = search f (splitRadix n w)

data Flag = Dif
          | Dit
          | SplitRadix
  deriving (Eq, Ord, Show)

options :: [OptDescr Flag]
options =
    [ Option [] ["dif"] (NoArg Dif)                "Use DIF"
    , Option [] ["dit"] (NoArg Dit)                "Use DIT"
    , Option [] ["split-radix"] (NoArg SplitRadix) "Use split radix"
    ]
