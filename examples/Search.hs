{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (MonadIO(..),
                               liftIO)
import Data.Complex (Complex)
import Data.List (minimumBy)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral
import Spiral.Exp
import Spiral.FFT
import Spiral.FFT.CooleyTukey
import Spiral.Monad
import Spiral.Search.Monad
import Spiral.NumberTheory (primeFactorization)
import Spiral.OpCount
import Spiral.SPL
import Spiral.SPL.Run
-- import Spiral.Util.Trace

main :: IO ()
main = defaultMain $ \args -> do
    n <- case args of
           [s] -> return (read s)
           _   -> return 4
    e    <- runS $ search (DFT n)
    pprint e
    prog <- toProgram "f" (Re e)
    pprint prog
    ops <- countProgramOps prog
    let adds = binopCount ops Add + binopCount ops Sub
        muls = binopCount ops Mul
    liftIO $ putDocLn $
        text "Multiplications:" <+> ppr muls </>
        text "      Additions:" <+> ppr adds </>
        text "          Total:" <+> ppr (adds + muls)

metricOrdering :: (a, Metric) -> (a, Metric) -> Ordering
metricOrdering (_, x) (_, y) =
    case compare (allOps x) (allOps y) of
      EQ -> compare (mulOps x) (mulOps y)
      o  -> o

binopCount :: OpCount Int -> Binop -> Int
binopCount opc op = fromMaybe 0 (Map.lookup op (binops opc))

allOps :: OpCount Int -> Int
allOps opc = addOps opc + mulOps opc

addOps :: OpCount Int -> Int
addOps opc = binopCount opc Add + binopCount opc Sub

mulOps :: OpCount Int -> Int
mulOps opc = binopCount opc Mul

search :: (MonadSpiral m, MonadIO m)
       => SPL (Exp (Complex Double))
       -> S m (SPL (Exp (Complex Double)))
search e@(RDFT n _) | n <= 2 =
   return e

search (RDFT n w) = do
   maybe_e <- lookupDFT n w
   case maybe_e of
     Just (e, _) -> return e
     Nothing     -> breakdown n w

search (Kron e1 e2) =
    Kron <$> search e1 <*> search e2

search (DSum e1 e2) =
    go <$> search e1 <*> search e2
  where
    go :: SPL (Exp (Complex Double))
       -> SPL (Exp (Complex Double))
       -> SPL (Exp (Complex Double))
    go (Diag xs) (Diag ys) = Diag (xs <> ys)
    go e1'       e2'       = DSum e1' e2'

search (Prod e1 e2) =
    Prod <$> search e1 <*> search e2

search e@E{}        = return e
search e@Diag{}     = return e
search e@KDiag{}    = return e
search e@Circ{}     = return e
search e@Toep{}     = return e
search e@I{}        = return e
search e@R{}        = return e
search e@Pi{}       = return e
search e@F2{}       = return e
search Re{}         = fail "Saw Re"
search (DFT n)      = search $ RDFT n (omega n)
search (IDFT n)     = search $ RIDFT n (omega n)
search (RIDFT n w)  = search $ KDiag n (1/fromIntegral n) × RDFT n (1/w)

breakdown :: forall m . (MonadSpiral m, MonadIO m)
          => Int
          -> Exp (Complex Double)
          -> S m (SPL (Exp (Complex Double)))
breakdown n w = do
    alts     <- mapM search [cooleyTukey r s w | (r, s) <- factors n]
    opcs     <- mapM (countOps . Re) alts
    liftIO $ putDocLn $ text "DFT size" <+> ppr n <> text ":" <+> ppr [(mulOps ops, addOps ops) | ops <- opcs]
    let (e, m) = minimumBy metricOrdering (alts `zip` opcs)
    tryCacheDFT n w e m

tryCacheDFT :: MonadSpiral m
            => Int
            -> Exp (Complex Double)
            -> SPL (Exp (Complex Double))
            -> Metric
            -> S m (SPL (Exp (Complex Double)))
tryCacheDFT n w e m = do
    maybe_e' <- lookupDFT n w
    case maybe_e' of
      Just t@(e', _) | metricOrdering t (e, m) /= LT -> return e'
      _ -> do cacheDFT n w e m
              return e

-- | Return all possible ways to factor a number into two factors, neither of
-- which is one.
factors :: Int -> [(Int,Int)]
factors n =
    filter (not . dumbFactors) $
    factorSplits $ primeFactorization n

-- | Return all possible ways to factor a number into two coprime factors,
-- neither of which is one.
coprimeFactors :: Int -> [(Int,Int)]
coprimeFactors n =
    filter (not . dumbFactors)
    [(unfactor fs1, unfactor fs2) | (fs1, fs2) <- splits $ primeFactorization n]

dumbFactors :: (Int,Int) -> Bool
dumbFactors (1,_) = True
dumbFactors (_,1) = True
dumbFactors _     = False

-- | Generate all possible splittings of a list, avoiding empty lists.
splits :: [a] -> [([a], [a])]
splits []     = []
splits [x]    = [([x], []), ([], [x])]
splits (x:xs) = [(x:ys, zs) | (ys, zs) <- ss] ++ [(ys, x:zs) | (ys, zs) <- ss]
  where
    ss = splits xs

-- | Given a prime factorization, return all possible ways to split the original
-- number into two factors.
factorSplits :: [(Int,Int)] -> [(Int,Int)]
factorSplits []         = [(1, 1)]
factorSplits ((p,n):fs) = [(p^i*x, p^(n-i)*y) | (x,y) <- factorSplits fs, i <- [0..n]]

-- | Convert a prime fatorization back into a number.
unfactor :: [(Int,Int)] -> Int
unfactor []         = 1
unfactor ((p,n):fs) = p^n*unfactor fs
