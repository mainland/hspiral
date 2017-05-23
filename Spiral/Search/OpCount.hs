{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Search.OpCount
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Search.OpCount (
    searchOpCount
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard,
                      msum)
import Data.List (minimumBy)
import Data.Typeable (Typeable)
import Math.NumberTheory.Primes.Testing (isPrime)
import Text.PrettyPrint.Mainland hiding ((<|>))
import Text.PrettyPrint.Mainland.Class

import Spiral.Config
import Spiral.Exp
import Spiral.FFT.CooleyTukey
import Spiral.FFT.GoodThomas (goodThomas)
import Spiral.FFT.Rader (rader)
import Spiral.Monad
import Spiral.Search.Monad
import Spiral.NumberTheory (primeFactorization)
import Spiral.OpCount
import Spiral.RootOfUnity
import Spiral.SPL
import Spiral.Util.Trace

-- | Search for the form of a transform with the best op-count.
searchOpCount :: (Typeable a, Typed a, Num (Exp a), MonadSpiral m)
              => SPL (Exp a)
              -> m (SPL (Exp a))
searchOpCount e = runS $ search e

search :: (Typeable a, Typed a, Num (Exp a), MonadSpiral m)
       => SPL (Exp a)
       -> S m (SPL (Exp a))
search e@(F n _) | n <= 2 =
   return e

search (F n w) = do
   maybe_e <- lookupDFT n w
   case maybe_e of
     Just (e, _) -> return e
     Nothing     -> bestBreakdown n w

search (Kron e1 e2) =
    Kron <$> search e1 <*> search e2

search (DSum e1 e2) =
    go <$> search e1 <*> search e2
  where
    go :: SPL a -> SPL a -> SPL a
    go (Diag xs) (Diag ys) = Diag (xs <> ys)
    go e1'       e2'       = DSum e1' e2'

search (Prod e1 e2) =
    Prod <$> search e1 <*> search e2

search e@E{}     = return e
search e@Diag{}  = return e
search e@KDiag{} = return e
search e@Circ{}  = return e
search e@Toep{}  = return e
search e@I{}     = return e
search e@Rot{}   = return e
search e@Pi{}    = return e
search e@F2{}    = return e
search (Re e)    = Re <$> search e
search (DFT n)   = search $ F n (omega n)
search (DFT' n)  = search $ F' n (omega n)
search (F' n w)  = search $ KDiag n (1/fromIntegral n) × F n (1/w)

-- | Find the best DFT breakdown.
bestBreakdown :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadSpiral m)
              => Int
              -> Exp a
              -> S m (SPL (Exp a))
bestBreakdown n w = do
    useComplexType <- asksConfig $ testDynFlag UseComplex
    alts           <- observeAll (breakdown n w) >>= mapM search
    opcs           <- mapM (countOps' useComplexType tau) alts
    traceSearch $ text "DFT size" <+> ppr n <> text ":" <+> commasep [ppr (mulOps ops) <> char '/' <> ppr (addOps ops) | ops <- opcs]
    let (e, m) = minimumBy metricOrdering (alts `zip` opcs)
    tryCacheDFT n w e m
  where
    tau :: Type a
    tau = typeOf (undefined :: a)

    -- If we aren't using native complex numbers, we need to count operations on
    -- the version of the transform that takes a size-2n vector as input.
    countOps' :: (Typed b, RootOfUnity (Exp b))
              => Bool
              -> Type b
              -> SPL (Exp b)
              -> S m (OpCount Int)
    countOps' False ComplexT{} = countOps . Re
    countOps' _     _          = countOps

-- | Generate DFT breakdowns
breakdown :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadSpiral m)
          => Int
          -> Exp a
          -> S m (SPL (Exp a))
breakdown n w =
    bruteForce <|>
    cooleyTukeyBreakdowns <|>
    goodThomasBreakdowns <|>
    raderBreakdowns
  where
    bruteForce :: S m (SPL (Exp a))
    bruteForce = return $ (spl . toMatrix) (F n w)

    cooleyTukeyBreakdowns :: S m (SPL (Exp a))
    cooleyTukeyBreakdowns =
        msum [return $ cooleyTukeyDIF r s w | (r, s) <- factors n] <|>
        msum [return $ cooleyTukeyDIT r s w | (r, s) <- factors n] <|>
        splitRadixBreakdown <|>
        splitRadix8Breakdown

    splitRadixBreakdown :: S m (SPL (Exp a))
    splitRadixBreakdown = do
        guard (n `rem` 4 == 0)
        return $ splitRadix n w

    splitRadix8Breakdown :: S m (SPL (Exp a))
    splitRadix8Breakdown = do
        guard (n `rem` 8 == 0)
        return $ splitRadix8 n w

    goodThomasBreakdowns :: S m (SPL (Exp a))
    goodThomasBreakdowns = msum [return $ goodThomas r s w | (r, s) <- coprimeFactors n]

    raderBreakdowns :: S m (SPL (Exp a))
    raderBreakdowns = do
        guard (isPrime (fromIntegral n))
        return $ rader n w

-- | Cache the given DFT transform if its metric improves on the previously
-- best-known DFT.
tryCacheDFT :: (Typeable a, Num (Exp a), MonadSpiral m)
            => Int
            -> Exp a
            -> SPL (Exp a)
            -> Metric
            -> S m (SPL (Exp a))
tryCacheDFT n w e m = do
    maybe_e' <- lookupDFT n w
    case maybe_e' of
      Just t@(e', _) | metricOrdering t (e, m) /= LT -> return e'
      _ -> do cacheDFT n w e m
              return e

metricOrdering :: (a, Metric) -> (a, Metric) -> Ordering
metricOrdering (_, x) (_, y) =
    case compare (allOps x) (allOps y) of
      EQ -> compare (mulOps x) (mulOps y)
      o  -> o

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
