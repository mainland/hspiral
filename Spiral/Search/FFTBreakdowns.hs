{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Search.FFTBreakdowns
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Search.FFTBreakdowns (
    bruteForce,
    cooleyTukeyBreakdowns,
    splitRadixBreakdown,
    splitRadix8Breakdown,
    conjPairSplitRadixBreakdown,
    goodThomasBreakdowns,
    raderBreakdowns
  ) where

import Control.Applicative ((<|>))
import Control.Monad (MonadPlus,
                      guard,
                      msum)
import Data.Typeable (Typeable)
import Math.NumberTheory.Primes.Testing (isPrime)

import Spiral.Exp
import Spiral.FFT.CooleyTukey
import Spiral.FFT.GoodThomas (goodThomas)
import Spiral.FFT.Rader (rader)
import Spiral.NumberTheory (primeFactorization)
import Spiral.RootOfUnity
import Spiral.SPL

bruteForce :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadPlus m)
           => Int
           -> Exp a
           -> m (SPL (Exp a))
bruteForce n w = return $ (spl . toMatrix) (F n w)

cooleyTukeyBreakdowns :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadPlus m)
                      => Int
                      -> Exp a
                      -> m (SPL (Exp a))
cooleyTukeyBreakdowns n w =
    msum [return $ cooleyTukeyDIF r s w | (r, s) <- factors n] <|>
    msum [return $ cooleyTukeyDIT r s w | (r, s) <- factors n]

splitRadixBreakdown :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadPlus m)
                    => Int
                    -> Exp a
                    -> m (SPL (Exp a))
splitRadixBreakdown n w = do
    guard (n `rem` 4 == 0)
    return $ splitRadix n w

splitRadix8Breakdown :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadPlus m)
                     => Int
                     -> Exp a
                     -> m (SPL (Exp a))
splitRadix8Breakdown n w = do
    guard (n `rem` 8 == 0)
    return $ splitRadix8 n w

conjPairSplitRadixBreakdown :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadPlus m)
                            => Int
                            -> Exp a
                            -> m (SPL (Exp a))
conjPairSplitRadixBreakdown n w = do
    guard (n `rem` 4 == 0)
    return $ conjPairSplitRadix n w

goodThomasBreakdowns :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadPlus m)
                     => Int
                     -> Exp a
                     -> m (SPL (Exp a))
goodThomasBreakdowns n w =
    msum [return $ goodThomas r s w | (r, s) <- coprimeFactors n]

raderBreakdowns :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadPlus m)
                => Int
                -> Exp a
                -> m (SPL (Exp a))
raderBreakdowns n w = do
    guard (isPrime (fromIntegral n))
    return $ rader n w

-- | Return all possible ways to factor a number into two factors, neither of
-- which is one.
factors :: Int -> [(Int,Int)]
factors n =
    filter (not . dumbFactor) $
    factorSplits $ primeFactorization n

-- | Return all possible ways to factor a number into two coprime factors,
-- neither of which is one.
coprimeFactors :: Int -> [(Int,Int)]
coprimeFactors n =
    filter (not . dumbFactor)
    [(unfactor fs1, unfactor fs2) | (fs1, fs2) <- splits $ primeFactorization n]

-- | A prime factor we don't want.
dumbFactor :: (Int,Int) -> Bool
dumbFactor (1,_) = True
dumbFactor (_,1) = True
dumbFactor _     = False

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
