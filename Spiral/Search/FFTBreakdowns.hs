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
    improvedSplitRadixBreakdown,
    goodThomasBreakdowns,
    raderBreakdowns
  ) where

import Control.Applicative ((<|>))
import Control.Monad (MonadPlus,
                      guard,
                      msum)
import Math.NumberTheory.Primes.Testing (isPrime)

import Spiral.Convolution
import Spiral.Exp
import Spiral.FFT.CooleyTukey
import Spiral.FFT.GoodThomas (goodThomas)
import Spiral.FFT.Rader (rader, raderI, raderII, raderIII, raderIV, raderLuIII)
import Spiral.NumberTheory (coprimeFactors,
                            factors)
import Spiral.RootOfUnity
import Spiral.SPL hiding ((<|>))

bruteForce :: (RootOfUnity (Exp a), MonadPlus m)
           => Int
           -> Exp a
           -> m (SPL (Exp a))
bruteForce n w = return $ (matrix . toMatrix) (F n w)

cooleyTukeyBreakdowns :: (RootOfUnity (Exp a), MonadPlus m)
                      => Int
                      -> Exp a
                      -> m (SPL (Exp a))
cooleyTukeyBreakdowns n w =
    msum [return $ cooleyTukeyDIF r s w | (r, s) <- fs] <|>
    msum [return $ cooleyTukeyDIT r s w | (r, s) <- fs]
  where
    fs :: [(Int, Int)]
    fs = factors n

splitRadixBreakdown :: (RootOfUnity (Exp a), MonadPlus m)
                    => Int
                    -> Exp a
                    -> m (SPL (Exp a))
splitRadixBreakdown n w = do
    guard (n `rem` 4 == 0)
    return $ splitRadix n w

splitRadix8Breakdown :: (RootOfUnity (Exp a), MonadPlus m)
                     => Int
                     -> Exp a
                     -> m (SPL (Exp a))
splitRadix8Breakdown n w = do
    guard (n `rem` 8 == 0)
    return $ splitRadix8 n w

conjPairSplitRadixBreakdown :: (RootOfUnity (Exp a), MonadPlus m)
                            => Int
                            -> Exp a
                            -> m (SPL (Exp a))
conjPairSplitRadixBreakdown n w = do
    guard (n `rem` 4 == 0)
    return $ conjPairSplitRadix n w

improvedSplitRadixBreakdown :: forall a m . (Typed a, RootOfUnity (Exp a), MonadPlus m)
                            => Int
                            -> Exp a
                            -> m (SPL (Exp a))
improvedSplitRadixBreakdown n w = do
    guard (n `rem` 4 == 0)
    case tau of
      ComplexT{} -> return $ improvedSplitRadix n w
      _          -> return $ splitRadix n w
  where
    tau :: Type a
    tau = typeOf (undefined :: a)

goodThomasBreakdowns :: (RootOfUnity (Exp a), MonadPlus m)
                     => Int
                     -> Exp a
                     -> m (SPL (Exp a))
goodThomasBreakdowns n w =
    msum [return $ goodThomas r s w | (r, s) <- coprimeFactors n]

raderBreakdowns :: forall a m . (RootOfUnity (Exp a), MonadPlus m)
                => Int
                -> Exp a
                -> m (SPL (Exp a))
raderBreakdowns n w = do
    guard (isPrime (fromIntegral n) && n > 2)
    msum $ map return $ [rader n w, raderII n w, raderIII n w, raderIV n w, raderLuIII n w]
      ++
      [raderI n w cyc | cyc <- getCycs (n-1)]
  where
    getCycs :: Int -> [CyclicConvolution (Exp a)]
    getCycs x = [ConvolutionTheorem x]
