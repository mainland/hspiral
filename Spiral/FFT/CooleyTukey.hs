{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- |
-- Module      :  Spiral.FFT.CooleyTukey
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.FFT.CooleyTukey (
    cooleyTukeyDIT,
    cooleyTukeyDIF,
    splitRadix,
    splitRadix8,
    conjPairSplitRadix,
    improvedSplitRadix,
    dit,
    dif
  ) where

import Data.List (foldr1)

import Spiral.Array
import Spiral.RootOfUnity
import Spiral.SPL

-- For exponent in 'splitRadix'
default (Int)

-- | The $W_n(\omega)$ matrix
ws :: RootOfUnity a => Int -> a -> SPL a
ws n w = diag [w^i | i <- [0..n-1]]

-- | Twiddle factor matrix $T^{rs}_s(\omega)$
twid :: RootOfUnity a => Int -> Int -> a -> SPL a
twid rs s w = foldr1 (⊕) [ws s (w^i) | i <- [0..r-1]]
  where
    r = rs `quot` s

-- | Cooley-Tukey decimation in time DFT decomposition.
cooleyTukeyDIT :: RootOfUnity a => Int -> Int -> a -> SPL a
cooleyTukeyDIT r s w =
    (F r (w^s) ⊗ I s) × twid (r*s) s w × (I r ⊗ F s (w^r)) × Pi (L (r*s) r)

-- | Cooley-Tukey decimation in frequency DFT decomposition.
cooleyTukeyDIF :: RootOfUnity a => Int -> Int -> a -> SPL a
cooleyTukeyDIF r s w =
    Pi (L (r*s) s) × (I r ⊗ F s (w^r)) × twid (r*s) s w × (F r (w^s) ⊗ I s)

-- | Split-radix DFT decomposition.
splitRadix :: forall a . RootOfUnity a => Int -> a -> SPL a
splitRadix n _ | n `mod` 4 /= 0 =
    error "Cannot call splitRadix when n is not divisible by 4"

splitRadix n w =
    (sigma4 ⊗ I p) × (I (2*p) ⊕ ws p w ⊕ ws p (w^3)) × (F (2*p) (w^2) ⊕ F p (w^4) ⊕ F p (w^4)) × (I (2*p) ⊕ Pi (L (2*p) 2)) × Pi (L (4*p) 2)
  where
    p :: Int
    p = n `quot` 4

    sigma4 :: SPL a
    sigma4 = (F2 ⊗ I 2) × (I 2 ⊕ (ws 2 w4 × F2))

    w4 :: a
    w4 = omega 4

-- | Split-radix DFT decomposition for eighth roots of unity.
splitRadix8 :: forall a . RootOfUnity a => Int -> a -> SPL a
splitRadix8 n _ | n `mod` 8 /= 0 =
    error "Cannot call splitRadix8 when n not divisible by 8"

splitRadix8 n w =
    (sigma8 ⊗ I p) ×
    (I (4*p) ⊕ ws p w ⊕ ws p w ⊕ ws p (w^3) ⊕ ws p (w^7)) ×
    (F (4*p) (w^2) ⊕ F (2*p) (w^4) ⊕ F p (w^8) ⊕ F p (w^8)) ×
    (I (6*p) ⊕ Pi (L (2*p) 2)) ×
    (I (4*p) ⊕ Pi (L (4*p) 2)) ×
    Pi (L (8*p) 2)
  where
    p :: Int
    p = n `quot` 8

    sigma8 :: SPL a
    sigma8 = (F2 ⊗ I 4) × (I 4 ⊕ (ws 4 w8 × (F2 ⊗ I 2))) × (I 6 ⊕ (ws 2 w4 × F2))

    w4, w8 :: a
    w4 = omega 4
    w8 = omega 8

-- | Conjugate-pair split-radix DFT decomposition.
conjPairSplitRadix :: forall a . RootOfUnity a => Int -> a -> SPL a
conjPairSplitRadix n _ | n `mod` 4 /= 0 =
    error "Cannot call splitRadix when n is not divisible by 4"

conjPairSplitRadix n w =
    (sigma4 ⊗ I p) ×
    (I (2*p) ⊕ ws p w ⊕ ws p (w^^(-1))) ×
    (F (2*p) (w^2) ⊕ F p (w^4) ⊕ F p (w^4)×Pi (CS p 1)) ×
    (I (2*p) ⊕ Pi (L (2*p) 2)) ×
    Pi (L (4*p) 2)
  where
    p :: Int
    p = n `quot` 4

    sigma4 :: SPL a
    sigma4 = (F2 ⊗ I 2) × (I 2 ⊕ (ws 2 w4 × F2))

    w4 :: a
    w4 = omega 4

-- | Improved split-radix DFT decomposition.
improvedSplitRadix :: forall a . (Floating a, RootOfUnity a) => Int -> a -> SPL a
improvedSplitRadix n w = f n w
  where
    f, fs, fs2, fs4 :: Int -> a -> SPL a
    f n _ | n `mod` 4 /= 0 =
        error "Cannot call improvedSplitRadix when n is not divisible by 4"

    f n w =
        (sigma4 ⊗ I p) ×
        (I (2*p) ⊕ diag [w^k * s p k | k <- [0..p-1]] ⊕ diag [w^^(-k) * s p k | k <- [0..p-1]]) ×
        (F (2*p) (w^2) ⊕ fs p (w^4) ⊕ fs p (w^4)×Pi (CS p 1)) ×
        (I (2*p) ⊕ Pi (L (2*p) 2)) ×
        Pi (L (4*p) 2)
      where
        p :: Int
        p = n `quot` 4

    fs n w | n `mod` 4 /= 0 =
        F n w

    fs n w =
        (sigma4 ⊗ I p) ×
        (I (2*p) ⊕ diag [w^k * s p k / s (4*p) k | k <- [0..p-1]] ⊕ diag [w^^(-k) * s p k / s (4*p) k | k <- [0..p-1]]) ×
        (fs (2*p) (w^2) ⊕ fs2 p (w^4) ⊕ fs2 p (w^4)×Pi (CS p 1)) ×
        (I (2*p) ⊕ Pi (L (2*p) 2)) ×
        Pi (L (4*p) 2)
      where
        p :: Int
        p = n `quot` 4

    fs2 n w | n `mod` 4 /= 0 =
        F n w

    fs2 n w =
        (F2 ⊗ I (2*p)) ×
        (I (2*p) ⊕ diag [s (4*p) k / s (2*4*p) k | k <- [0..2*p-1]]) ×
        ((I 2 ⊕ (ws 2 w4 × F2)) ⊗ I p) ×
        (I (2*p) ⊕ diag [w^k * s p k / s (4*p) k | k <- [0..p-1]] ⊕ diag [w^^(-k) * s p k / s (4*p) k | k <- [0..p-1]]) ×
        (fs4 (2*p) (w^2) ⊕ fs p (w^4) ⊕ fs p (w^4)×Pi (CS p 1)) ×
        (I (2*p) ⊕ Pi (L (2*p) 2)) ×
        Pi (L (4*p) 2)
      where
        p :: Int
        p = n `quot` 4

    fs4 n w | n `mod` 4 /= 0 =
        F n w

    fs4 n w =
        diag [s (4*p) k / s (4*4*p) k | k <- [0..(4*p)-1]] ×
        (sigma4 ⊗ I p) ×
        (I (2*p) ⊕ diag [w^k * s p k / s (4*p) k | k <- [0..p-1]] ⊕ diag [w^^(-k) * s p k / s (4*p) k | k <- [0..p-1]]) ×
        (fs (2*p) (w^2) ⊕ fs2 p (w^4) ⊕ fs2 p (w^4)×Pi (CS p 1)) ×
        (I (2*p) ⊕ Pi (L (2*p) 2)) ×
        Pi (L (4*p) 2)
      where
        p :: Int
        p = n `quot` 4

    s :: Floating a => Int -> Int -> a
    s n k
      | n  <= 4         = 1
      | k4 <= n `div` 8 = s (n `div` 4) k4 * cos (2 * pi * fromIntegral k4 / fromIntegral n)
      | otherwise       = s (n `div` 4) k4 * sin (2 * pi * fromIntegral k4 / fromIntegral n)
      where
        k4 :: Int
        k4 = k `mod` (n `div` 4)

    sigma4 :: SPL a
    sigma4 = (F2 ⊗ I 2) × (I 2 ⊕ (ws 2 w4 × F2))

    w4 :: a
    w4 = omega 4

-- | Decimation in time DFT matrix $F_n$, for $n$ even
dit :: RootOfUnity a => Int -> SPL a
dit = f
  where
    f 1 = spl $ matrix [[1]]
    f 2 = F2

    f n | even n =
        (f 2 ⊗ I n2) × twid n n2 (omega n) × (I 2 ⊗ f n2) × Pi (L n 2)
      where
        n2 = n `quot` 2

    f n =
        error $ "dit: not even: " ++ show n

-- | Decimation in frequency DFT matrix $F_n$, for $n$ even
dif :: RootOfUnity a => Int -> SPL a
dif = f
  where
    f 1 = spl $ matrix [[1]]
    f 2 = F2

    f n | even n =
        Pi (L n n2) × (I 2 ⊗ f n2) × twid n n2 (omega n) × (f 2 ⊗ I n2)
      where
        n2 = n `quot` 2

    f n =
        error $ "dit: not even: " ++ show n
