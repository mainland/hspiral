{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Spiral.FFT.Winograd (
  -- Prime algorithms
  winogradSmall,

  -- Power of 2 algorithms
  winogradDIT,
  winogradDIF,
  winogradSplitRadix,
  winogradConjPairSplitRadix,
  winogradSplitRadix8,
  winogradImprovedSplitRadix,
  ) where

import Spiral.Convolution
import Spiral.NumberTheory
import Spiral.RootOfUnity
import Spiral.SPL

default (Int)

-- | A 1x1 matrix of just a 1
one :: (RootOfUnity a, Show a) => SPL a
one = I 1

-- | The bilinear factorization of a prime FFT
-- | per Selesnick+Burrus in their automation of prime FFT paper
biWinogradSmall :: forall a . (RootOfUnity a, Show a, Eq a)
                => Int
                -> CyclicConvolution a
                -> (SPL a, SPL a, SPL a)
biWinogradSmall p conv = (cW, bW, aW)
  where
    a, a' :: Int
    a = generator p
    a' = inv p a

    qS, qR, qR' :: SPL a
    qS  = backpermute (R p a')
    qR  = permute (R p a)
    qR' = backpermute (R p a)

    u, v, ut :: SPL a
    u  = fromLists [[1, 1],
                    [0, 1]]
    v  = fromLists [[ 1, 0],
                    [-1, 1]]
    ut = fromLists [[1, 0],
                    [1, 1]]

    up, vp, upt :: SPL a -> SPL a
    up  x = let (Z :. m :. _) = extent x
            in u ⊕ I (m-1)
    vp  x = let (Z :. m :. _) = extent x
            in v ⊕ I (m-1)
    upt x = let (Z :. m :. _) = extent x
            in ut ⊕ I (m-1)

    aW, bW, cW :: SPL a
    aW = (up (getB conv)) × (one ⊕ (getA conv)) × qS
    bW = (vp (getB conv)) × (one ⊕ (getB conv)) × qR'
    cW = qR × (one ⊕ (getC conv)) × (upt (getB conv))

-- | Given a bilinear algorithm for computing cyclic convolution of (p-1)
-- | produces a Winograd FFT of size p, where p is prime
winogradSmall :: forall a . (RootOfUnity a, Show a, Eq a)
              => Int
              -> a
              -> CyclicConvolution a
              -> SPL a
winogradSmall p w conv = let (c, b, a) = biWinogradSmall p conv
                         in bilinear omegas c b a
  where
    omegas :: [a]
    omegas = [w^i | i <- [0..p-1]]

-- | Twiddle factor matrix $T^{rs}_s(\omega)$
twid :: RootOfUnity a => Int -> Int -> a -> SPL a
twid rs s w = foldr1 (⊕) [ws s (w^i) | i <- [0..r-1]]
 where
   r = rs `quot` s

-- | The $W_n(\omega)$ matrix
ws :: RootOfUnity a => Int -> a -> SPL a
ws n w = diag [w^i | i <- [0..n-1]]

winogradDIT :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> Int -> a -> SPL a
winogradDIT r s w = let (c, b, a) = biWinogradDIT r s w
                    in bilinear os c b a
  where
    n = r*s

    os :: [a]
    os = [w^i | i <- [0..n-1]]

biWinogradDIT :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> Int -> a -> (SPL a, SPL a, SPL a)
biWinogradDIT 2 1 w = biWinogradDIT 1 2 w
biWinogradDIT 1 2 w = (c, b, a)
 where
   c, b, a :: SPL a
   a = F2
   b = diag [1, 1/w]
   c = I 2
biWinogradDIT r s w = (c, b, a)
  where
    n = r*s
    os = [1/(w^i) | i <- ([0..n-1] :: [Int])]

    c, b, a :: SPL a
    c = (F r (w^s) ⊗ I s)
    b = twid (r*s) s w × diag os
    a = (I r ⊗ F s (w^r)) × Pi (L (r*s) r)

winogradDIF :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> Int -> a -> SPL a
winogradDIF r s w = let (c, b, a) = biWinogradDIF r s w
                    in bilinear os c b a
  where
    n = r*s

    os :: [a]
    os = [w^i | i <- [0..n-1]]

biWinogradDIF :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> Int -> a -> (SPL a, SPL a, SPL a)
biWinogradDIF 1 2 w = biWinogradDIF 2 1 w
biWinogradDIF 2 1 w = (c, b, a)
  where
    c, b, a :: SPL a
    a = I 2
    b = diag [1, 1/w]
    c = F2
biWinogradDIF r s w = (c, b, a)
  where
    n = r*s
    os = [1/(w^i) | i <- ([0..n-1] :: [Int])]

    c, b, a :: SPL a
    c = Pi (L (r*s) s) × (I r ⊗ F s (w^r))
    b = twid (r*s) s w × diag os
    a = (F r (w^s) ⊗ I s)

winogradSplitRadix :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> a -> SPL a
winogradSplitRadix n w = let (c, b, a) = biWinogradSplitRadix n w
                         in bilinear os c b a
  where
    os :: [a]
    os = [w^i | i <- [0..n-1]]

biWinogradSplitRadix :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> a -> (SPL a, SPL a, SPL a)
biWinogradSplitRadix n _ | n `mod` 4 /= 0 =
    error "Cannot call splitRadix when n is not divisible by 4"
biWinogradSplitRadix n w = (c, b, a)
  where
    os = [1/(w^i) | i <- ([0..n-1] :: [Int])]

    p :: Int
    p = n `quot` 4

    sigma4 :: SPL a
    sigma4 = (F2 ⊗ I 2) × (I 2 ⊕ (ws 2 w4 × F2))

    w4 :: a
    w4 = w ^ (n `div` 4)

    c, b, a :: SPL a
    c = (sigma4 ⊗ I p)
    b = (I (2*p) ⊕ ws p w ⊕ ws p (w^3)) × diag os
    a = (F (2*p) (w^2) ⊕ F p (w^4) ⊕ F p (w^4)) × (I (2*p) ⊕ Pi (L (2*p) 2)) × Pi (L (4*p) 2)


winogradConjPairSplitRadix :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> a -> SPL a
winogradConjPairSplitRadix n w = let (c, b, a) = biWinogradConjPairSplitRadix n w
                                 in bilinear os c b a
  where
    os :: [a]
    os = [w^i | i <- [0..n-1]]

biWinogradConjPairSplitRadix :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> a -> (SPL a, SPL a, SPL a)
biWinogradConjPairSplitRadix n _ | n `mod` 4 /= 0 =
    error "Cannot call splitRadix when n is not divisible by 4"
biWinogradConjPairSplitRadix n w = (c, b, a)
  where
    os = [1/(w^i) | i <- ([0..n-1] :: [Int])]

    p :: Int
    p = n `quot` 4

    sigma4 :: SPL a
    sigma4 = (F2 ⊗ I 2) × (I 2 ⊕ (ws 2 w4 × F2))

    w4 :: a
    w4 = w ^ (n `div` 4)

    c, b, a :: SPL a
    c = (sigma4 ⊗ I p)
    b = (I (2*p) ⊕ ws p w ⊕ ws p (w^^(-1))) × diag os
    a = (F (2*p) (w^2) ⊕ F p (w^4) ⊕ F p (w^4)×Pi (CS p 1)) × (I (2*p) ⊕ Pi (L (2*p) 2)) × Pi (L (4*p) 2)


winogradSplitRadix8 :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> a -> SPL a
winogradSplitRadix8 n w = let (c, b, a) = biWinogradSplitRadix8 n w
                          in bilinear os c b a
  where
    os :: [a]
    os = [w^i | i <- [0..n-1]]

biWinogradSplitRadix8 :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> a -> (SPL a, SPL a, SPL a)
biWinogradSplitRadix8 n _ | n `mod` 8 /= 0 =
    error "Cannot call splitRadix8 when n is not divisible by 8"
biWinogradSplitRadix8 n w = (c, b, a)
  where
    os = [1/(w^i) | i <- ([0..n-1] :: [Int])]

    p :: Int
    p = n `quot` 8

    sigma8 :: SPL a
    sigma8 = (F2 ⊗ I 4) × (I 4 ⊕ (ws 4 w8 × (F2 ⊗ I 2))) × (I 6 ⊕ (ws 2 w4 × F2))

    w4, w8 :: a
    w4 = w ^ (n `div` 4)
    w8 = w ^ (n `div` 8)

    c, b, a :: SPL a
    c = (sigma8 ⊗ I p)
    b = (I (4*p) ⊕ ws p w ⊕ ws p w ⊕ ws p (w^3) ⊕ ws p (w^7)) × diag os
    a = (F (4*p) (w^2) ⊕ F (2*p) (w^4) ⊕ F p (w^8) ⊕ F p (w^8)) ×
        (I (6*p) ⊕ Pi (L (2*p) 2)) ×
        (I (4*p) ⊕ Pi (L (4*p) 2)) ×
        Pi (L (8*p) 2)

winogradImprovedSplitRadix :: forall a . (RootOfUnity a, Show a, Eq a, Floating a) => Int -> a -> SPL a
winogradImprovedSplitRadix n w = let (c, b, a) = biWinogradImprovedSplitRadix n w
                                 in bilinear os c b a
  where
    os :: [a]
    os = [w^i | i <- [0..n-1]]

biWinogradImprovedSplitRadix :: forall a . (RootOfUnity a, Show a, Eq a, Floating a) => Int -> a -> (SPL a, SPL a, SPL a)
biWinogradImprovedSplitRadix n _ | n `mod` 4 /= 0 =
    error "Cannot call splitRadix when n is not divisible by 4"
biWinogradImprovedSplitRadix n w = (c, b, a)
  where
    os = [1/(w^i) | i <- ([0..n-1] :: [Int])]

    p :: Int
    p = n `quot` 4

    sigma4 :: SPL a
    sigma4 = (F2 ⊗ I 2) × (I 2 ⊕ (ws 2 w4 × F2))

    w4 :: a
    w4 = w ^ (n `div` 4)

    s :: Floating a => Int -> Int -> a
    s n k
      | n  <= 4         = 1
      | k4 <= n `div` 8 = s (n `div` 4) k4 * cos (2 * pi * fromIntegral k4 / fromIntegral n)
      | otherwise       = s (n `div` 4) k4 * sin (2 * pi * fromIntegral k4 / fromIntegral n)
      where
        k4 :: Int
        k4 = k `mod` (n `div` 4)

    c, b, a :: SPL a
    c = (sigma4 ⊗ I p)
    b = (I (2*p) ⊕ diag [w^k * s p k | k <- [0..p-1]] ⊕ diag [w^^(-k) * s p k | k <- [0..p-1]]) × diag os
    a = (F (2*p) (w^2) ⊕ fs p (w^4) ⊕ fs p (w^4) × Pi (CS p 1)) × (I (2*p) ⊕ Pi (L (2*p) 2)) × Pi (L (4*p) 2)
      where
      fs, fs2, fs4 :: Int -> a -> SPL a
      fs n w | n `mod` 4 /= 0 =
        diag [1/s n k | k <- [0..n-1]] × F n w

      fs n w =
          (sigma4 ⊗ I p) ×
          (I (2*p) ⊕ diag [w^k * s p k / s (4*p) k | k <- [0..p-1]] ⊕ diag [w^^(-k) * s p k / s (4*p) k | k <- [0..p-1]]) ×
          (fs2 (2*p) (w^2) ⊕ fs p (w^4) ⊕ fs p (w^4)×Pi (CS p 1)) ×
          (I (2*p) ⊕ Pi (L (2*p) 2)) ×
          Pi (L (4*p) 2)
        where
          p :: Int
          p = n `quot` 4

      fs2 n w | n `mod` 4 /= 0 =
        diag [1/s (2*n) k | k <- [0..n-1]] × F n w

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
        diag [1/s (4*n) k | k <- [0..n-1]] × F n w

      fs4 n w =
          diag [s (4*p) k / s (4*4*p) k | k <- [0..(4*p)-1]] ×
          (sigma4 ⊗ I p) ×
          (I (2*p) ⊕ diag [w^k * s p k / s (4*p) k | k <- [0..p-1]] ⊕ diag [w^^(-k) * s p k / s (4*p) k | k <- [0..p-1]]) ×
          (fs2 (2*p) (w^2) ⊕ fs p (w^4) ⊕ fs p (w^4)×Pi (CS p 1)) ×
          (I (2*p) ⊕ Pi (L (2*p) 2)) ×
          Pi (L (4*p) 2)
        where
          p :: Int
          p = n `quot` 4
