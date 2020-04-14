{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Spiral.FFT.Winograd (
  winogradSmall,
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
