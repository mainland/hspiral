{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.FFT
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.FFT (
    w,
    t,
    f
  ) where

import Text.PrettyPrint.Mainland

import Spiral.ExtendedFloat
import Spiral.SPL

-- | The $W_m(\omega_n)$ matrix
w :: forall e . (ExtendedFloat e, Pretty e) => Int -> Int -> Matrix SPL e
w m n = spl $ fromFunction (ix2 m m) f
  where
    f :: DIM2 -> e
    f (Z :. i :. j) | i == j    = omega n^i
                    | otherwise = 0

-- | Twiddle factor matrix $T^{mn}_m$
t :: (ExtendedFloat e, Pretty e) => Int -> Int -> Matrix SPL e
t mn m = I m ⊕ go (n-1)
  where
    n = mn `quot` m

    go i | i == 1    = w m mn
         | otherwise = w m mn ⊕ go (i-1)

-- | DFT matrix $F_n$, for $n$ even
f :: (ExtendedFloat e, Pretty e) => Int -> Matrix SPL e
f 1 = spl $ matrix [[1]]

f 2 = spl $  matrix [[1,  1],
                     [1, -1]]

f n | even n =
    (f 2 ⊗ I n2) × t n n2 × (I 2 ⊗ f n2) × L n 2
  where
    n2 = n `quot` 2

f n =
    error $ "f: not even: " ++ show n
