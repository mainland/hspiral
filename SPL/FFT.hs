{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  SPL.FFT
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module SPL.FFT (
    w,
    t,
    f
  ) where

import SPL.ExtendedFloat
import SPL.Syntax

-- | The $W_m(\omega_n)$ matrix
w :: forall e . ExtendedFloat e => Int -> Int -> SPL e
w m n = D (m, m) f
  where
    f :: Ix -> e
    f (i, j) | i == j    = omega n^i
             | otherwise = 0

-- | Twiddle factor matrix $T^{mn}_m$
t :: ExtendedFloat e => Int -> Int -> SPL e
t mn m = I m ⊕ go (n-1)
  where
    n = mn `quot` m

    go i | i == 1    = w m mn
         | otherwise = w m mn ⊕ go (i-1)

-- | DFT matrix $F_n$, for $n$ even
f :: ExtendedFloat e => Int -> SPL e
f 1 = matrix [[1]]

f 2 = matrix [[1,  1],
              [1, -1]]

f n | even n =
    (f 2 ⊗ I n2) × t n n2 × (I 2 ⊗ f n2) × L n 2
  where
    n2 = n `quot` 2

f n =
    error $ "f: not even: " ++ show n
