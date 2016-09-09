{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.FFT
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.FFT (
    omega,

    w,
    t,
    f
  ) where

import Text.PrettyPrint.Mainland

import Spiral.Array
import Spiral.ExtendedFloat
import Spiral.SPL

-- | The $W_m(\omega_n)$ matrix
w :: ExtendedFloat a => Int -> Int -> SPL a
w m n = diag [w^i | i <- [0..m-1]]
  where
    w = omega n

-- | Twiddle factor matrix $T^{mn}_m$
t :: ExtendedFloat a => Int -> Int -> SPL a
t mn m = I m ⊕ go (n-1)
  where
    n = mn `quot` m

    go i | i == 1    = w m mn
         | otherwise = w m mn ⊕ go (i-1)

-- | DFT matrix $F_n$, for $n$ even
f :: (ExtendedFloat a, Pretty a) => Int -> SPL a
f 1 = spl $ matrix [[1]]
f 2 = F2

f n | even n =
    (f 2 ⊗ I n2) × t n n2 × (I 2 ⊗ f n2) × Pi (L n 2)
  where
    n2 = n `quot` 2

f n =
    error $ "f: not even: " ++ show n
