-- |
-- Module      :  Spiral.FFT.CooleyTukey
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.FFT.CooleyTukey (
    cooleyTukey,
    dit
  ) where

import Data.List (foldr1)

import Spiral.Array
import Spiral.RootOfUnity
import Spiral.SPL

-- | The $W_n(\omega)$ matrix
ws :: RootOfUnity a => Int -> a -> SPL a
ws n w = diag [w^i | i <- [0..n-1]]

-- | Twiddle factor matrix $T^{rs}_s(\omega)$
twid :: RootOfUnity a => Int -> Int -> a -> SPL a
twid rs s w = foldr1 (⊕) [ws s (w^i) | i <- [0..r-1]]
  where
    r = rs `quot` s

-- | Cooley-Tukey DFT decomposition.
cooleyTukey :: RootOfUnity a => Int -> Int -> a -> SPL a
cooleyTukey r s w =
    (RDFT r (w^s) ⊗ I s) × twid (r*s) s w × (I r ⊗ RDFT s (w^r)) × Pi (L (r*s) r)

-- | Decimation in time DFT matrix $F_n$, for $n$ even
dit :: (RootOfUnity a, Show a) => Int -> SPL a
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
