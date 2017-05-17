-- |
-- Module      :  Spiral.FFT.CooleyTukey
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.FFT.CooleyTukey (
    cooleyTukey
  ) where

import Data.List (foldr1)

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
