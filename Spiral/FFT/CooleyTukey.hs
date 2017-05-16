{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.FFT.CooleyTukey
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.FFT.CooleyTukey (
    _W,
    _T,
    cooleyTukey
  ) where

import Data.List (foldr1)

import Spiral.RootOfUnity
import Spiral.SPL

-- | The $W_n(\omega)$ matrix
_W :: RootOfUnity a => Int -> a -> SPL a
_W n w = diag [w^i | i <- [0..n-1]]

-- | Twiddle factor matrix $T^{rs}_s(\omega)$
_T :: RootOfUnity a => Int -> Int -> a -> SPL a
_T rs s w = foldr1 (⊕) [_W s (w^i) | i <- [0..r-1]]
  where
    r = rs `quot` s

cooleyTukey :: RootOfUnity a => Int -> Int -> a -> SPL a
cooleyTukey r s w =
    (RDFT r (w^s) ⊗ I s) × _T (r*s) s w × (I r ⊗ RDFT s (w^r)) × Pi (L (r*s) r)
