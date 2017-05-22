{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.FFT.GoodThomas
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.FFT.GoodThomas (
    goodThomas
  ) where

import Spiral.NumberTheory (euclid)
import Spiral.RootOfUnity
import Spiral.SPL

-- | Good-Thomas DFT decomposition.
goodThomas :: forall a . RootOfUnity a => Int -> Int -> a -> SPL a
goodThomas r s w = _Q' × (RDFT r (w^^er) ⊗ RDFT s (w^^es)) × _Q
  where
    (u, v) = euclid r s
    er     = s * v
    es     = r * u

    pi :: Permutation
    pi = CRT r s er es

    _Q :: SPL a
    _Q = Pi pi

    _Q' :: SPL a
    _Q' = Pi (invert pi)
