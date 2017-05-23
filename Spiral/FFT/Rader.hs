{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.FFT.Rader
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.FFT.Rader (
    rader
  ) where

import Spiral.Array
import Spiral.Array.Operators.IndexSpace
import Spiral.NumberTheory
import Spiral.RootOfUnity
import Spiral.SPL

-- | Rader DFT decomposition.
rader :: forall a . (RootOfUnity a, Show a) => Int -> a -> SPL a
rader p w = permute (R p a) × (one ⊕ f_pm1 (1/u)) × d_p × (one ⊕ f_pm1 u) × backpermute (R p a')
  where
    a, a' :: Int
    a  = generator p
    a' = inv p a

    one :: SPL a
    one = spl $ matrix [[1]]

    u :: a
    u = omega (p-1)

    d_p :: SPL a
    d_p = spl (fromLists [[1, 1], [1, head deltas]]) ⊕ diag (tail deltas)

    f_pm1 :: a -> SPL a
    f_pm1 = F (fromIntegral p-1)

    deltas :: [a]
    deltas = toList $
        fmap (/ (fromIntegral p-1)) $
        toMatrix (f_pm1 u) `mv` fromList [w^modExp a i p | i <- [0..p-2]]
