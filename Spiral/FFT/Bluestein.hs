{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- |
-- Module      :  Spiral.FFT.Bluestein
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.FFT.Bluestein (
    bluestein
  ) where

import Spiral.Array hiding ((!!))
import Spiral.Array.Operators.IndexSpace
import Spiral.RootOfUnity
import Spiral.SPL hiding (R)

default (Int)

-- | Decompose 'RDFT n (w^2)'
bluestein :: forall a . RootOfUnity a => Int -> Int -> a -> SPL a
bluestein n m w = ws × t × ws
  where
    ws :: SPL a
    ws = diag [w^(k^2) | k <- [0..n-1]]

    t :: SPL a
    t = ones n m × c × ones m n

    c :: SPL a
    c = IDFT m × diag deltas × DFT m

    deltas :: [a]
    deltas = toList $ toMatrix (DFT m) `mv` fromList cs

    cs :: [a]
    cs = [w^^(-j^2) | j <- [0..n-1]] ++
         replicate (m - (2*n-1)) 0 ++
         [w^^(-k^2) | k <- [n-1,n-2..1]]

-- | The nxm matrix whose diagonal elements are 1 and off-diagonal elements are
-- 0.
ones :: Num a => Int -> Int -> SPL a
ones n m = spl $ fromFunction (ix2 n m) f
  where
    f (Z :. i :. j) | i == j    = 1
                    | otherwise = 0
