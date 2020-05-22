{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- |
-- Module      :  Spiral.FFT.Bluestein
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.FFT.Bluestein (
    bluestein,
    bluestein'
  ) where

import qualified Spiral.Array as A
import Spiral.Array.Operators.Matrix
import Spiral.RootOfUnity
import Spiral.SPL hiding ((#>),
                          R)

default (Int)

-- | Decompose 'F n (w^2)'
bluestein :: forall a . RootOfUnity a => Int -> Int -> a -> SPL a
bluestein n m w = ws × t × ws
  where
    ws :: SPL a
    ws = diag [w^(k^2) | k <- [0..n-1]]

    t :: SPL a
    t = ones n m × c × ones m n

    c :: SPL a
    c = DFT' m × diag deltas × DFT m

    deltas :: [a]
    deltas = A.toList $ toMatrix (DFT m) #> A.fromList cs

    cs :: [a]
    cs = [w^^(-j^2) | j <- [0..n-1]] ++
         replicate (m - (2*n-1)) 0 ++
         [w^^(-k^2) | k <- [n-1,n-2..1]]

-- | Matrix exchanged Bluestein; This format could be extended to general
-- | convolution, instead of just two FFTs to do convolution
bluestein' :: forall a . (RootOfUnity a, Show a) => Int -> Int -> a -> SPL a
bluestein' n m w = ws × t × ws
 where
   ws :: SPL a
   ws = diag [w^(k^2) | k <- [0..n-1]]

   t :: SPL a
   t = ones n m × c × ones m n

   j :: SPL a
   j = permute $ J m

   c :: SPL a
   c = j × DFT m × diag deltas × DFT m

   deltas :: [a]
   deltas = A.toList $ toMatrix (DFT' m × j) #> A.fromList cs

   cs :: [a]
   cs = [w^^(-j^2) | j <- [0..n-1]] ++
        replicate (m - (2*n-1)) 0 ++
        [w^^(-k^2) | k <- [n-1,n-2..1]]

-- | The nxm matrix whose diagonal elements are 1 and off-diagonal elements are
-- 0.
ones :: Num a => Int -> Int -> SPL a
ones n m = fromFunction (ix2 n m) f
  where
    f (Z :. i :. j) | i == j    = 1
                    | otherwise = 0
