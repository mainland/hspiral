{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Spiral.Convolution.Tensor (
  tensorA,
  tensorB,
  tensorC
  ) where

import Spiral.SPL

import Data.List (foldl1')

-- | Generates an O Matrix for combining linear convolutions through a tensor
-- | product
oMatrix :: forall a . (Num a, Show a) => Int -> Int -> SPL a
oMatrix m n = fromFunction (ix2 rows (colM * colN)) f
  where
    rows, colM, colN :: Int
    rows = 2 * m * n - 1
    colM = (2 * m - 1)
    colN = (2 * n - 1)

    f (Z :. i :. j) = let (q, r) = j `quotRem` colN
                      in if i == q * n + r then 1 else 0

-- | Generates the recursive o matrix based on an integer factorization
fullOMatrix :: forall a . (Num a, Show a) => [Int] -> SPL a
fullOMatrix ns | length ns == 2 = oMatrix (head ns) (head $ tail ns)
               | otherwise      = oMatrix (head ns) (product $ tail ns) × (I (2 * (head ns) - 1) ⊗ (fullOMatrix $ tail ns))

tensorA :: [SPL a] -> SPL a
tensorA ls = foldl1' (⊗) ls

tensorB :: [SPL a] -> SPL a
tensorB ls = foldl1' (⊗) ls

tensorC :: forall a . (Num a, Show a) => [Int] -> [SPL a] -> SPL a
tensorC ns ls = fullOMatrix ns × (foldl1' (⊗) ls)
