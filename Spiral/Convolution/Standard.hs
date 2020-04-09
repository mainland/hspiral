{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Spiral.Convolution.Standard (
  standardLinearA,
  standardLinearB,
  standardLinearC,
  ) where

import Spiral.SPL

import Data.List (foldl')

-- | A matrix of all zeros
zeros :: (Num a, Show a) => Int -> Int -> SPL a
zeros r c | r == c    = KDiag r 0
          | otherwise = fromFunction (ix2 r c) (\_ -> 0)


-- | Generates the C matrix for standard linear convolution
standardLinearC :: forall a . (Num a, Show a) => Int -> SPL a
standardLinearC 1 = I 1
standardLinearC n = fromFunction (ix2 l width) f
  where
    width, l, mid_l :: Int
    width = n * n  -- # of cols
    l = n + n - 1  -- # of rows
    mid_l = l `div` 2 -- Mid point

    f (Z :. i :. j) | i < mid_l = let pad = foldl' (+) 0 [0..i]
                                  in if j >= pad && j < pad + i + 1 then 1 else 0
                    | i > mid_l = let pad = width - (l - i) - (foldl' (+) 0 [0..(l - i - 1)])
                                  in if j >= pad && j < pad + l - i then 1 else 0
                    | otherwise = let pad = (width - n) `div` 2
                                  in if j >= pad && j < pad + n then 1 else 0

-- | Generates the B matrix for standard linear convolution
-- | Generated through a stacking of reverse identity matrices
standardLinearB :: forall a . (Num a, Show a) => Int -> SPL a
standardLinearB 1 = I 1
standardLinearB n = foldr1 (<->) [getStack i | i <- [1..(2*n-1)]]
  where
    getStack :: Int -> SPL a
    getStack x | x < n     = permute (J x) <|> zeros x (n-x)
               | x == n    = permute (J n)
               | otherwise = let x' = n - (rem x n) in zeros x' (n-x') <|> permute (J x')

-- | Generates the A matrix for standard linear convolution
-- | Generated through a stacking of identity matrices
standardLinearA :: forall a . (Num a, Show a) => Int -> SPL a
standardLinearA 1 = I 1
standardLinearA n = foldr1 (<->) [getStack i | i <- [1..(2*n-1)]]
  where
    getStack :: Int -> SPL a
    getStack x | x < n     = I x <|> zeros x (n-x)
               | x == n    = I n
               | otherwise = let x' = n - (rem x n) in zeros x' (n-x') <|> I x'
