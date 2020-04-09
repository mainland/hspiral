{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Spiral.Convolution.ToomCook (
  toomCookA,
  toomCookB,
  toomCookC
  ) where

import Spiral.SPL

import Spiral.Convolution.Core (Polynomial, HPoly(..))

import Data.List (foldl1')

-- | Generates a 1x1 matrix of just 1
one :: forall a . (Num a, Show a) => SPL a
one = I 1

-- | A padded vandermonde matrix with a default alpha point scheme
extVandermondeW :: forall a . (Fractional a, Show a) => Int -> Int -> SPL a
extVandermondeW n m = fromFunction (ix2 (n+1) m) f
  where
    f (Z :. x :. y) | x < n      = (getAlpha x)^y
                    | y == (m-1) = 1
                    | otherwise  = 0

-- | A padded inverse vandermonde matrix with default alpha point scheme
extInvVandermondeW :: forall a . (Eq a, Fractional a, Show a) => Int -> SPL a
extInvVandermondeW n = numerators × (divisors ⊕ I 1)
 where
   divisors :: SPL a
   divisors = diag [1/(foldl1' (*) [(getAlpha x) - (getAlpha y) | y <- [0..(n-1)], x /= y]) | x <- [0..(n-1)]]

   vandPolys :: Int -> [Polynomial Rational]
   vandPolys i | i == n    = [construct [-(getAlpha i), 1 :: Int] | i <- [0..(n-1)]]
               | otherwise = let (f, b) = splitAt i (vandPolys n)
                             in f ++ tail b

   cs :: [[a]]
   cs = [coeffs (foldl1' mult (vandPolys i)) | i <- [0..n]]

   numerators :: SPL a
   numerators = fromLists [[(f i j) | j <- [0..n]] | i <- [0..n]]
    where
      f x y | x < n && y < n = (cs !! y) !! x
            | y < n          = 0
            | otherwise      = (cs !! y) !! x

-- | Generates an alpha for a Vandermonde, given a default scheme for
-- | generating values
getAlpha :: Num a => Int -> a
getAlpha x = g x
  where
    f x = fromIntegral $ div (x+1) 2
    g 0 = 0
    g x = if odd x then f x else -(f x)

-- | Generation of the A matrix for a Toom Cook linear convolution
toomCookA :: forall a . (Fractional a, Show a, Eq a) => Int -> SPL a
toomCookA 1 = one
toomCookA 3 = a × b × c
  where
    a, b, c :: SPL a
    a = fromLists [[1, 0, 0, 0, 0, 0],
                   [0, 1, 0, 0, 0, 0],
                   [1, 0, 1, 0, 0, 0],
                   [0, 1, 1, 1, 1, 0],
                   [0, 0, 0, 0, 0, 1]]
    b = fromLists [[1, 0, 0, 0],
                   [1, 1, 0, 0],
                   [0, 0, 1, 0],
                   [0, 1, 0, 0],
                   [0, 1, 0, 0],
                   [0, 0, 0, 1]]
    c = fromLists [[1,  0, 0],
                   [0,  1, 1],
                   [0, -1, 1],
                   [0,  0, 1]]
toomCookA n = extVandermondeW (2 * n - 2) n

-- | A and B matrices are identical
toomCookB :: (Fractional a, Show a, Eq a) => Int -> SPL a
toomCookB = toomCookA

-- | Generation of the C matrix for Toom Cook linear convolution
toomCookC :: (Fractional a, Show a, Eq a) => Int -> SPL a
toomCookC 1 = one
toomCookC n = extInvVandermondeW (2 * n - 2)
