{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Test
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Main (main) where

import Data.Complex

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit ((@?=),
                   Assertion)

import SPL.Exp
import SPL.ExtendedFloat
import SPL.Syntax

infix 1 `matrixEq`
matrixEq :: (Eq e, Show e, Num e) => SPL e -> SPL e -> Assertion
matrixEq a b = matrixOf a @?= matrixOf b

infix 1 `expMatrixEq`
expMatrixEq :: (e ~ Exp (Complex Double)) => SPL e -> SPL e -> Assertion
expMatrixEq a b = co a @?= co b
  where
    co = map (map toComplex) . matrixOf

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [strideTest, kroneckerTest, directSumTest, fftTest]

-- See:
--   https://en.wikipedia.org/wiki/Kronecker_product
strideTest :: Test
strideTest = testCase "Stride matrix L^8_$" $ L 8 4 `matrixEq` a
  where
    a :: SPL Int
    a = matrix [[1, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 1, 0, 0, 0],
                [0, 1, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 1, 0, 0],
                [0, 0, 1, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 1, 0],
                [0, 0, 0, 1, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 1]]
-- See:
--   https://en.wikipedia.org/wiki/Kronecker_product
kroneckerTest :: Test
kroneckerTest = testCase "Kronecker product (⊗)" $ a ⊗ b `matrixEq` c
  where
    a, b, c :: SPL Int
    a = matrix [[1, 2],
                [3, 4]]
    b = matrix [[0, 5],
                [6, 7]]
    c = matrix [[0, 5, 0, 10],
                [6, 7, 12, 14],
                [0, 15, 0, 20],
                [18, 21, 24, 28]]

-- See:
--   https://en.wikipedia.org/wiki/Matrix_addition#Direct_sum
directSumTest :: Test
directSumTest = testCase "Direct sum (⊕)" $ a ⊕ b `matrixEq` c
  where
    a, b, c :: SPL Int
    a = matrix [[1, 3, 2],
                [2, 3, 1]]
    b = matrix [[1, 6],
                [0, 1]]
    c = matrix [[1, 3, 2, 0, 0],
                [2, 3, 1, 0, 0],
                [0, 0, 0, 1, 6],
                [0, 0, 0, 0, 1]]

-- $F_4$ calculated per "SPL: A Language and Compiler for DSP Algorithms"
fftTest :: Test
fftTest = testCase "F_4" $ f 4 `expMatrixEq` f4
  where
    f4 :: SPL (Exp (Complex Double))
    f4 = matrix [[1,  1,  1,  1],
                 [1, -i, -1,  i],
                 [1, -1,  1, -1],
                 [1,  i, -1, -i]]
      where
        i = ComplexC 0 1

    -- | The $W_m(\omega_n)$ matrix
    w :: forall e . ExtendedFloat e => Int -> Int -> SPL e
    w m n = D (m, m) f
      where
        f :: Ix -> e
        f (i, j) | i == j    = omega n^i
                 | otherwise = 0

    -- | Twiddle factor matrix $T^{mn}_n$
    t :: ExtendedFloat e => Int -> Int -> SPL e
    t mn n = I m ⊕ go (n-1)
      where
        m = mn `quot` n

        go i | i == 1    = w m mn
             | otherwise = w m mn ⊕ go (i-1)

    -- | DFT matrix $F_n$, for $n$ even
    f :: ExtendedFloat e => Int -> SPL e
    f 2 = matrix [[1,  1],
                  [1, -1]]

    f n | even n =
        (f 2 ⊗ I n2) × t n n2 × (I 2 ⊗ f n2) × L n 2
      where
        n2 = n `quot` 2

    f n =
        error $ "f: not even: " ++ show n
