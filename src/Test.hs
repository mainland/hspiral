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
import qualified SPL.FFT as FFT
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
tests = [strideTest, kroneckerTest, directSumTest, f2Test, f4Test, f8Test]

-- See:
--   https://en.wikipedia.org/wiki/Kronecker_product
strideTest :: Test
strideTest = testCase "Stride matrix L^8_4" $ L 8 4 `matrixEq` a
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
-- $F_2$
f2Test :: Test
f2Test = testCase "F_2" $ FFT.f 2 `expMatrixEq` f2
  where
    f2 :: SPL (Exp (Complex Double))
    f2 = matrix [[1,  1],
                 [1, -1]]

-- $F_4$ calculated per "SPL: A Language and Compiler for DSP Algorithms"
f4Test :: Test
f4Test = testCase "F_4" $ FFT.f 4 `expMatrixEq` f4
  where
    f4 :: SPL (Exp (Complex Double))
    f4 = matrix [[1,  1,  1,  1],
                 [1, -i, -1,  i],
                 [1, -1,  1, -1],
                 [1,  i, -1, -i]]
      where
        i = ComplexC 0 1

-- $F_8$ calculated per "SPL: A Language and Compiler for DSP Algorithms"
-- See also:
--   https://en.wikipedia.org/wiki/DFT_matrix
f8Test :: Test
f8Test = testCase "F_8" $ FFT.f 8 `expMatrixEq` f8
  where
    f8 :: SPL (Exp (Complex Double))
    f8 = matrix [[1,  1,  1,  1, 1, 1, 1, 1],
                 [1,  w, -i,  -i*w, -1, -w, i, i*w],
                 [1, -i, -1, i, 1, -i, -1, i],
                 [1, -i*w, i, w, -1, i*w, -i, -w],
                 [1, -1, 1, -1, 1, -1, 1, -1],
                 [1, -w, -i, i*w, -1, w, i, -i*w],
                 [1, i, -1, -i, 1, i, -1, -i],
                 [1, i*w, i, -w, -1, -i*w, -i, w]]
      where
        i = ComplexC 0 1

        w = omega (8 :: Int)
