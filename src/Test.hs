{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Test.Framework.Providers.QuickCheck2
import Test.HUnit ((@?=))
import Test.QuickCheck

import SPL.Exp
import SPL.ExtendedFloat
import qualified SPL.FFT as FFT
import SPL.Syntax

-- | A small number
newtype SmallPowerOfTwo = SmallPowerOfTwo Int
    deriving (Eq, Ord, Show, Num, Integral, Real, Enum)

instance Arbitrary SmallPowerOfTwo where
    arbitrary = SmallPowerOfTwo <$> (choose (1, 7) :: Gen Int)

    shrink (SmallPowerOfTwo 0) = []
    shrink (SmallPowerOfTwo n) = [SmallPowerOfTwo (n-1)]

-- | An 'Exp (Complex Double)' that is guaranteed to be a root of unity. Useful
-- for property testing.
newtype RootOfUnity = RootOfUnity (Exp (Complex Double))

instance Arbitrary RootOfUnity where
    arbitrary = (RootOfUnity . RouC) <$> arbitrary

    shrink (RootOfUnity (RouC r)) = [RootOfUnity (RouC r') | r' <- shrink r]
    shrink _                      = []

complexMatrixOf :: (ToComplex f, Num (f (Complex Double)))
                => SPL (f (Complex Double))
                -> [[f (Complex Double)]]
complexMatrixOf = map (map toComplex) . matrixOf

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [strideTest, kroneckerTest, directSumTest, dftTests]

-- See:
--   https://en.wikipedia.org/wiki/Kronecker_product
strideTest :: Test
strideTest = testCase "Stride matrix L^8_4" $
    matrixOf (L 8 4) @?= matrixOf a
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
kroneckerTest = testCase "Kronecker product (⊗)" $
    matrixOf (a ⊗ b) @?= matrixOf c
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
directSumTest = testCase "Direct sum (⊕)" $
    matrixOf (a ⊕ b) @?= matrixOf c
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

dftTests :: Test
dftTests = testGroup "DFT tests"
    [f2Test, f4Test, f8Test, testProperty "DFT" prop_DFT]

prop_DFT :: SmallPowerOfTwo -> Property
prop_DFT (SmallPowerOfTwo n) =
    complexMatrixOf (directDFT (2^n)) === complexMatrixOf (FFT.f (2^n))
  where
    -- | Direct calculation of the DFT matrix.
    directDFT :: Int -> SPL (Exp (Complex Double))
    directDFT n = D (n, n) f
      where
        f (i, j) = w^(i*j)

        w = omega n

-- $F_2$
f2Test :: Test
f2Test = testCase "F_2" $ complexMatrixOf (FFT.f 2) @?= complexMatrixOf f2
  where
    f2 :: SPL (Exp (Complex Double))
    f2 = matrix [[1,  1],
                 [1, -1]]

-- $F_4$ calculated per "SPL: A Language and Compiler for DSP Algorithms"
f4Test :: Test
f4Test = testCase "F_4" $ complexMatrixOf (FFT.f 4) @?= complexMatrixOf f4
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
f8Test = testCase "F_8" $ complexMatrixOf (FFT.f 8) @?= complexMatrixOf f8
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
