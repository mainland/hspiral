{-# LANGUAGE FlexibleContexts #-}
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
import Test.QuickCheck ((===),
                        Arbitrary(..),
                        Gen,
                        Property,
                        choose)

import Spiral.Array
import Spiral.Exp
import qualified Spiral.FFT as FFT
import Spiral.SPL

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
    arbitrary = (RootOfUnity . ConstE . RouC) <$> arbitrary

    shrink (RootOfUnity (ConstE (RouC r))) =
        [RootOfUnity (ConstE (RouC r')) | r' <- shrink r]

    shrink _ =
        []

manifestComplex :: (IArray r DIM2 (Exp (Complex Double)))
                => Matrix r (Exp (Complex Double))
                -> Matrix M (Exp (Complex Double))
manifestComplex = fmap f . manifest
  where
    f :: Exp a -> Exp a
    f (ConstE c) = ConstE (flatten c)
    f e          = e

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [strideTest, kroneckerTest, directSumTest, dftTests]

-- See:
--   https://en.wikipedia.org/wiki/Kronecker_product
strideTest :: Test
strideTest = testCase "Stride matrix L^8_4" $
    toMatrix (L 8 4) @?= a
  where
    a :: Matrix M Int
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
    toMatrix (spl a ⊗ spl b) @?= c
  where
    a, b, c :: Matrix M Int
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
    toMatrix (spl a ⊕ spl b) @?= c
  where
    a, b, c :: Matrix M Int
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
    manifestComplex (toMatrix (DFT (2^n))) === manifestComplex (toMatrix (FFT.f (2^n)))

-- $F_2$
f2Test :: Test
f2Test = testCase "F_2" $ manifestComplex (toMatrix (FFT.f 2)) @?= manifestComplex f2
  where
    f2 :: Matrix M (Exp (Complex Double))
    f2 = matrix [[1,  1],
                 [1, -1]]

-- $F_4$ calculated per "SPL: A Language and Compiler for DSP Algorithms"
f4Test :: Test
f4Test = testCase "F_4" $ manifestComplex (toMatrix (FFT.f 4)) @?= manifestComplex f4
  where
    f4 :: Matrix M (Exp (Complex Double))
    f4 = matrix [[1,  1,  1,  1],
                 [1, -i, -1,  i],
                 [1, -1,  1, -1],
                 [1,  i, -1, -i]]
      where
        i :: Exp (Complex Double)
        i = complexE (0 :+ 1)

-- $F_8$ calculated per "SPL: A Language and Compiler for DSP Algorithms"
-- See also:
--   https://en.wikipedia.org/wiki/DFT_matrix
f8Test :: Test
f8Test = testCase "F_8" $ manifestComplex (toMatrix (FFT.f 8)) @?= manifestComplex f8
  where
    f8 :: Matrix M (Exp (Complex Double))
    f8 = matrix [[1,  1,  1,  1, 1, 1, 1, 1],
                 [1,  w, -i,  -i*w, -1, -w, i, i*w],
                 [1, -i, -1, i, 1, -i, -1, i],
                 [1, -i*w, i, w, -1, i*w, -i, -w],
                 [1, -1, 1, -1, 1, -1, 1, -1],
                 [1, -w, -i, i*w, -1, w, i, -i*w],
                 [1, i, -1, -i, 1, i, -1, -i],
                 [1, i*w, i, -w, -1, -i*w, -i, w]]
      where
        i = complexE (0 :+ 1)

        w = FFT.omega (8 :: Int)
