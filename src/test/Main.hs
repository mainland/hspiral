{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Test
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Main where

import Control.Monad (replicateM)
import Data.Complex
import qualified Data.Vector.Storable as V
import System.Environment (getArgs)
import Test.Framework (Test,
                       defaultMainWithArgs,
                       testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck ((===),
                        Arbitrary(..),
                        Gen,
                        Property,
                        forAll,
                        choose,
                        counterexample)

import Spiral (Config(..),
               parseOpts)
import Spiral.Array
import Spiral.Exp
import qualified Spiral.FFT as FFT
import Spiral.FFT.CooleyTukey
import Spiral.FFT.GoodThomas
import Spiral.FFT.Rader
import Spiral.SPL

import qualified Test.FFTW as FFTW
import Test.Gen

main :: IO ()
main = do
    (conf, args') <- getArgs >>= parseOpts
    dftTests <- genDFTTests conf
    defaultMainWithArgs (tests ++ [dftTests]) args'

tests :: [Test]
tests = [strideTest, l82Test, kroneckerTest, directSumTest, dftTests]

-- See:
--   https://en.wikipedia.org/wiki/Kronecker_product
strideTest :: Test
strideTest = testCase "Stride matrix L^8_4" $
    toMatrix (Pi (L 8 4)) @?= a
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

l82Test :: Test
l82Test = testCase "Stride matrix L^8_2" $
    toMatrix (Pi (L 8 2)) @?= a
  where
    a :: Matrix M Int
    a = matrix [[1, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 1, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 1, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 1, 0],
                [0, 1, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 1, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 1, 0, 0],
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
    [ f2Test, f4Test, f8Test
    , ck_5_7_test
    , gt_5_7_test
    , rader_test 7, rader_test 23
    , testProperty "DFT" prop_DFT]

-- | Test that 'FFT.f' produces correct DFT matrices.
prop_DFT :: SmallPowerOfTwo -> Property
prop_DFT (SmallPowerOfTwo n) = toMatrix (DFT (2^n)) === toMatrix fft_spl
  where
    fft_spl :: SPL (Exp (Complex Double))
    fft_spl = FFT.f (2^n)

-- $F_2$
f2Test :: Test
f2Test = testCase "F_2" $ toMatrix (FFT.f 2) @?= f2
  where
    f2 :: Matrix M (Exp (Complex Double))
    f2 = matrix [[1,  1],
                 [1, -1]]

-- $F_4$ calculated per "SPL: A Language and Compiler for DSP Algorithms"
f4Test :: Test
f4Test = testCase "F_4" $ toMatrix (FFT.f 4) @?= f4
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
f8Test = testCase "F_8" $ toMatrix (FFT.f 8) @?= f8
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

-- Test Cooley-Tukey with factors 5 and 7
ck_5_7_test :: Test
ck_5_7_test = testCase "CooleyTukey(5,7)" $
    toMatrix (cooleyTukey 5 7 w) @?= toMatrix (RDFT 35 w)
  where
    w :: Exp (Complex Double)
    w = FFT.omega (35 :: Int)

-- Test Good-Thomas with factors 5 and 7
gt_5_7_test :: Test
gt_5_7_test = testCase "GoodThomas(5,7)" $
    toMatrix (goodThomas 5 7 w) @?= toMatrix (RDFT 35 w)
  where
    w :: Exp (Complex Double)
    w = FFT.omega (35 :: Int)

-- Test Rader for given prime
rader_test :: Int -> Test
rader_test n = testCase ("Rader(" ++ show n ++ ")") $
    toMatrix (rader (fromIntegral n) w) @?= toMatrix (RDFT n w)
  where
    w :: Exp (Complex Double)
    w = FFT.omega (n :: Int)

genDFTTests :: Config -> IO Test
genDFTTests conf =
    testGroup "Generated DFT" <$>
    mapM (\i -> dftTest conf (2^i)) [1..9::Int]

dftTest :: Config -> Int -> IO Test
dftTest conf n = do
    dft <- genComplexTransform conf ("dft" ++ show n) (FFT.f n)
    return $
      testProperty ("Generated DFT of size " ++ show n) $
      forAll (vectorsOfSize n) $ \v -> epsDiff (dft v) (FFTW.fft n v)

-- | Generate vectors of a given size.
vectorsOfSize :: (Arbitrary a, V.Storable a)
              => Int
              -> Gen (V.Vector a)
vectorsOfSize n = V.fromList <$> replicateM n arbitrary

-- | Return 'True' if the maximum pairwise difference between two vectors is
-- less than epsilon.
epsDiff :: forall a . (Show a, RealFloat a, V.Storable a)
        => V.Vector (Complex a)
        -> V.Vector (Complex a)
        -> Property
epsDiff v1 v2 =
    counterexample ("Max delta: " ++ show maxDelta) $
    maxDelta < eps
  where
    maxDelta :: a
    maxDelta = V.maximum $ V.zipWith (\x y -> magnitude (x - y)) v1 v2

    eps :: a
    eps = 1e-7

-- | Manifest a flattened array of complex numbers so that we can easily compare
-- it.
manifestComplex :: (IArray r DIM2 (Exp (Complex Double)))
                => Matrix r (Exp (Complex Double))
                -> Matrix M (Exp (Complex Double))
manifestComplex = fmap f . manifest
  where
    f :: Exp a -> Exp a
    f (ConstE c) = ConstE (flatten c)
    f e          = e

-- | A small number
newtype SmallPowerOfTwo = SmallPowerOfTwo Int
    deriving (Eq, Ord, Show, Num, Integral, Real, Enum)

instance Arbitrary SmallPowerOfTwo where
    arbitrary = SmallPowerOfTwo <$> (choose (1, 7) :: Gen Int)

    shrink (SmallPowerOfTwo 0) = []
    shrink (SmallPowerOfTwo n) = [SmallPowerOfTwo (n-1)]
