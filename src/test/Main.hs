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
import Control.Monad.IO.Class (liftIO)
import Data.Complex
import Data.Typeable (Typeable)
import qualified Data.Vector.Storable as V
import System.Environment (getArgs)
import Test.Framework (Test,
                       buildTest,
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

import qualified Spiral
import Spiral (Config(..),
               parseOpts)
import Spiral.Array
import Spiral.Exp
import Spiral.FFT.Bluestein
import Spiral.FFT.CooleyTukey
import Spiral.FFT.GoodThomas
import Spiral.FFT.Rader
import Spiral.Monad
import Spiral.RootOfUnity
import Spiral.SPL
import Spiral.Search.Generic
import Spiral.Search.Monad
import Spiral.Search.OpCount

import qualified Test.FFTW as FFTW
import Test.Gen

main :: IO ()
main = do
    (conf, args') <- getArgs >>= parseOpts
    defaultMainWithArgs (tests ++
      [ searchTests
      , ditCodegenTests conf
      , difCodegenTests conf
      , splitRadixCodegenTests conf
      , searchCodegenTests conf
      ])
      args'

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
    , ck_dit_5_7_test
    , ck_dif_5_7_test
    , gt_5_7_test
    , rader_test 7, rader_test 23
    , bluestein_test 3 6, bluestein_test 4 8, bluestein_test 4 9, bluestein_test 9 18
    , testGroup "DIT" [dit_test n | n <- [1..7]]
    , testGroup "DIF" [dif_test n | n <- [1..7]]
    , testGroup "Split Radix" [split_radix_test n | n <- [1..3]]
    , testGroup "Split Radix 8" [split_radix8_test n | n <- [1..2]]]
--    , testProperty "DFT" prop_DFT]

-- | Test that 'dit' produces correct DFT matrices.
prop_DFT :: SmallPowerOfTwo -> Property
prop_DFT (SmallPowerOfTwo n) = toMatrix (DFT (2^n)) === toMatrix fft_spl
  where
    fft_spl :: SPL (Exp (Complex Double))
    fft_spl = dit (2^n)

-- $F_2$
f2Test :: Test
f2Test = testCase "F_2" $ toMatrix (dit 2) @?= f2
  where
    f2 :: Matrix M (Exp (Complex Double))
    f2 = matrix [[1,  1],
                 [1, -1]]

-- $F_4$ calculated per "SPL: A Language and Compiler for DSP Algorithms"
f4Test :: Test
f4Test = testCase "F_4" $ toMatrix (dit 4) @?= f4
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
f8Test = testCase "F_8" $ toMatrix (dit 8) @?= f8
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

        w = omega 8

-- DIT test
dit_test :: Int -> Test
dit_test n = testCase ("DIT(2^" ++ show n ++ ")") $
    toMatrix fft_spl @?= toMatrix (DFT (2^n))
  where
    fft_spl :: SPL (Exp (Complex Double))
    fft_spl = dit (2^n)

-- DIF test
dif_test :: Int -> Test
dif_test n = testCase ("DIF(2^" ++ show n ++ ")") $
    toMatrix fft_spl @?= toMatrix (DFT (2^n))
  where
    fft_spl :: SPL (Exp (Complex Double))
    fft_spl = dif (2^n)

-- Split radix test
split_radix_test :: Int -> Test
split_radix_test n = testCase ("SplitRadix(4^" ++ show n ++ ")") $
    toMatrix (splitRadix (4^n) w) @?= toMatrix (DFT (4^n))
  where
    w :: Exp (Complex Double)
    w = omega (4^n)

-- Split radix 8 test
split_radix8_test :: Int -> Test
split_radix8_test n = testCase ("SplitRadix(8^" ++ show n ++ ")") $
    toMatrix (splitRadix8 (8^n) w) @?= toMatrix (DFT (8^n))
  where
    w :: Exp (Complex Double)
    w = omega (8^n)

-- Test Cooley-Tukey with factors 5 and 7
ck_dit_5_7_test :: Test
ck_dit_5_7_test = testCase "CooleyTukeyDIT(5,7)" $
    toMatrix (cooleyTukeyDIT 5 7 w) @?= toMatrix (F 35 w)
  where
    w :: Exp (Complex Double)
    w = omega 35

ck_dif_5_7_test :: Test
ck_dif_5_7_test = testCase "CooleyTukeyDIF(5,7)" $
    toMatrix (cooleyTukeyDIF 5 7 w) @?= toMatrix (F 35 w)
  where
    w :: Exp (Complex Double)
    w = omega 35

-- Test Good-Thomas with factors 5 and 7
gt_5_7_test :: Test
gt_5_7_test = testCase "GoodThomas(5,7)" $
    toMatrix (goodThomas 5 7 w) @?= toMatrix (F 35 w)
  where
    w :: Exp (Complex Double)
    w = omega 35

-- Test Rader for given prime
rader_test :: Int -> Test
rader_test n = testCase ("Rader(" ++ show n ++ ")") $
    toMatrix (rader (fromIntegral n) w) @?= toMatrix (F n w)
  where
    w :: Exp (Complex Double)
    w = omega n

-- Test Bluestein for given n and m
bluestein_test :: Int -> Int -> Test
bluestein_test n m = testCase ("Bluestein(" ++ show n ++ "," ++ show m ++ ")") $
    toMatrix (bluestein n m w) @?= toMatrix (DFT n)
  where
    w :: Exp (Complex Double)
    w = omega (2*n)

searchTests :: Test
searchTests =
    testGroup "Opcount-optimized DFT" $
    map dftTest [2^i | i <- [1..6::Int]]
  where
    dftTest :: Int -> Test
    dftTest n = testCase ("OpcountSearch(" ++ show n ++ ")") $ do
        Re e <- Spiral.defaultMain $ \_ -> searchOpCount (Re (DFT n) :: SPL (Exp Double))
        toMatrix e @?= toMatrix (DFT n)

ditCodegenTests :: Config -> Test
ditCodegenTests conf =
    testGroup "Generated DIT" $
    map (\i -> dftTest conf (2^i)) [1..9::Int]
  where
    dftTest :: Config -> Int -> Test
    dftTest conf n = buildTest $ do
        dft <- liftIO $ genComplexTransform conf ("dft" ++ show n) (dit n)
        return $
            testProperty ("Generated DIT DFT of size " ++ show n) $
            forAll (vectorsOfSize n) $ \v -> epsDiff (dft v) (FFTW.fft n v)

difCodegenTests :: Config -> Test
difCodegenTests conf =
    testGroup "Generated DIF" $
    map (\i -> dftTest conf (2^i)) [1..9::Int]
  where
    dftTest :: Config -> Int -> Test
    dftTest conf n = buildTest $ do
        dft <- liftIO $ genComplexTransform conf ("dft" ++ show n) (dif n)
        return $
            testProperty ("Generated DIF DFT of size " ++ show n) $
            forAll (vectorsOfSize n) $ \v -> epsDiff (dft v) (FFTW.fft n v)

splitRadixCodegenTests :: Config -> Test
splitRadixCodegenTests conf =
    testGroup "Generated splitRadix DFT" $
    map (dftTest conf) [2^i | i <- [1..9::Int]]
  where
    dftTest :: Config -> Int -> Test
    dftTest conf n = buildTest $ do
        Re e <- Spiral.defaultMain $ \_ -> runSearch f (Re (DFT n) :: SPL (Exp Double))
        dft  <- genComplexTransform conf ("dftsplitradix" ++ show n) e
        return $
            testProperty ("Split radix DFT of size " ++ show n) $
            forAll (vectorsOfSize n) $ \v -> epsDiff (dft v) (FFTW.fft n v)

    f :: (Typeable a, Typed a, RootOfUnity (Exp a), MonadSpiral m)
      => Int
      -> Exp a
      -> S m (SPL (Exp a))
    f n w = search f (splitRadix n w)

searchCodegenTests :: Config -> Test
searchCodegenTests conf =
    testGroup "Generated opcount-optimized DFT" $
    map (dftTest conf) [2^i | i <- [1..9::Int]]
  where
    dftTest :: Config -> Int -> Test
    dftTest conf n = buildTest $ do
        Re e <- Spiral.defaultMain $ \_ -> searchOpCount (Re (DFT n) :: SPL (Exp Double))
        dft  <- genComplexTransform conf ("dftop" ++ show n) e
        return $
            testProperty ("Opcount-optimized DFT of size " ++ show n) $
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
    f (ConstE c) = ConstE (lower c)
    f e          = e

-- | A small number
newtype SmallPowerOfTwo = SmallPowerOfTwo Int
    deriving (Eq, Ord, Show, Num, Integral, Real, Enum)

instance Arbitrary SmallPowerOfTwo where
    arbitrary = SmallPowerOfTwo <$> (choose (1, 7) :: Gen Int)

    shrink (SmallPowerOfTwo 0) = []
    shrink (SmallPowerOfTwo n) = [SmallPowerOfTwo (n-1)]
