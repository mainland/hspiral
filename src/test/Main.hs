{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Test
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Main (main) where

import Control.Monad (mzero,
                      replicateM)
import Data.Complex
import Data.Maybe (catMaybes)
import Data.Modular
import Data.Typeable (Typeable)
import qualified Data.Vector.Storable as V
import Data.Word (Word32)
import System.Environment (getArgs)
import Test.Framework (Test,
                       buildTest,
                       defaultMainWithOpts,
                       optionsDescription,
                       testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck (Arbitrary(..),
                        Gen,
                        Property,
                        forAll,
                        counterexample)

import qualified Data.FlagSet as FS

import qualified Spiral
import Spiral (Config(..))
import Spiral.Array
import Spiral.Config
import Spiral.Driver
import Spiral.Driver.Opts (parseOpts')
import Spiral.Exp
import Spiral.FFT.Bluestein
import Spiral.FFT.CooleyTukey
import Spiral.FFT.GoodThomas
import Spiral.FFT.Rader
import Spiral.Monad
import Spiral.NumberTheory
import Spiral.OpCount
import Spiral.RootOfUnity
import Spiral.SPL
import Spiral.SPL.Run
import Spiral.Search
import Spiral.Search.FFTBreakdowns
import Spiral.Search.OpCount

import qualified Test.FFTW as FFTW
import Test.Gen

main :: IO ()
main = do
    setGenerator 17 3
    setGenerator 2013265921 31
    (conf, opts, _args) <- getArgs >>= \args -> parseOpts' args optionsDescription
    defaultMainWithOpts (tests conf) (mconcat (catMaybes opts))
  where
    tests :: Config -> [Test]
    tests conf = [ strideTest
                 , l82Test
                 , kroneckerTest
                 , directSumTest
                 , dftTests
                 , moddftTests
                 , searchTests
                 , splitRadixOpcountTests
                 , difOpcountTests
                 , ditCodegenTests conf
                 , difCodegenTests conf
                 , splitRadixCodegenTests conf
                 , improvedSplitRadixCodegenTests conf
                 , searchCodegenTests conf
                 ]

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
    , testGroup "Split Radix 8" [split_radix8_test n | n <- [1..2]]
    , testGroup "Conjugate Pair Split Radix" [conj_pair_split_radix_test n | n <- [1..3]]
    , testGroup "Improved Split Radix" [improved_split_radix_test n | n <- [1..3]]
    ]

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

-- Split radix test
conj_pair_split_radix_test :: Int -> Test
conj_pair_split_radix_test n = testCase ("ConjPairSplitRadix(4^" ++ show n ++ ")") $
    toMatrix (conjPairSplitRadix (4^n) w) @?= toMatrix (DFT (4^n))
  where
    w :: Exp (Complex Double)
    w = omega (4^n)

-- Split radix test
improved_split_radix_test :: Int -> Test
improved_split_radix_test n = testCase ("ImprovedSplitRadix(4^" ++ show n ++ ")") $
    toMatrix (conjPairSplitRadix (4^n) w) @?= toMatrix (DFT (4^n))
  where
    w :: Exp (Complex Double)
    w = omega (4^n)

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

--
-- Modular DFT tests
--

moddftTests :: Test
moddftTests = testGroup "Modular DFT tests"
    [modf4Test_17, modf128Test_2013265921]

-- $F_4$
modf4Test_17 :: Test
modf4Test_17 = testCase "F_4 in ℤ/17" $
    toMatrix fft_spl @?= toMatrix (DFT 4)
  where
    fft_spl :: SPL (Exp (ℤ/17))
    fft_spl = dit 4

-- $F_64$
modf128Test_2013265921 :: Test
modf128Test_2013265921 = testCase "F_128 in ℤ/2013265921" $
    toMatrix fft_spl @?= toMatrix (DFT 128)
  where
    fft_spl :: SPL (Exp (ℤ/2013265921))
    fft_spl = dit 128

ditCodegenTests :: Config -> Test
ditCodegenTests conf =
    testGroup "Generated DIT" $
    codegenTests conf "Generated DIT DFT of size" (\n -> return (Re (dit n)))

difCodegenTests :: Config -> Test
difCodegenTests conf =
    testGroup "Generated DIF" $
    codegenTests conf "Generated DIF DFT of size" (\n -> return (Re (dif n)))

splitRadixCodegenTests :: Config -> Test
splitRadixCodegenTests conf =
    testGroup "Generated splitRadix DFT" $
    codegenTests conf "Split radix DFT of size" (\n -> runSearch () f (Re (DFT n)))
  where
    f :: (Typeable a, Typed a, MonadSpiral m)
      => SPL (Exp a)
      -> S s m (SPL (Exp a))
    f (F n w) = splitRadixBreakdown n w
    f _       = mzero

improvedSplitRadixCodegenTests :: Config -> Test
improvedSplitRadixCodegenTests conf =
    testGroup "Generated improved split radix DFT" $
    codegenTests conf "Improved split radix DFT of size" (\n -> runSearch () f (Re (DFT n)))
  where
    f :: (Typeable a, Typed a, MonadSpiral m)
      => SPL (Exp a)
      -> S s m (SPL (Exp a))
    f (F n w) = improvedSplitRadixBreakdown n w
    f _       = mzero

searchCodegenTests :: Config -> Test
searchCodegenTests conf =
    testGroup "Generated opcount-optimized DFT" $
    codegenTests conf "Opcount-optimized DFT of size" (\n -> searchOpCount (Re (DFT n)))

codegenTests :: Config
             -> String
             -> (Int -> Spiral (SPL (Exp Double)))
             -> [Test]
codegenTests conf desc f =
    map (dftTest conf) [2^i | i <- [1..9::Int]]
  where
    dftTest :: Config -> Int -> Test
    dftTest conf n = buildTest $ do
        Re e <- Spiral.defaultMain $ \_ -> f n
        dft  <- genComplexTransform conf ("dft" ++ show n) e
        return $
            testProperty (desc ++ " " ++ show n) $
            forAll (uniformVectorsOfSize n) $ \v -> epsDiff (dft v) (FFTW.fft n v)

-- | Generate vectors of a given size with uniformly distributed elements.
uniformVectorsOfSize :: (Arbitrary (Uniform01 a), V.Storable a)
                     => Int
                     -> Gen (V.Vector a)
uniformVectorsOfSize n = do
    xs <- replicateM n arbitrary
    return $ V.fromList (map unUniform01 xs)

-- | A uniform value i the range [0,1]
newtype Uniform01 a = Uniform01 { unUniform01 :: a }
  deriving (Eq, Ord, Show)

instance Arbitrary (Uniform01 a) => Arbitrary (Uniform01 (Complex a)) where
    arbitrary = do
        Uniform01 r <- arbitrary
        Uniform01 i <- arbitrary
        return $ Uniform01 $ r :+ i

instance Arbitrary (Uniform01 Double) where
    -- This code take from the dsp Haskell library, authored by Matt Donadio.
    -- 53 bits in [0,1], i.e., 64-bit IEEE 754 in [0,1]
    -- 67108864 = 2^26
    -- 9007199254740991 = 2^53 - 1
    arbitrary = do
        u1 :: Word32 <- arbitrary
        u2 :: Word32 <- arbitrary
        -- 27 bits
        let a = fromIntegral u1 / 32.0
        -- 26 bits
        let b = fromIntegral u2 / 64.0
        return $ Uniform01 $ (a * 67108864.0 + b) / 9007199254740991.0

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
    eps = 1e-12

splitRadixOpcountTests :: Test
splitRadixOpcountTests =
    testGroup "Split radix opcounts"
    [ mkTest n (muls + adds) | (n, muls, adds) <- splitRadixOpcounts]
  where
    mkTest :: Int -> Int -> Test
    mkTest n nops =
      opCountTest ("Split radix " ++ show n) fs nops (runSearch () f (DFT n))
      where
        fs :: [DynFlag]
        fs = [ StoreIntermediate
             , SplitComplex
             , CSE
             , Rewrite
             ]

    f :: (Typeable a, Typed a, MonadSpiral m)
      => SPL (Exp a)
      -> S s m (SPL (Exp a))
    f (F n w) = splitRadixBreakdown n w
    f _       = mzero

-- The number of multiplies and additions for spit radix decomposition of size n
-- when using three-multiply form of complex multiply. Taken from Table II of
-- Heideman and Burrus.
splitRadixOpcounts :: [(Int, Int, Int)]
splitRadixOpcounts = [ (4,    0,     16)
                     , (8,    4,     52)
                     , (16,   20,    148)
                     , (32,   68,    388)
                     , (64,   196,   964)
                     , (128,  516,   2308)
                     , (256,  1284,  5380)
                     --, (512,  3076,  12292)
                     --, (1024, 7172,  27652)
                     --, (2048, 16388, 61444)
                     --, (4096, 36868, 135172)
                     ]

difOpcountTests :: Test
difOpcountTests =
    testGroup "DIF opcounts"
    [ mkTest n (muls + adds) | (n, muls, adds) <- splitRadixOpcounts]
  where
    mkTest :: Int -> Int -> Test
    mkTest n nops =
      opCountTest ("DIF " ++ show n) fs nops (return $ dif n)
      where
        fs :: [DynFlag]
        fs = [ StoreIntermediate
             , SplitComplex
             , CSE
             , Rewrite
             , DifRewrite
             ]

withOpcountFlags :: MonadConfig m => [DynFlag] -> m a -> m a
withOpcountFlags fs =
    localConfig $ \env -> env { dynFlags  = FS.fromList fs
                              , maxUnroll = 256
                              }

opCountTest :: String
            -> [DynFlag]
            -> Int
            -> Spiral (SPL (Exp (Complex Double)))
            -> Test
opCountTest desc fs nops gen = buildTest $ do
    ops <- Spiral.defaultMain $ \_ -> withOpcountFlags fs $ do
           f <- gen
           toProgram "f" (Re f) >>= countProgramOps
    return $ testCase desc $
             mulOps ops + addOps ops @?= nops
