{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Test
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Main (main) where

import Control.Monad (mzero,
                      replicateM)
import Data.Complex
import Data.Modular
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import qualified Data.Vector.Storable as V
import Data.Word (Word32)
import GHC.TypeLits (KnownNat,
                     Nat,
                     natVal)
import Test.HUnit ((@?=))
import Test.Hspec
import Test.Hspec.Core.Spec (Example(..),
                             Result)
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary(..),
                        Gen,
                        Property,
                        choose,
                        counterexample,
                        forAll,
                        ioProperty)

import qualified Data.FlagSet as FS

import Spiral (Config(..))
import Spiral.Array
import Spiral.Config
import Spiral.Driver
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
    hspec spec

spec :: Spec
spec = do
    strideTest
    l82Test
    kroneckerTest
    directSumTest
    dftTests
    searchTests
    moddftTests
    splitRadixOpcountTests
    difOpcountTests
    let conf = mempty
    ditCodegenTests conf
    difCodegenTests conf
    splitRadixCodegenTests conf
    improvedSplitRadixCodegenTests conf
    searchCodegenTests conf
    modCodegenTests conf

-- See:
--   https://en.wikipedia.org/wiki/Kronecker_product
strideTest :: Spec
strideTest = it "Stride matrix L^8_4" $
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

l82Test :: Spec
l82Test = it "Stride matrix L^8_2" $
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
kroneckerTest :: Spec
kroneckerTest = it "Kronecker product (⊗)" $
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
directSumTest :: Spec
directSumTest = it "Direct sum (⊕)" $
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

dftTests :: Spec
dftTests = describe "DFT tests" $ do
    f2Test
    f4Test
    f8Test
    ck_dit_5_7_test
    ck_dif_5_7_test
    gt_5_7_test
    rader_test 7
    rader_test 23
    bluestein_test 3 6
    bluestein_test 4 8
    bluestein_test 4 9
    bluestein_test 9 18
    describe "DIT" $ sequence_ [dit_test n | n <- [1..7]]
    describe "DIF" $ sequence_ [dif_test n | n <- [1..7]]
    describe "Split Radix" $ sequence_ [split_radix_test n | n <- [1..3]]
    describe "Split Radix 8" $ sequence_ [split_radix8_test n | n <- [1..2]]
    describe "Conjugate Pair Split Radix" $ sequence_ [conj_pair_split_radix_test n | n <- [1..3]]
    describe "Improved Split Radix" $ sequence_ [improved_split_radix_test n | n <- [1..3]]

-- $F_2$
f2Test :: Spec
f2Test = it "F_2" $ toMatrix (dit 2) @?= f2
  where
    f2 :: Matrix M (Exp (Complex Double))
    f2 = matrix [[1,  1],
                 [1, -1]]

-- $F_4$ calculated per "SPL: A Language and Compiler for DSP Algorithms"
f4Test :: Spec
f4Test = it "F_4" $ toMatrix (dit 4) @?= f4
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
f8Test :: Spec
f8Test = it "F_8" $ toMatrix (dit 8) @?= f8
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

-- Test Cooley-Tukey with factors 5 and 7
ck_dit_5_7_test :: Spec
ck_dit_5_7_test = it "CooleyTukeyDIT(5,7)" $
    toMatrix (cooleyTukeyDIT 5 7 w) @?= toMatrix (F 35 w)
  where
    w :: Exp (Complex Double)
    w = omega 35

ck_dif_5_7_test :: Spec
ck_dif_5_7_test = it "CooleyTukeyDIF(5,7)" $
    toMatrix (cooleyTukeyDIF 5 7 w) @?= toMatrix (F 35 w)
  where
    w :: Exp (Complex Double)
    w = omega 35

-- Test Good-Thomas with factors 5 and 7
gt_5_7_test :: Spec
gt_5_7_test = it "GoodThomas(5,7)" $
    toMatrix (goodThomas 5 7 w) @?= toMatrix (F 35 w)
  where
    w :: Exp (Complex Double)
    w = omega 35

-- Test Rader for given prime
rader_test :: Int -> Spec
rader_test n = it ("Rader(" ++ show n ++ ")") $
    toMatrix (rader (fromIntegral n) w) @?= toMatrix (F n w)
  where
    w :: Exp (Complex Double)
    w = omega n

-- Test Bluestein for given n and m
bluestein_test :: Int -> Int -> Spec
bluestein_test n m = it ("Bluestein(" ++ show n ++ "," ++ show m ++ ")") $
    toMatrix (bluestein n m w) @?= toMatrix (DFT n)
  where
    w :: Exp (Complex Double)
    w = omega (2*n)

-- DIT test
dit_test :: Int -> Spec
dit_test n = it ("DIT(2^" ++ show n ++ ")") $
    toMatrix fft_spl @?= toMatrix (DFT (2^n))
  where
    fft_spl :: SPL (Exp (Complex Double))
    fft_spl = dit (2^n)

-- DIF test
dif_test :: Int -> Spec
dif_test n = it ("DIF(2^" ++ show n ++ ")") $
    toMatrix fft_spl @?= toMatrix (DFT (2^n))
  where
    fft_spl :: SPL (Exp (Complex Double))
    fft_spl = dif (2^n)

-- Split radix test
split_radix_test :: Int -> Spec
split_radix_test n = it ("SplitRadix(4^" ++ show n ++ ")") $
    toMatrix (splitRadix (4^n) w) @?= toMatrix (DFT (4^n))
  where
    w :: Exp (Complex Double)
    w = omega (4^n)

-- Split radix 8 test
split_radix8_test :: Int -> Spec
split_radix8_test n = it ("SplitRadix(8^" ++ show n ++ ")") $
    toMatrix (splitRadix8 (8^n) w) @?= toMatrix (DFT (8^n))
  where
    w :: Exp (Complex Double)
    w = omega (8^n)

-- Split radix test
conj_pair_split_radix_test :: Int -> Spec
conj_pair_split_radix_test n = it ("ConjPairSplitRadix(4^" ++ show n ++ ")") $
    toMatrix (conjPairSplitRadix (4^n) w) @?= toMatrix (DFT (4^n))
  where
    w :: Exp (Complex Double)
    w = omega (4^n)

-- Split radix test
improved_split_radix_test :: Int -> Spec
improved_split_radix_test n = it ("ImprovedSplitRadix(4^" ++ show n ++ ")") $
    toMatrix (conjPairSplitRadix (4^n) w) @?= toMatrix (DFT (4^n))
  where
    w :: Exp (Complex Double)
    w = omega (4^n)

searchTests :: Spec
searchTests =
    describe "Opcount-optimized DFT" $
    sequence_ [dftTest (2^i) | i <- [1..6::Int]]
  where
    dftTest :: Int -> Spec
    dftTest n = it ("OpcountSearch(" ++ show n ++ ")") $ do
        Re e <- runSpiralWith mempty $
                searchOpCount (Re (DFT n) :: SPL (Exp Double))
        toMatrix e @?= toMatrix (DFT n)

--
-- Modular DFT tests
--

moddftTests :: Spec
moddftTests = describe "Modular DFT tests" $ do
    modf4Test_17
    modf128Test_2013265921

-- $F_4$
modf4Test_17 :: Spec
modf4Test_17 = it "F_4 in ℤ/17" $
    toMatrix fft_spl @?= toMatrix (DFT 4)
  where
    fft_spl :: SPL (Exp (ℤ/17))
    fft_spl = dit 4

-- $F_64$
modf128Test_2013265921 :: Spec
modf128Test_2013265921 = it "F_128 in ℤ/2013265921" $
    toMatrix fft_spl @?= toMatrix (DFT 128)
  where
    fft_spl :: SPL (Exp (ℤ/2013265921))
    fft_spl = dit 128

splitRadixOpcountTests :: Spec
splitRadixOpcountTests =
    describe "Split radix opcounts" $
    sequence_ [mkTest n (muls + adds) | (n, muls, adds) <- splitRadixOpcounts]
  where
    mkTest :: Int -> Int -> Spec
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

difOpcountTests :: Spec
difOpcountTests =
    describe "DIF opcounts" $
    sequence_ [mkTest n (muls + adds) | (n, muls, adds) <- splitRadixOpcounts]
  where
    mkTest :: Int -> Int -> Spec
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
            -> Spec
opCountTest desc fs nops gen = do
    ops <- runIO $ runSpiralWith mempty $ withOpcountFlags fs $ do
           f <- gen
           toProgram "f" (Re f) >>= countProgramOps
    it desc $ mulOps ops + addOps ops @?= nops
    return ()

ditCodegenTests :: Config -> Spec
ditCodegenTests conf =
    describe "Generated DIT" $
    codegenTests conf "Generated DIT DFT of size" (\n -> return (Re (dit n)))

difCodegenTests :: Config -> Spec
difCodegenTests conf =
    describe "Generated DIF" $
    codegenTests conf "Generated DIF DFT of size" (\n -> return (Re (dif n)))

splitRadixCodegenTests :: Config -> Spec
splitRadixCodegenTests conf =
    describe "Generated splitRadix DFT" $
    codegenTests conf "Split radix DFT of size" (\n -> runSearch () f (Re (DFT n)))
  where
    f :: (Typeable a, Typed a, MonadSpiral m)
      => SPL (Exp a)
      -> S s m (SPL (Exp a))
    f (F n w) = splitRadixBreakdown n w
    f _       = mzero

improvedSplitRadixCodegenTests :: Config -> Spec
improvedSplitRadixCodegenTests conf =
    xdescribe "Generated improved split radix DFT" $
    codegenTests conf "Improved split radix DFT of size" (\n -> runSearch () f (Re (DFT n)))
  where
    f :: (Typeable a, Typed a, MonadSpiral m)
      => SPL (Exp a)
      -> S s m (SPL (Exp a))
    f (F n w) = improvedSplitRadixBreakdown n w
    f _       = mzero

searchCodegenTests :: Config -> Spec
searchCodegenTests conf =
    describe "Generated opcount-optimized DFT" $
    codegenTests conf "Opcount-optimized DFT of size" (\n -> searchOpCount (Re (DFT n)))

instance Example (IO Property) where
    type Arg (IO Property) = ()
    evaluateExample mp c action progressCallback = do
        p <- mp
        evaluateExample p c action progressCallback

instance Example ((Property -> IO Result) -> IO Result) where
    type Arg ((Property -> IO Result) -> IO Result) = ()
    evaluateExample k c action progressCallback =
        k $ \p -> evaluateExample p c action progressCallback

codegenTests :: Config
             -> String
             -> (Int -> Spiral (SPL (Exp Double)))
             -> Spec
codegenTests conf desc f =
    sequence_ [dftTest (2^i) | i <- [1..9::Int]]
  where
    dftTest :: Int -> Spec
    dftTest n = it (desc ++ " " ++ show n) $ \(k :: Property -> IO Result) -> do
        Re e <- runSpiralWith mempty $ f n
        withComplexTransform conf ("dft" ++ show n) e $ \dft ->
          k $ forAll (uniformVectorsOfSize n) $ \v -> epsDiff (dft v) (FFTW.fft n v)

data ModTest (p :: Nat) = ModTest Int Int
  deriving (Eq, Ord, Show)

instance Arbitrary (ModTest 2013265921) where
    arbitrary = do
        n <- choose (0, 12)
        i <- choose (0, 2^n-1)
        return $ ModTest n i

instance Arbitrary (ModTest 17) where
    arbitrary = do
        n <- choose (0, 4)
        i <- choose (0, 2^n-1)
        return $ ModTest n i

moddft17Test :: Config -> Spec
moddft17Test conf = prop "Generated ModDFT (p = 17)"
    (modDFTTest conf :: ModTest 17 -> Property)

moddft2013265921Test :: Config -> Spec
moddft2013265921Test conf = prop "Generated ModDFT (p = 2013265921)"
    (modDFTTest conf :: ModTest 2013265921 -> Property)

modDFTTest :: forall p . KnownNat p => Config -> ModTest p -> Property
modDFTTest conf (ModTest n i) = ioProperty $
    withModularTransform conf ("moddft_" ++ show p ++ "_" ++ show n) fft_spl $ \moddft ->
    -- We must evaluate the result now because moddft is dynamically loaded code
    -- that cannot escape the scope of withModularTransform
    return $! moddft e_i == res
  where
    fft_spl :: SPL (Exp (ℤ/p))
    fft_spl = dit (2^n)

    p :: Integer
    p = natVal (Proxy :: Proxy p)

    len :: Int
    len = 2^n

    e_i :: V.Vector (ℤ/p)
    e_i = V.generate len $ \j -> if i == j then 1 else 0

    res :: V.Vector (ℤ/p)
    res = V.generate len $ \j -> a^j
      where
        a :: ℤ/p
        a = w^i

    w :: ℤ/p
    w = omega len

modCodegenTests :: Config
                -> Spec
modCodegenTests conf =
    describe "Generated ModDFT" $ do
    moddft17Test conf
    moddft2013265921Test conf

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
