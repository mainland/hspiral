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
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word32)
import GHC.TypeLits (KnownNat,
                     natVal)
import Test.HUnit ((@?=),
                   Assertion)
import Test.Hspec
import Test.Hspec.Core.Spec (Example(..),
                             Result)
import Test.QuickCheck ((===),
                        Arbitrary(..),
                        Gen,
                        Property,
                        counterexample,
                        forAll)
import Test.QuickCheck.Modifiers (Positive(..))

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
    splTests
    factorizationTests
    opCountTests
    codegenTests mempty

--
-- SPL operation test
--

splTests :: Spec
splTests = describe "SPL operations" $ do
    strideTest
    l82Test
    reverseIdentityTest
    colTest
    rowTest
    kroneckerTest
    directSumTest

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

reverseIdentityTest :: Spec
reverseIdentityTest = it "Reverse identity matrix J 5" $
    toMatrix (Pi (J 5)) @?= a
  where
    a :: Matrix M Int
    a = matrix [[0, 0, 0, 0, 1],
                [0, 0, 0, 1, 0],
                [0, 0, 1, 0, 0],
                [0, 1, 0, 0, 0],
                [1, 0, 0, 0, 0]]

colTest :: Spec
colTest = it "Extract column" $
    col a 0 @?= v
  where
    a :: Matrix M Int
    a = matrix [[0, 1],
                [2, 5],
                [5, 1]]

    v :: V.Vector Int
    v = V.fromList [0, 2, 5]

rowTest :: Spec
rowTest = it "Extract row" $
    row a 0 @?= v
  where
    a :: Matrix M Int
    a = matrix [[0, 1],
                [2, 5],
                [5, 1]]

    v :: V.Vector Int
    v = V.fromList [0, 1]

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

factorizationTests :: Spec
factorizationTests = describe "Factorization" $ do
    f2Test
    f4Test
    f8Test
    describe "Cooley-Tukey" $ do
        cooleyTukeyDITTest 5 7
        cooleyTukeyDIFTest 5 7
    describe "Good-Thomas" $
        goodThomasTest 5 7
    describe "Rader" $ do
        raderTest 7
        raderTest 23
    describe "Bluestein" $ do
        bluesteinTest 3 6
        bluesteinTest 4 8
        bluesteinTest 4 9
        bluesteinTest 9 18
    describe "DIT" $ sequence_ [ditTest n | n <- [1..7]]
    describe "DIF" $ sequence_ [difTest n | n <- [1..7]]
    describe "Split Radix" $ sequence_ [splitRadixTest n | n <- [1..3]]
    describe "Split Radix 8" $ sequence_ [splitRadix8Test n | n <- [1..2]]
    describe "Conjugate Pair Split Radix" $ sequence_ [conjPairSplitRadixTest n | n <- [1..3]]
    describe "Improved Split Radix" $ sequence_ [improvedSplitRadixTest n | n <- [1..3]]
    searchTests
    modularDFTTests

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

-- Test Cooley-Tukey DIT
cooleyTukeyDITTest :: Int -> Int -> Spec
cooleyTukeyDITTest r s =
    it ("CooleyTukeyDIT(" ++ show r ++ "," ++ show s ++ ")") $
    toMatrix (cooleyTukeyDIT r s w) @?= toMatrix (F rs w)
  where
    w :: Exp (Complex Double)
    w = omega rs

    rs :: Int
    rs = r*s

-- Test Cooley-Tukey DIF
cooleyTukeyDIFTest :: Int -> Int -> Spec
cooleyTukeyDIFTest r s =
    it ("CooleyTukeyDIF(" ++ show r ++ "," ++ show s ++ ")") $
    toMatrix (cooleyTukeyDIF r s w) @?= toMatrix (F rs w)
  where
    w :: Exp (Complex Double)
    w = omega rs

    rs :: Int
    rs = r*s

-- Test Good-Thomas with factors 5 and 7
goodThomasTest :: Int -> Int -> Spec
goodThomasTest r s =
    it ("GoodThomas(" ++ show r ++ "," ++ show s ++ ")") $
    toMatrix (goodThomas r s w) @?= toMatrix (F rs w)
  where
    w :: Exp (Complex Double)
    w = omega rs

    rs :: Int
    rs = r*s

-- Test Rader for given prime
raderTest :: Int -> Spec
raderTest n = it ("Rader(" ++ show n ++ ")") $
    toMatrix (rader (fromIntegral n) w) @?= toMatrix (F n w)
  where
    w :: Exp (Complex Double)
    w = omega n

-- Test Bluestein for given n and m
bluesteinTest :: Int -> Int -> Spec
bluesteinTest n m = it ("Bluestein(" ++ show n ++ "," ++ show m ++ ")") $
    toMatrix (bluestein n m w) @?= toMatrix (DFT n)
  where
    w :: Exp (Complex Double)
    w = omega (2*n)

-- DIT test
ditTest :: Int -> Spec
ditTest n = it ("DIT(2^" ++ show n ++ ")") $
    toMatrix fft_spl @?= toMatrix (DFT (2^n))
  where
    fft_spl :: SPL (Exp (Complex Double))
    fft_spl = dit (2^n)

-- DIF test
difTest :: Int -> Spec
difTest n = it ("DIF(2^" ++ show n ++ ")") $
    toMatrix fft_spl @?= toMatrix (DFT (2^n))
  where
    fft_spl :: SPL (Exp (Complex Double))
    fft_spl = dif (2^n)

-- Split radix test
splitRadixTest :: Int -> Spec
splitRadixTest n = it ("SplitRadix(4^" ++ show n ++ ")") $
    toMatrix (splitRadix (4^n) w) @?= toMatrix (DFT (4^n))
  where
    w :: Exp (Complex Double)
    w = omega (4^n)

-- Split radix 8 test
splitRadix8Test :: Int -> Spec
splitRadix8Test n = it ("SplitRadix(8^" ++ show n ++ ")") $
    toMatrix (splitRadix8 (8^n) w) @?= toMatrix (DFT (8^n))
  where
    w :: Exp (Complex Double)
    w = omega (8^n)

-- Split radix test
conjPairSplitRadixTest :: Int -> Spec
conjPairSplitRadixTest n = it ("ConjPairSplitRadix(4^" ++ show n ++ ")") $
    toMatrix (conjPairSplitRadix (4^n) w) @?= toMatrix (DFT (4^n))
  where
    w :: Exp (Complex Double)
    w = omega (4^n)

-- Split radix test
improvedSplitRadixTest :: Int -> Spec
improvedSplitRadixTest n = it ("ImprovedSplitRadix(4^" ++ show n ++ ")") $
    toMatrix (improvedSplitRadix (4^n) w) @?= toMatrix (DFT (4^n))
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

modularDFTTests :: Spec
modularDFTTests = describe "Modular DFT" $ do
    mkModularDFTTest (Proxy :: Proxy 17) 4
    mkModularDFTTest (Proxy :: Proxy 2013265921) 128

mkModularDFTTest :: forall p . KnownNat p
                 => Proxy p
                 -> Int
                 -> Spec
mkModularDFTTest proxy n = it ("F_" ++ show n ++ " in ℤ/" ++ show p) $
    toMatrix fft_spl @?= toMatrix (DFT n)
  where
    p :: Integer
    p = natVal proxy

    fft_spl :: SPL (Exp (ℤ/p))
    fft_spl = dit n

--
-- Operation count tests
--

opCountTests :: Spec
opCountTests =
    describe "Opcount" $ do
    splitRadixOpcountTests
    difOpcountTests

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

splitRadixOpcountTests :: Spec
splitRadixOpcountTests =
    describe "Split radix opcounts" $
    sequence_ [mkTest n (muls + adds) | (n, muls, adds) <- splitRadixOpcounts]
  where
    mkTest :: Int -> Int -> Spec
    mkTest n nops =
      mkOpCountTest ("Split radix " ++ show n) fs nops (runSearch () f (DFT n))
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

-- The DIF form should have the same operation count as split radix when the
-- DifRewrite flag is enabled.
difOpcountTests :: Spec
difOpcountTests =
    describe "DIF opcounts" $
    sequence_ [mkTest n (muls + adds) | (n, muls, adds) <- splitRadixOpcounts]
  where
    mkTest :: Int -> Int -> Spec
    mkTest n nops =
      mkOpCountTest ("DIF " ++ show n) fs nops (return $ dif n)
      where
        fs :: [DynFlag]
        fs = [ StoreIntermediate
             , SplitComplex
             , CSE
             , Rewrite
             , DifRewrite
             ]

mkOpCountTest :: String
              -> [DynFlag]
              -> Int
              -> Spiral (SPL (Exp (Complex Double)))
              -> Spec
mkOpCountTest desc fs nops gen =
    it desc $ do
    ops <- runSpiralWith mempty $ withOpcountFlags fs $ do
           f   <- gen
           toProgram "f" (Re f) >>= countProgramOps
    return $ mulOps ops + addOps ops @?= nops

withOpcountFlags :: MonadConfig m => [DynFlag] -> m a -> m a
withOpcountFlags fs =
    localConfig $ \env -> env { dynFlags  = FS.fromList fs
                              , maxUnroll = 256
                              }

--
-- Code generation tests
--

codegenTests :: Config -> Spec
codegenTests conf =
    describe "Code Generation Tests" $ do
    ditCodegenTests conf
    difCodegenTests conf
    splitRadixCodegenTests conf
    improvedSplitRadixCodegenTests conf
    searchCodegenTests conf
    modCodegenTests conf

ditCodegenTests :: Config -> Spec
ditCodegenTests conf =
    describe "Generated DIT" $
    mkCodegenTests conf "Generated DIT DFT of size" (return . Re . dit)

difCodegenTests :: Config -> Spec
difCodegenTests conf =
    describe "Generated DIF" $
    mkCodegenTests conf "Generated DIF DFT of size" (return . Re . dif)

splitRadixCodegenTests :: Config -> Spec
splitRadixCodegenTests conf =
    describe "Generated splitRadix DFT" $
    mkCodegenTests conf "Split radix DFT of size" (runSearch () f . Re . DFT)
  where
    f :: (Typeable a, Typed a, MonadSpiral m)
      => SPL (Exp a)
      -> S s m (SPL (Exp a))
    f (F n w) = splitRadixBreakdown n w
    f _       = mzero

improvedSplitRadixCodegenTests :: Config -> Spec
improvedSplitRadixCodegenTests conf =
    describe "Generated improved split radix DFT" $
    mkCodegenTests conf "Improved split radix DFT of size" (runSearch () f . Re . DFT)
  where
    f :: (Typeable a, Typed a, MonadSpiral m)
      => SPL (Exp a)
      -> S s m (SPL (Exp a))
    f (F n w) = improvedSplitRadixBreakdown n w
    f _       = mzero

searchCodegenTests :: Config -> Spec
searchCodegenTests conf =
    describe "Generated opcount-optimized DFT" $
    mkCodegenTests conf "Opcount-optimized DFT of size" (searchOpCount . Re . DFT)

instance Example (IO Property) where
    type Arg (IO Property) = ()
    evaluateExample mp c action progressCallback = do
        p <- mp
        evaluateExample p c action progressCallback

instance Example (IO Assertion) where
    type Arg (IO Assertion) = ()
    evaluateExample ma c action progressCallback = do
        a <- ma
        evaluateExample a c action progressCallback

instance Example ((Property -> IO Result) -> IO Result) where
    type Arg ((Property -> IO Result) -> IO Result) = ()
    evaluateExample k c action progressCallback =
        k $ \p -> evaluateExample p c action progressCallback

mkCodegenTests :: Config
               -> String
               -> (Int -> Spiral (SPL (Exp Double)))
               -> Spec
mkCodegenTests conf desc f =
    sequence_ [dftTest (2^i) | i <- [1..9::Int]]
  where
    dftTest :: Int -> Spec
    dftTest n = it (desc ++ " " ++ show n) $ \(k :: Property -> IO Result) -> do
        Re e <- runSpiralWith mempty $ f n
        withComplexTransform conf ("dft" ++ show n) e $ \dft ->
          k $ forAll (uniformVectorsOfSize n) $ \v -> epsDiff (dft v) (FFTW.fft n v)

modCodegenTests :: Config
                -> Spec
modCodegenTests conf =
    describe "Generated modular DFT" $ do
    mkModularDFTCodegenTest (Proxy :: Proxy 17) conf "DIT" dit 4
    mkModularDFTCodegenTest (Proxy :: Proxy 2013265921) conf "DIT" dit 12

mkModularDFTCodegenTest :: forall p . KnownNat p
                        => Proxy p
                        -> Config
                        -> String
                        -> (Int -> SPL (Exp (ℤ/p)))
                        -> Int
                        -> Spec
mkModularDFTCodegenTest proxy conf desc mkSPL n_max =
    describe ("Modular DFT (p=" ++ show (natVal proxy) ++ ")") $
    sequence_ [test n | n <- [1..n_max::Int]]
  where
    p :: Integer
    p = natVal proxy

    test :: Int -> Spec
    test n = it (desc ++ "(2^" ++ show n ++ ")") $ \(k :: Property -> IO Result) ->
        withModularTransform conf ("moddft_" ++ show p ++ "_" ++ show n) fft_spl $ \moddft ->
        -- We must evaluate the result now because moddft is dynamically loaded code
        -- that cannot escape the scope of withModularTransform
        k $ forAll basis $ \i -> moddft (e_i i) === res i
      where
        basis :: Gen Int
        basis = do
            Positive x <- arbitrary
            return $ x `mod` (2^n)

        fft_spl :: SPL (Exp (ℤ/p))
        fft_spl = mkSPL (2^n)

        len :: Int
        len = 2^n

        e_i :: Int -> VS.Vector (ℤ/p)
        e_i i = VS.generate len $ \j -> if i == j then 1 else 0

        res :: Int -> VS.Vector (ℤ/p)
        res i = VS.generate len $ \j -> a^j
          where
            a :: ℤ/p
            a = w^i

        w :: ℤ/p
        w = omega len

-- | Generate vectors of a given size with uniformly distributed elements.
uniformVectorsOfSize :: (Arbitrary (Uniform01 a), VS.Storable a)
                     => Int
                     -> Gen (VS.Vector a)
uniformVectorsOfSize n = do
    xs <- replicateM n arbitrary
    return $ VS.fromList (map unUniform01 xs)

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
epsDiff :: forall a . (Show a, RealFloat a, VS.Storable a)
        => VS.Vector (Complex a)
        -> VS.Vector (Complex a)
        -> Property
epsDiff v1 v2 =
    counterexample ("Max delta: " ++ show maxDelta) $
    maxDelta < eps
  where
    maxDelta :: a
    maxDelta = VS.maximum $ VS.zipWith (\x y -> magnitude (x - y)) v1 v2

    eps :: a
    eps = 1e-12
