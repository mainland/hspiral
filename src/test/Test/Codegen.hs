{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Test.Factorization
-- Copyright   :  (c) 2016-2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Test.Codegen (
    codegenTests,
    ditCodegenTests,
    difCodegenTests,
    splitRadixCodegenTests,
    improvedSplitRadixCodegenTests,
    searchCodegenTests,
    modCodegenTests
  ) where

import Control.Monad (mzero,
                      replicateM)
import Data.Complex
import Data.Modular
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import qualified Data.Vector.Storable as VS
import Data.Word (Word32)
import GHC.TypeLits (KnownNat,
                     natVal)
import Test.Hspec
import Test.Hspec.Core.Spec (Result)
import Test.QuickCheck ((===),
                        Arbitrary(..),
                        Gen,
                        Property,
                        counterexample,
                        forAll)
import Test.QuickCheck.Modifiers (Positive(..))

import Spiral (Config(..))
import Spiral.Driver
import Spiral.Exp
import Spiral.FFT.CooleyTukey
import Spiral.Monad
import Spiral.RootOfUnity
import Spiral.SPL
import Spiral.Search
import Spiral.Search.FFTBreakdowns
import Spiral.Search.OpCount

import qualified Test.FFTW as FFTW
import Test.Gen
import Test.Instances ()

codegenTests :: Config -> Spec
codegenTests conf = do
    ditCodegenTests conf [2^i | i <- [1..9::Int]]
    difCodegenTests conf [2^i | i <- [1..9::Int]]
    splitRadixCodegenTests conf [2^i | i <- [1..9::Int]]
    improvedSplitRadixCodegenTests conf [2^i | i <- [1..9::Int]]
    searchCodegenTests conf [2^i | i <- [1..9::Int]]
    modCodegenTests conf

ditCodegenTests :: Config -> [Int] -> Spec
ditCodegenTests conf sizes =
    describe "Generated DIT" $
    mkCodegenTests conf "DIT" (return . Re . dit) sizes

difCodegenTests :: Config -> [Int] -> Spec
difCodegenTests conf sizes =
    describe "Generated DIF" $
    mkCodegenTests conf "DIF" (return . Re . dif) sizes

splitRadixCodegenTests :: Config -> [Int] -> Spec
splitRadixCodegenTests conf sizes =
    describe "Generated splitRadix DFT" $
    mkCodegenTests conf "SplitRadix" (runSearch () f . Re . DFT) sizes
  where
    f :: (Typeable a, Typed a, MonadSpiral m)
      => SPL (Exp a)
      -> S s m (SPL (Exp a))
    f (F n w) = splitRadixBreakdown n w
    f _       = mzero

improvedSplitRadixCodegenTests :: Config -> [Int] -> Spec
improvedSplitRadixCodegenTests conf sizes =
    describe "Generated improved split radix DFT" $
    mkCodegenTests conf "ImprovedSplitRadix" (runSearch () f . Re . DFT) sizes
  where
    f :: (Typeable a, Typed a, MonadSpiral m)
      => SPL (Exp a)
      -> S s m (SPL (Exp a))
    f (F n w) = improvedSplitRadixBreakdown n w
    f _       = mzero

searchCodegenTests :: Config -> [Int] -> Spec
searchCodegenTests conf sizes =
    describe "Generated opcount-optimized DFT" $
    mkCodegenTests conf "OpcountSearch" (searchOpCount . Re . DFT) sizes

mkCodegenTests :: Config
               -> String
               -> (Int -> Spiral (SPL (Exp Double)))
               -> [Int]
               -> Spec
mkCodegenTests conf desc f = mapM_ dftTest
  where
    dftTest :: Int -> Spec
    dftTest n = it (desc ++ "(" ++ show n ++ ")") $ \(k :: Property -> IO Result) -> do
        Re e <- runSpiralWith mempty $ f n
        withComplexTransform conf ("dft" ++ show n) e $ \dft ->
          k $ forAll (uniformVectorsOfSize n) $ \v -> epsDiff (dft v) (FFTW.fft n v)

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

modCodegenTests :: Config
                -> Spec
modCodegenTests conf =
    describe "Generated modular DFT" $ do
    describe "ℤ/17" $
        mkModCodegenTests (Proxy :: Proxy 17) conf "DIT" (return . dit) [2^n | n <- [1..4::Int]]
    describe "ℤ/2013265921" $
        mkModCodegenTests (Proxy :: Proxy 2013265921) conf "DIT" (return . dit) [2^n | n <- [1..12::Int]]

mkModCodegenTests :: forall p . KnownNat p
                  => Proxy p
                  -> Config
                  -> String
                  -> (Int -> Spiral (SPL (Exp (ℤ/p))))
                  -> [Int]
                  -> Spec
mkModCodegenTests proxy conf desc f = mapM_ dftTest
  where
    p :: Integer
    p = natVal proxy

    dftTest :: Int -> Spec
    dftTest n = it (desc ++ "(" ++ show n ++ ")") $ \(k :: Property -> IO Result) -> do
        e <- runSpiralWith mempty $ f n
        withModularTransform conf ("moddft_" ++ show p ++ "_" ++ show n) e $ \moddft ->
          k $ forAll basis $ \i -> moddft (e_i i) === res i
      where
        basis :: Gen Int
        basis = do
            Positive x <- arbitrary
            return $ x `mod` n

        e_i :: Int -> VS.Vector (ℤ/p)
        e_i i = VS.generate n $ \j -> if i == j then 1 else 0

        res :: Int -> VS.Vector (ℤ/p)
        res i = VS.generate n $ \j -> a^j
          where
            a :: ℤ/p
            a = w^i

        w :: ℤ/p
        w = omega n
