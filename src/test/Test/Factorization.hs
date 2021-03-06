{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Test.Factorization
-- Copyright   :  (c) 2016-2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Test.Factorization (
    factorizationTests,
    f2Test,
    f4Test,
    f8Test,
    cooleyTukeyDITTest,
    cooleyTukeyDIFTest,
    goodThomasTest,
    raderTest,
    bluesteinTest,
    winogradSmallTest,
    ditTest,
    difTest,
    splitRadixTest,
    splitRadix8Test,
    conjPairSplitRadixTest,
    improvedSplitRadixTest,
    opcountSearchTest
  ) where

import Data.Complex
import Data.Modular
import Data.Proxy (Proxy(..))
import Test.HUnit ((@?=))
import Test.Hspec

import Spiral.Array (M,
                     Matrix)
import qualified Spiral.Array as A
import Spiral.Convolution
import Spiral.Driver
import Spiral.Exp
import Spiral.FFT.Bluestein
import Spiral.FFT.CooleyTukey
import Spiral.FFT.GoodThomas
import Spiral.FFT.Rader
import Spiral.FFT.Winograd
import Spiral.NumberTheory (factors)
import Spiral.RootOfUnity
import Spiral.SPL
import Spiral.Search.FFTBreakdowns (getCycs)
import Spiral.Search.OpCount

factorizationTests :: Spec
factorizationTests = do
    f2Test
    f4Test
    f8Test
    complexFactorizationTests
    describe "Modular DFT" $ do
        describe "ℤ/17" $
            sequence_ [ditTest (Proxy :: Proxy (ℤ/17)) i | i <- [1..4::Int]]
        describe "ℤ/2013265921" $
            sequence_ [ditTest (Proxy :: Proxy (ℤ/2013265921)) i | i <- [1..7::Int]]

complexFactorizationTests :: Spec
complexFactorizationTests = do
    describe "Cooley-Tukey" $ do
        cooleyTukeyDITTest p 5 7
        cooleyTukeyDIFTest p 5 7
    describe "Good-Thomas" $
        goodThomasTest p 5 7
    describe "Rader" $ do
        mapM_ (raderTest p "Rader" rader) [7, 23]
        mapM_ (raderTest p "RaderI" (\n w -> raderI n w (ConvolutionTheorem (n-1)))) [7, 17, 23]
        mapM_ (raderTest p "RaderII" raderII) [7, 17, 23]
        mapM_ (raderTest p "RaderIII" raderIII) [7, 17, 23]
        mapM_ (raderTest p "RaderIV" raderIV) [7, 17, 23]
        mapM_ (raderTest p "raderLuIII" raderLuIII) [7, 17, 23]
    describe "Bluestein" $ do
        mapM_ (bluesteinTest p "Bluestein" bluestein) [(3, 6), (4, 8), (4, 9), (9, 18)]
        mapM_ (bluesteinTest p "Bluestein'" bluestein') [(3, 6), (4, 8), (4, 9), (9, 18)]
    describe "Winograd Small" $ do
        winogradSmallTest 3 (ConvolutionTheorem 2)
        winogradSmallTest 5 (ConvolutionTheorem 4)
        winogradSmallTest 7 (SplitNesting [3, 2] [(Winograd 3 [Standard 1, Standard 2]), (Winograd 2 [Standard 1, Standard 1])])
        winogradSmallTest 7 (AgarwalCooley 3 2 (Winograd 3 [Standard 1, Lift 2 6 (Tensor [3,2] [Standard 3,Standard 2])]) (Winograd 2 [Standard 1,Standard 1]))
    describe "Winograd Large" $
        mapM_ winogradLargeTest [(2,3), (5, 2), (2,7), (3,4), (4,3), (5,4), (3,8), (5,8), (8,3), (3,5), (5,3)]
    describe "Winograd 2^n" $
        mapM_ winogradTwoTest [2^i | i <- [1..5::Int]]
    describe "Winograd Power" $ do
        winogradPowerTest 3 2
        winogradSquareTest 3
        winogradSquareTest 5
    describe "DIT" $
        sequence_ [ditTest p n | n <- [1..7]]
    describe "DIF" $
        sequence_ [difTest p n | n <- [1..7]]
    describe "Split Radix" $
        sequence_ [splitRadixTest p n | n <- [1..3]]
    describe "Split Radix 8" $
        sequence_ [splitRadix8Test p n | n <- [1..2]]
    describe "Conjugate Pair Split Radix" $
        sequence_ [conjPairSplitRadixTest p n | n <- [1..3]]
    describe "Improved Split Radix" $
        sequence_ [improvedSplitRadixTest p n | n <- [1..3]]
  where
    p :: Proxy (Complex Double)
    p = Proxy

-- $F_2$
f2Test :: Spec
f2Test = it "F_2" $ toMatrix (dit 2) @?= f2
  where
    f2 :: Matrix M (Exp (Complex Double))
    f2 = A.matrix [[1,  1],
                   [1, -1]]

-- $F_4$ calculated per "SPL: A Language and Compiler for DSP Algorithms"
f4Test :: Spec
f4Test = it "F_4" $ toMatrix (dit 4) @?= f4
  where
    f4 :: Matrix M (Exp (Complex Double))
    f4 = A.matrix [[1,  1,  1,  1],
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
    f8 = A.matrix [[1,  1,  1,  1, 1, 1, 1, 1],
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
cooleyTukeyDITTest :: forall p . RootOfUnity (Exp p)
                   => Proxy p
                   -> Int
                   -> Int
                   -> Spec
cooleyTukeyDITTest _ r s =
    it ("CooleyTukeyDIT(" ++ show r ++ "," ++ show s ++ ")") $
    toMatrix (cooleyTukeyDIT r s w) @?= toMatrix (F rs w)
  where
    w :: Exp p
    w = omega rs

    rs :: Int
    rs = r*s

-- Test Cooley-Tukey DIF
cooleyTukeyDIFTest :: forall p . RootOfUnity (Exp p)
                   => Proxy p
                   -> Int
                   -> Int
                   -> Spec
cooleyTukeyDIFTest _ r s =
    it ("CooleyTukeyDIF(" ++ show r ++ "," ++ show s ++ ")") $
    toMatrix (cooleyTukeyDIF r s w) @?= toMatrix (F rs w)
  where
    w :: Exp p
    w = omega rs

    rs :: Int
    rs = r*s

-- Test Good-Thomas with factors 5 and 7
goodThomasTest :: forall p . RootOfUnity (Exp p)
               => Proxy p
               -> Int
               -> Int
               -> Spec
goodThomasTest _ r s =
    it ("GoodThomas(" ++ show r ++ "," ++ show s ++ ")") $
    toMatrix (goodThomas r s w) @?= toMatrix (F rs w)
  where
    w :: Exp p
    w = omega rs

    rs :: Int
    rs = r*s

-- Test Rader for given prime
raderTest :: forall p . RootOfUnity (Exp p)
          => Proxy p
          -> String
          -> (Int -> Exp p -> SPL (Exp p))
          -> Int
          -> Spec
raderTest _ desc rader n = it (desc ++ "(" ++ show n ++ ")") $
    toMatrix (rader (fromIntegral n) w) @?= toMatrix (F n w)
  where
    w :: Exp p
    w = omega n

-- Test Bluestein for given n and m
bluesteinTest :: forall p . RootOfUnity (Exp p)
              => Proxy p
              -> String
              -> (Int -> Int -> Exp p -> SPL (Exp p))
              -> (Int, Int)
              -> Spec
bluesteinTest _ desc bluestein (n, m) = it (desc ++ "(" ++ show n ++ "," ++ show m ++ ")") $
    toMatrix (bluestein n m w) @?= toMatrix (DFT n)
  where
    w :: Exp p
    w = omega (2*n)

winogradSmallTest :: Int
                  -> CyclicConvolution (Exp (Complex Double))
                  -> Spec
winogradSmallTest n cyc =
    it ("WinogradSmall(" ++ show n ++ "--" ++ show cyc ++ ")") $
    toMatrix (winogradSmall n w cyc) @?= toMatrix (F n w)
  where
    w :: Exp (Complex Double)
    w = omega n

winogradLargeTest :: (Int, Int) -> Spec
winogradLargeTest (r, s) = it ("WinogradLarge(" ++ show r ++ ", " ++ show s ++ ")") $
    sequence_ [toMatrix (winogradLarge r s w cr cc) @?= toMatrix (F n w)
                | cr <- take 2 $ getWinogradTriple' r s w getCycs
                , cc <- take 2 $ getWinogradTriple' s r w getCycs]
  where
    n :: Int
    n = r*s

    w :: Exp (Complex Double)
    w = omega n

winogradTwoTest :: Int -> Spec
winogradTwoTest n@2 = describe ("Winograd N=" ++ show n) $ do
    sequence_ [it ("DIF(2,1): w^" ++ show i) $ toMatrix (winogradDIF 2 1 (w^i)) @?= toMatrix (F n (w^i)) | i <- [1..n-1], rem i 2 /= 0]
    sequence_ [it ("DIT(2,1): w^" ++ show i) $ toMatrix (winogradDIT 2 1 (w^i)) @?= toMatrix (F n (w^i)) | i <- [1..n-1], rem i 2 /= 0]
  where
    w :: Exp (Complex Double)
    w = omega n

winogradTwoTest n = describe ("Winograd N=" ++ show n) $ do
    sequence_ [it ("DIF(" ++ show r ++ "," ++ show s ++ "): w^" ++ show i) $ toMatrix (winogradDIF r s (w^i)) @?= toMatrix (F n (w^i)) | i <- [1,n-1], rem i 2 /= 0, (r,s) <- factors n]
    sequence_ [it ("DIT(" ++ show r ++ "," ++ show s ++ "): w^" ++ show i) $ toMatrix (winogradDIT r s (w^i)) @?= toMatrix (F n (w^i)) | i <- [1,n-1], rem i 2 /= 0, (r,s) <- factors n]
    sequence_ [it ("SplitRadix: w^" ++ show i) $ toMatrix (winogradSplitRadix n (w^i)) @?= toMatrix (F n (w^i)) | i <- [1,n-1], rem i 2 /= 0]
    sequence_ [it ("ConjPairSplitRadix: w^" ++ show i) $ toMatrix (winogradConjPairSplitRadix n (w^i)) @?= toMatrix (F n (w^i)) | i <- [1,n-1], rem i 2 /= 0]
    sequence_ [it ("SplitRadix8: w^" ++ show i) $ toMatrix (winogradSplitRadix8 n (w^i)) @?= toMatrix (F n (w^i)) | i <- [1,n-1], n >= 8, rem i 2 /= 0]
    sequence_ [it ("ImprovedSplitRadix: w^" ++ show i) $ toMatrix (winogradImprovedSplitRadix n (w^i)) @?= toMatrix (F n (w^i)) | i <- [1,n-1], rem i 2 /= 0]
  where
    w :: Exp (Complex Double)
    w = omega n

winogradPowerTest :: Int -> Int -> Spec
winogradPowerTest p k = describe ("WinogradPower(" ++ show p ++ "^" ++ show k ++ ")") $
    sequence_
    [it ("w^" ++ show i ++ " -- p': " ++ show cP ++ " -- s: " ++ show cS) $
        toMatrix (winogradPower p k (w^^i) cP cS) @?= toMatrix (F n (w^^i))
        | i  <- [1,8],
          cP <- take 2 $ getCycs p',
          cS <- take 2 $ getCycs s]
  where
    n, p', s :: Int
    n  = p^k
    p' = (p-1)
    s  = p * p'

    w :: Exp (Complex Double)
    w = omega n

winogradSquareTest :: Int -> Spec
winogradSquareTest p = describe ("WinogradSquare(" ++ show p ++ "^2)") $
    sequence_
    [it ("w^" ++ show i) $ toMatrix f @?= toMatrix (F n (w^^i))
        | i  <- [1,p],
          f  <- take 1 $ winogradSquare p k (w^^i) getCycs]
  where
    n, p', s, k :: Int
    k  = 2
    n  = p^k
    p' = (p-1)
    s  = p * p'

    w :: Exp (Complex Double)
    w = omega n

-- DIT test
ditTest :: forall p . RootOfUnity (Exp p)
        => Proxy p
        -> Int
        -> Spec
ditTest _ n = it ("DIT(2^" ++ show n ++ ")") $
    toMatrix fft_spl @?= toMatrix (DFT (2^n))
  where
    fft_spl :: SPL (Exp p)
    fft_spl = dit (2^n)

-- DIF test
difTest :: forall p . RootOfUnity (Exp p)
        => Proxy p
        -> Int
        -> Spec
difTest _ n = it ("DIF(2^" ++ show n ++ ")") $
    toMatrix fft_spl @?= toMatrix (DFT (2^n))
  where
    fft_spl :: SPL (Exp p)
    fft_spl = dif (2^n)

-- Split radix test
splitRadixTest :: forall p . RootOfUnity (Exp p)
               => Proxy p
               -> Int
               -> Spec
splitRadixTest _ n = it ("SplitRadix(4^" ++ show n ++ ")") $
    toMatrix (splitRadix (4^n) w) @?= toMatrix (DFT (4^n))
  where
    w :: Exp p
    w = omega (4^n)

-- Split radix 8 test
splitRadix8Test :: forall p . RootOfUnity (Exp p)
                => Proxy p
                -> Int
                -> Spec
splitRadix8Test _ n = it ("SplitRadix(8^" ++ show n ++ ")") $
    toMatrix (splitRadix8 (8^n) w) @?= toMatrix (DFT (8^n))
  where
    w :: Exp p
    w = omega (8^n)

-- Split radix test
conjPairSplitRadixTest :: forall p . RootOfUnity (Exp p)
                       => Proxy p
                       -> Int
                       -> Spec
conjPairSplitRadixTest _ n = it ("ConjPairSplitRadix(4^" ++ show n ++ ")") $
    toMatrix (conjPairSplitRadix (4^n) w) @?= toMatrix (DFT (4^n))
  where
    w :: Exp p
    w = omega (4^n)

-- Split radix test
improvedSplitRadixTest :: forall p . (Floating (Exp p), RootOfUnity (Exp p))
                       => Proxy p
                       -> Int
                       -> Spec
improvedSplitRadixTest _ n = it ("ImprovedSplitRadix(4^" ++ show n ++ ")") $
    toMatrix (improvedSplitRadix (4^n) w) @?= toMatrix (DFT (4^n))
  where
    w :: Exp p
    w = omega (4^n)

-- Lowest-opcount test
opcountSearchTest :: Int -> Spec
opcountSearchTest n = it ("OpcountSearch(" ++ show n ++ ")") $ do
    Re e <- runSpiralWith mempty $
            searchOpCount (Re (DFT n) :: SPL (Exp Double))
    toMatrix e @?= toMatrix (DFT n)
