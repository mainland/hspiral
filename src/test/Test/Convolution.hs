{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Test.Convolution
-- Copyright   :  (c) 2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Test.Convolution (
    convolutionTests,
    linearConvolutionTests,
    cyclicConvolutionTests
  ) where

import Data.Complex
import Test.HUnit ((@?=))
import Test.Hspec

import Spiral.Convolution
import Spiral.Exp
import Spiral.SPL

convolutionTests :: Spec
convolutionTests = describe "Convolution" $ do
    linearConvolutionTests
    cyclicConvolutionTests

linearConvolutionTests :: Spec
linearConvolutionTests = describe "Linear Convolution" $ do
  sequence_ [linearConvolutionTest (Standard i) i | i <- [1..16::Int]]
  sequence_ [linearConvolutionTest (ToomCook i) i | i <- [1..16::Int]]
  linearConvolutionTest (Tensor [2,2] [Standard 2, ToomCook 2]) 4
  linearConvolutionTest (Tensor [3,5] [Standard 3, ToomCook 5]) 15
  linearConvolutionTest (Tensor [2,2,2,2] [ToomCook 2, ToomCook 2, ToomCook 2, ToomCook 2]) 16
  sequence_ [linearConvolutionTest (Lift 5 i (Standard i)) 5 | i <- [6..10::Int]]

linearConvolutionTest :: LinearConvolution (Exp (Complex Double)) -> Int -> Spec
linearConvolutionTest lin n =
      it (show lin) $ toMatrix (convolve lin vector) @?= expected_result
  where
      l :: Int
      l = 2 * n - 1

      vector :: [Exp (Complex Double)]
      vector = [fromIntegral i | i <- [1..n]]

      -- expected_result :: Matrix M (Exp (Complex Double))
      expected_result = toMatrix $ transpose $ fromLists
          [replicate i 0 ++ vector ++ replicate (l - n - i) 0 | i <- [0..n-1]]

cyclicConvolutionTests :: Spec
cyclicConvolutionTests = describe "Cyclic Convolution" $
  sequence_ [cyclicConvolutionTest (ConvolutionTheorem i) | i <- [3..8::Int]]

cyclicConvolutionTest :: CyclicConvolution (Exp (Complex Double)) -> Spec
cyclicConvolutionTest cyc = it (show cyc) $
    toMatrix (convolve cyc vector) @?= toMatrix (circ vector)
  where
    n :: Int
    n = getSize cyc

    vector :: [Exp (Complex Double)]
    vector = [fromIntegral i | i <- [1..n]]
