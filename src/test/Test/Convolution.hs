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
    linearConvolutionTests
  ) where

import Data.Complex
import Test.HUnit ((@?=))
import Test.Hspec

import Spiral.Convolution
import Spiral.Exp
import Spiral.SPL

linearConvolutionTests :: Spec
linearConvolutionTests = describe "Linear Convolution" $ do
  sequence_ [linearConvolutionTest (Standard i) i | i <- [1..16::Int]]
  sequence_ [linearConvolutionTest (ToomCook i) i | i <- [1..16::Int]]

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
