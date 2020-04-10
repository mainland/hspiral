{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Spiral.Convolution.ConvolutionTheorem (
  biConvolutionTheorem,

  convolutionA,
  convolutionB,
  convolutionC
  ) where

import Spiral.RootOfUnity
import Spiral.SPL

-- | Convolution theorem in the form of (C, A, B) as expected of a bilinear
-- | algorithm
biConvolutionTheorem :: forall a . (RootOfUnity a, Show a) => Int -> (SPL a, SPL a, SPL a)
biConvolutionTheorem n = let j = permute $ J n
                         in (j × DFT n, DFT' n × j, DFT n)

convolutionA :: forall a . (RootOfUnity a, Show a) => Int -> SPL a
convolutionA n = DFT n

convolutionB :: forall a . (RootOfUnity a, Show a) => Int -> SPL a
convolutionB n = let j = permute $ J n
                 in DFT' n × j

convolutionC :: forall a . (RootOfUnity a, Show a) => Int -> SPL a
convolutionC n = let j = permute $ J n
                 in j × DFT n
