{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Spiral.Convolution (
  module Spiral.Convolution.Core,

  LinearConvolution(..)
  ) where

import Spiral.SPL

import Spiral.Convolution.Core
import Spiral.Convolution.Standard
import Spiral.Convolution.ToomCook

data LinearConvolution a where

  -- | Standard Linear Convolution
  Standard :: (Num a) => Int -> LinearConvolution a

  -- | Toom-Cook Linear Convolution
  ToomCook :: (Fractional a) => Int -> LinearConvolution a

deriving instance Show e => Show (LinearConvolution e)

instance Bilinear LinearConvolution where

  getA (Standard n) = standardLinearA n
  getA (ToomCook n) = toomCookA n

  getB (Standard n) = standardLinearB n
  getB (ToomCook n) = toomCookB n

  getC (Standard n) = standardLinearC n
  getC (ToomCook n) = toomCookC n

instance Convolution LinearConvolution where
  toLinearA = error "Already a linear convolution. Did you mean to convert a cyclic?"
  toLinearB = error "Already a linear convolution. Did you mean to convert a cyclic?"
  toLinearC = error "Already a linear convolution. Did you mean to convert a cyclic?"

  toCyclicA = getA
  toCyclicB p lin = (transpose $ getC lin) × (transpose $ modMatrix p (2*((degree p)-1)))
  toCyclicC = transpose . getB

  getSize (Standard n) = n
  getSize (ToomCook n) = n
