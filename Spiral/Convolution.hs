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
import Spiral.Convolution.Tensor
import Spiral.Convolution.ToomCook

data LinearConvolution a where

  -- | Standard Linear Convolution
  Standard :: (Num a) => Int -> LinearConvolution a

  -- | Toom-Cook Linear Convolution
  ToomCook :: (Fractional a) => Int -> LinearConvolution a

  -- | Tensor Product of Linear Convolutions
  Tensor :: [Int] -> [LinearConvolution a] -> LinearConvolution a

deriving instance Show e => Show (LinearConvolution e)

instance Bilinear LinearConvolution where

  getA (Standard n)  = standardLinearA n
  getA (ToomCook n)  = toomCookA n
  getA (Tensor _ ls) = tensorA (map getA ls)

  getB (Standard n)  = standardLinearB n
  getB (ToomCook n)  = toomCookB n
  getB (Tensor _ ls) = tensorB (map getB ls)

  getC (Standard n)   = standardLinearC n
  getC (ToomCook n)   = toomCookC n
  getC (Tensor ns ls) = tensorC ns (map getC ls)

instance Convolution LinearConvolution where
  toLinearA = error "Already a linear convolution. Did you mean to convert a cyclic?"
  toLinearB = error "Already a linear convolution. Did you mean to convert a cyclic?"
  toLinearC = error "Already a linear convolution. Did you mean to convert a cyclic?"

  toCyclicA = getA
  toCyclicB p lin = (transpose $ getC lin) × (transpose $ modMatrix p (2*((degree p)-1)))
  toCyclicC = transpose . getB

  getSize (Standard n)  = n
  getSize (ToomCook n)  = n
  getSize (Tensor ns _) = product ns
