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

  LinearConvolution(..),
  CyclicConvolution(..)
  ) where

import Spiral.RootOfUnity
import Spiral.SPL

import Spiral.Convolution.Core

-- Linear convolution algorithms
import Spiral.Convolution.Standard
import Spiral.Convolution.Tensor
import Spiral.Convolution.ToomCook

-- Cyclic convolution algorithms
import Spiral.Convolution.AgarwalCooley
import Spiral.Convolution.ConvolutionTheorem
import Spiral.Convolution.SplitNesting
import Spiral.Convolution.Winograd

data LinearConvolution a where

  -- | Standard Linear Convolution
  Standard :: (Num a) => Int -> LinearConvolution a

  -- | Toom-Cook Linear Convolution
  ToomCook :: (Fractional a) => Int -> LinearConvolution a

  -- | Tensor Product of Linear Convolutions
  Tensor :: [Int] -> [LinearConvolution a] -> LinearConvolution a

  -- | Liftend linear convolution (using a larger convolution to perform a smaller one)
  Lift :: Int -> Int -> LinearConvolution a -> LinearConvolution a

  -- | Linear convolution by cyclic convolution
  FromCyclic :: Int -> CyclicConvolution a -> LinearConvolution a

deriving instance Show e => Show (LinearConvolution e)

-- | The nxm matrix whose diagonal elements are 1 and off-diagonal elements are
-- 0.
ones :: (Num a, Show a) => Int -> Int -> SPL a
ones n m = fromFunction (ix2 n m) f
  where
    f (Z :. i :. j) | i == j    = 1
                    | otherwise = 0

instance Bilinear LinearConvolution where

  getA (Standard n)       = standardLinearA n
  getA (ToomCook n)       = toomCookA n
  getA (Tensor _ ls)      = tensorA (map getA ls)
  getA (Lift n m lin)     = (getA lin) × (ones m n)
  getA (FromCyclic _ cyc) = toLinearA cyc

  getB (Standard n)       = standardLinearB n
  getB (ToomCook n)       = toomCookB n
  getB (Tensor _ ls)      = tensorB (map getB ls)
  getB (Lift n m lin)     = (getB lin) × (ones m n)
  getB (FromCyclic _ cyc) = toLinearB cyc

  getC (Standard n)       = standardLinearC n
  getC (ToomCook n)       = toomCookC n
  getC (Tensor ns ls)     = tensorC ns (map getC ls)
  getC (Lift n m lin)     = (ones (2 * n - 1) (2 * m - 1)) × (getC lin)
  getC (FromCyclic _ cyc) = toLinearC cyc

instance Convolution LinearConvolution where
  toLinearA = error "Already a linear convolution. Did you mean to convert a cyclic?"
  toLinearB = error "Already a linear convolution. Did you mean to convert a cyclic?"
  toLinearC = error "Already a linear convolution. Did you mean to convert a cyclic?"

  toCyclicA       = getA
  toCyclicB p lin = (transpose $ getC lin) × (transpose $ modMatrix p (2*((degree p)-1)))
  toCyclicC       = transpose . getB

  getSize (Standard n)     = n
  getSize (ToomCook n)     = n
  getSize (Tensor ns _)    = product ns
  getSize (Lift n _ _)     = n
  getSize (FromCyclic n _) = n

data CyclicConvolution a where
  -- | Cyclic Convolution based on Convolution Theorem (given a size n)
  ConvolutionTheorem :: (RootOfUnity a) => Int -> CyclicConvolution a

  -- | Cyclic Convolution by Winograd, given a size n and linear convolution
  -- | options for the convoltuons that will be needed
  Winograd :: (RootOfUnity a, Eq a) => Int -> [LinearConvolution a] -> CyclicConvolution a

  -- | Given any coprime factors and cyclic convolutions of those sizes,
  -- | uses the PFA to construct a cyclic convolution of the product of the factors
  AgarwalCooley :: (RootOfUnity a, Eq a) => Int -> Int -> CyclicConvolution a -> CyclicConvolution a -> CyclicConvolution a

  -- | Similar to AgarwalCooley, however, must be constructed with Winograd options
  SplitNesting :: (RootOfUnity a, Eq a) => [Int] -> [CyclicConvolution a] -> CyclicConvolution a

deriving instance Show e => Show (CyclicConvolution e)

instance Bilinear CyclicConvolution where
  getA (ConvolutionTheorem n) = convolutionA n
  getA (Winograd n lins) = winogradA n lins
  getA (AgarwalCooley r s cr cs) = agarwalCooleyA r s (getA cr) (getA cs)
  getA (SplitNesting ns ws) = splitNestingA ns (map (coreWinogradA . lins)  ws)
    where
      lins (Winograd _ ls) = ls
      lins _               = error "Ill-formatted split nesting convolution"

  getB (ConvolutionTheorem n) = convolutionB n
  getB (Winograd n lins) = winogradB n lins
  getB (AgarwalCooley r s cr cs) = agarwalCooleyB r s (getB cr) (getB cs)
  getB (SplitNesting ns ws) = splitNestingB ns (map (\(n,ls) -> coreWinogradB n ls) $ map lins ws)
    where
      lins (Winograd n ls) = (n,ls)
      lins _               = error "Ill-formatted split nesting convolution"

  getC (ConvolutionTheorem n) = convolutionC n
  getC (Winograd n lins) = winogradC n lins
  getC (AgarwalCooley r s cr cs) = agarwalCooleyC r s (getC cr) (getC cs)
  getC (SplitNesting ns ws) = splitNestingC ns (map (coreWinogradC . lins) ws)
    where
      lins (Winograd _ ls) = ls
      lins _               = error "Ill-formatted split nesting convolution"

instance Convolution CyclicConvolution where
  toLinearA a = (getA a ⊕ I 1) × ex
    where
      m = getSize a
      n = div (m+2) 2
      ex = fromFunction (ix2 (2*n-1) n) f
        where
          f (Z :. i :. j) | i == j                     = 1
                          | i == (2*n-2) && j == (n-1) = 1
                          | otherwise                  = 0

  toLinearB b = (getB b ⊕ I 1) × ex
    where
      m = getSize b
      n = div (m+2) 2
      ex = fromFunction (ix2 (2*n-1) n) f
        where
          f (Z :. i :. j) | i == j                     = 1
                          | i == (2*n-2) && j == (n-1) = 1
                          | otherwise                  = 0

  toLinearC c = rx × (getC c ⊕ I 1)
    where
      m = getSize c
      n = div (m+2) 2
      rx = fromFunction (ix2 (2*n-1) (m+1)) f
        where
          f (Z :. i :. j) | i == j           = 1
                          | i == 0 && j == m = -1
                          | otherwise        = 0

  toCyclicA = error "Already a cyclic convolution. Did you mean to convert a linear?"
  toCyclicB = error "Already a cyclic convolution. Did you mean to convert a linear?"
  toCyclicC = error "Already a cyclic convolution. Did you mean to convert a linear?"

  getSize (ConvolutionTheorem n)  = n
  getSize (Winograd n _)          = n
  getSize (AgarwalCooley r s _ _) = r * s
  getSize (SplitNesting ns _)     = product ns
