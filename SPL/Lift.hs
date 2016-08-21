{-# LANGUAGE RankNTypes #-}

-- |
-- Module      :  SPL.Lift
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module SPL.Lift (
    Unop(..),
    Binop(..),

    LiftNum(..),

    isZero,
    isOne,
    isNegOne
  ) where

-- | Unary operators
data Unop = Neg
          | Abs
          | Signum
  deriving (Eq, Ord, Show, Enum)

-- | Binary operators
data Binop = Add
           | Sub
           | Mul
           | Div
  deriving (Eq, Ord, Show, Enum)

-- | Test for 0
isZero :: LiftNum a => a -> Bool
isZero = isIntegral 0

-- | Test for 1
isOne :: LiftNum a => a -> Bool
isOne = isIntegral 1

-- | Test for -1
isNegOne :: LiftNum a => a -> Bool
isNegOne = isIntegral (-1)

-- | Class to lift 'Num' operators.
class LiftNum b where
    isIntegral :: (forall a . Integral a => a) -> b -> Bool

    liftNum :: Unop -> (forall a . Num a => a -> a) -> b -> b
    liftNum = liftNum_

    liftNum_ :: Unop -> (forall a . Num a => a -> a) -> b -> b

    liftNum2 :: Binop -> (forall a . Num a => a -> a -> a) -> b -> b -> b
    liftNum2 Add _ x y | isZero x = y
                       | isZero y = x

    liftNum2 Sub _ x y | isZero x = liftNum Neg negate y
                       | isZero y = x

    liftNum2 Mul _ x y | isZero x   = x
                       | isZero y   = y
                       | isOne x    = y
                       | isNegOne x = liftNum Neg negate y
                       | isOne y    = x
                       | isNegOne y = liftNum Neg negate x

    liftNum2 op f x y = liftNum2_ op f x y

    liftNum2_ :: Binop -> (forall a . Num a => a -> a -> a) -> b -> b -> b
