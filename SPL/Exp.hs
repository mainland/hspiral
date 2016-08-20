{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- |
-- Module      :  SPL.Exp
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module SPL.Exp (
    Exp(..),

    ToComplex(..)
  ) where

import Data.Complex
import Data.Ratio ((%),
                   denominator,
                   numerator)
import Test.QuickCheck
import Text.PrettyPrint.Mainland

import SPL.ExtendedFloat

-- | Representation of scalar constants.
data Exp a where
    IntC      :: Integer -> Exp Integer
    DoubleC   :: Double -> Exp Double
    RationalC :: Rational -> Exp Rational
    ComplexC  :: Exp Double -> Exp Double -> Exp (Complex Double)

    -- Root of unity, $e^{2 \pi i \frac{k}{n}}$
    RouC :: Rational -> Exp (Complex Double)

deriving instance Eq (Exp a)
deriving instance Show (Exp a)

instance Arbitrary (Exp Integer) where
    arbitrary = IntC <$> arbitrary

instance Arbitrary (Exp Double) where
    arbitrary = DoubleC <$> arbitrary

instance Arbitrary (Exp Rational) where
    arbitrary = RationalC <$> arbitrary

instance Arbitrary (Exp (Complex Double)) where
    arbitrary = ComplexC <$> arbitrary <*> arbitrary

-- Orphan instance...
pprComplex :: (Eq a, Num a, Pretty a) => Complex a -> Doc
pprComplex (r :+ 0) = ppr r
pprComplex (r :+ i) = ppr r <+> text "+" <+> ppr i <> char 'i'

instance Pretty (Exp a) where
    ppr (IntC x)      = ppr x
    ppr (DoubleC x)   = ppr x
    ppr (RationalC x) = ppr x

    ppr x@(ComplexC r i)
      | r == 0 && i == 0    = char '0'
      | r == 0 && i == 1    = char 'i'
      | r == 0 && i == (-1) = text "-i"
      | r == 0              = ppr i <> char 'i'
      | i == 0              = ppr r
      | otherwise           = pprComplex (unComplex x)

    ppr (RouC r)
        | denominator r <= 4 = ppr (toComplex (RouC r))
        | r < 0              = text "exp" <> parens (char '-' <> go (negate r))
        | otherwise          = text "exp" <> parens (go r)
      where
      go r = text "2*pi*i*" <> ppr (numerator r) <>
                               char '/' <>
                               ppr (denominator r)

instance ExtendedFloat (Exp (Complex Double)) where
    rootOfUnity = RouC

-- | Convert a 'Complex Double' to a 'Constant'
fromComplex :: Complex Double -> Exp (Complex Double)
fromComplex (r :+ i) = ComplexC (DoubleC r) (DoubleC i)

-- | Convert a 'Constant' to a 'Complex Double'
unComplex :: Exp (Complex Double) -> Complex Double
unComplex (ComplexC (DoubleC r) (DoubleC i)) =
    r :+ i

unComplex (RouC x) =
    exp (2*pi*i*fromRational x)
  where
    i = 0 :+ 1

-- | Test for equality with Integral
isIntegral :: Integral a => a -> Exp b -> Bool
isIntegral x = go
  where
    go :: Exp b -> Bool
    go (IntC y)       = y == fromIntegral x
    go (DoubleC y)    = y == fromIntegral x
    go (RationalC y)  = y == fromIntegral x
    go (ComplexC y i) = y == fromIntegral x && isZero i
    go (RouC 1)       = x == 1
    go _              = False

-- | Test for 0
isZero :: Exp a -> Bool
isZero = isIntegral (0 :: Int)

-- | Test for 1
isOne :: Exp a -> Bool
isOne = isIntegral (1 :: Int)

-- | Test for -1
isNegOne :: Exp a -> Bool
isNegOne = isIntegral (-1 :: Int)

class ToComplex f where
    toComplex :: f a -> f (Complex Double)

instance ToComplex Exp where
    toComplex (IntC x)      = ComplexC (fromIntegral x) 0
    toComplex (DoubleC x)   = ComplexC (DoubleC x) 0
    toComplex (RationalC x) = ComplexC (DoubleC (fromRational x)) 0
    toComplex x@ComplexC{}  = x

    toComplex (RouC r)
        | r < 0      = toComplex (RouC (1 + r))
        | r == 0     = ComplexC 1 0
        | r == 1 % 4 = ComplexC 0 1
        | r == 1 % 2 = ComplexC (-1) 0
        | r == 3 % 4 = ComplexC 0 (-1)
        | otherwise  = fromComplex (unComplex (RouC r))

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

-- | Class to lift 'Num' operators to a functor
class LiftNum f where
    liftNum  :: Num a => Unop -> (a -> a) -> f a -> f a
    liftNum2 :: Num a => Binop -> (a -> a -> a) -> f a -> f a -> f a

instance LiftNum Exp where
    liftNum Neg _ (RouC r) = RouC (r + 1 % 2)

    liftNum _  f (IntC x)      = IntC (f x)
    liftNum _  f (DoubleC x)   = DoubleC (f x)
    liftNum _  f (RationalC x) = RationalC (f x)
    liftNum _  f x@ComplexC{}  = fromComplex (f (unComplex x))
    liftNum op f c@RouC{}      = liftNum op f (toComplex c)

    liftNum2 Add _ x y | isZero x = y
                       | isZero y = x

    liftNum2 Sub _ x y | isZero x = liftNum Neg negate y
                       | isZero y = x

    liftNum2 Mul _ x y | isOne x    = y
                       | isNegOne x = liftNum Neg negate y
                       | isOne y    = x
                       | isNegOne y = liftNum Neg negate x

    liftNum2 Mul _ (RouC x) (RouC y) =
        RouC (norm (x + y))
      where
        norm :: Rational -> Rational
        norm x = (n `rem` d) % d
          where
            n = numerator x
            d = denominator x

    liftNum2 Mul f (RouC x) (ComplexC 1      0) = liftNum2 Mul f (RouC x) (RouC 0)
    liftNum2 Mul f (RouC x) (ComplexC (-1)   0) = liftNum2 Mul f (RouC x) (RouC (1 % 2))
    liftNum2 Mul f (RouC x) (ComplexC 0      1) = liftNum2 Mul f (RouC x) (RouC (1 % 4))
    liftNum2 Mul f (RouC x) (ComplexC 0   (-1)) = liftNum2 Mul f (RouC x) (RouC (3 % 4))
    liftNum2 Mul f x        y@RouC{}            = liftNum2 Mul f y x

    liftNum2 _   f (IntC x)      (IntC y)      = IntC (f x y)
    liftNum2 _   f (DoubleC x)   (DoubleC y)   = DoubleC (f x y)
    liftNum2 _   f (RationalC x) (RationalC y) = RationalC (f x y)
    liftNum2 _   f x@ComplexC{}  y@ComplexC{}  = fromComplex $ f (unComplex x) (unComplex y)
    liftNum2 _   f x@ComplexC{}  y@RouC{}      = fromComplex $ f (unComplex x) (unComplex y)
    liftNum2 _   f x@RouC{}      y@ComplexC{}  = fromComplex $ f (unComplex x) (unComplex y)
    liftNum2 op  f x@RouC{}      y@RouC{}      = liftNum2 op f (toComplex x) (toComplex y)

instance Num (Exp Integer) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = IntC

instance Num (Exp Double) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger x = DoubleC (fromInteger x)

instance Num (Exp Rational) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger x = RationalC (fromInteger x)

instance Num (Exp (Complex Double)) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger x = fromComplex (fromInteger x)
