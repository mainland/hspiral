{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- |
-- Module      :  Spiral.Exp
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Exp (
    Exp(..),

    ToComplex(..)
  ) where

import Data.Complex
import Data.Ratio ((%),
                   denominator,
                   numerator)
import Test.QuickCheck
import Text.PrettyPrint.Mainland

import Spiral.ExtendedFloat
import Spiral.Util.Lift

-- | Representation of scalar constants.
data Exp a where
    IntC      :: Integer -> Exp Integer
    DoubleC   :: Double -> Exp Double
    RationalC :: Rational -> Exp Rational
    ComplexC  :: Exp Double -> Exp Double -> Exp (Complex Double)

    -- Root of unity, $e^{2 \pi i \frac{k}{n}}$
    RouC :: Rational -> Exp (Complex Double)

    -- Multiple of $\pi$
    PiC :: Rational -> Exp Double

lower :: Exp a -> a
lower (IntC x)       = x
lower (DoubleC x)    = x
lower (RationalC x)  = x
lower (ComplexC r i) = lower r :+ lower i
lower (RouC r)       = cos (lower (PiC (2*r))) :+ sin (lower (PiC (2*r)))
lower (PiC r)        = pi*fromRational r

deriving instance Eq (Exp a)
deriving instance Show (Exp a)

instance Ord (Exp a) where
    compare (IntC x)         (IntC y)         = compare x y
    compare (DoubleC x)      (DoubleC y)      = compare x y
    compare (RationalC x)    (RationalC y)    = compare x y
    compare (ComplexC r1 i1) (ComplexC r2 i2) = compare (r1, i1) (r2, i2)
    compare (RouC x)         (RouC y)         = compare x y
    compare ComplexC{}       RouC{}           = LT
    compare RouC{}           ComplexC{}       = GT

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

    normRootOfUnity (RouC x) = RouC ((n `rem` d) % d)
      where
        n = numerator x
        d = denominator x

    normRootOfUnity x = x

-- | Convert a 'Complex Double' to a 'Constant'
fromComplex :: Complex Double -> Exp (Complex Double)
fromComplex (r :+ i) = ComplexC (DoubleC r) (DoubleC i)

-- | Convert a 'Constant' to a 'Complex Double'
unComplex :: Exp (Complex Double) -> Complex Double
unComplex (ComplexC (DoubleC r) (DoubleC i)) = r :+ i
unComplex (RouC r) = lower (cos (PiC (2*r))) :+ lower (sin (PiC (2*r)))

-- | A functor whose contents can be converted to a 'Complex Double'
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

instance LiftNum (Exp a) where
    isIntegral x (IntC y)       = y == fromInteger x
    isIntegral x (DoubleC y)    = y == fromInteger x
    isIntegral x (RationalC y)  = y == fromInteger x
    isIntegral x (ComplexC y i) = y == fromInteger x && isZero i
    isIntegral x (RouC y)
      | y == 1                  = fromInteger x == (1 :: Integer)
      | y == 1 % 2              = fromInteger x == (-1 :: Integer)
    isIntegral _ _              = False

    liftNum_ Neg _ (RouC r) = RouC (r + 1 % 2)

    liftNum_ _  f (IntC x)      = IntC (f x)
    liftNum_ _  f (DoubleC x)   = DoubleC (f x)
    liftNum_ _  f (RationalC x) = RationalC (f x)
    liftNum_ _  f x@ComplexC{}  = fromComplex (f (unComplex x))
    liftNum_ op f c@RouC{}      = liftNum op f (toComplex c)

    liftNum2_ Mul _ (RouC x) (RouC y)            = normRootOfUnity $ RouC (x + y)
    liftNum2_ Mul f (RouC x) (ComplexC 1      0) = liftNum2 Mul f (RouC x) (RouC 0)
    liftNum2_ Mul f (RouC x) (ComplexC (-1)   0) = liftNum2 Mul f (RouC x) (RouC (1 % 2))
    liftNum2_ Mul f (RouC x) (ComplexC 0      1) = liftNum2 Mul f (RouC x) (RouC (1 % 4))
    liftNum2_ Mul f (RouC x) (ComplexC 0   (-1)) = liftNum2 Mul f (RouC x) (RouC (3 % 4))
    liftNum2_ Mul f x        y@RouC{}            = liftNum2 Mul f y x

    liftNum2_ _  f (IntC x)      (IntC y)      = IntC (f x y)
    liftNum2_ _  f (DoubleC x)   (DoubleC y)   = DoubleC (f x y)
    liftNum2_ _  f (RationalC x) (RationalC y) = RationalC (f x y)
    liftNum2_ _  f x@ComplexC{}  y@ComplexC{}  = fromComplex $ f (unComplex x) (unComplex y)
    liftNum2_ _  f x@ComplexC{}  y@RouC{}      = fromComplex $ f (unComplex x) (unComplex y)
    liftNum2_ _  f x@RouC{}      y@ComplexC{}  = fromComplex $ f (unComplex x) (unComplex y)
    liftNum2_ op f x@RouC{}      y@RouC{}      = liftNum2 op f (toComplex x) (toComplex y)

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

instance Fractional (Exp Double) where
    fromRational = DoubleC . fromRational

    x / y = DoubleC $ lower x / lower y

lift :: (Double -> Double) -> Exp Double -> Exp Double
lift f = DoubleC . f . lower

instance Floating (Exp Double) where
    pi = PiC 1

    exp = lift exp
    log = lift log
    asin = lift asin
    acos = lift acos
    atan = lift atan
    sinh = lift sinh
    cosh = lift cosh
    asinh = lift asinh
    acosh = lift acosh
    atanh = lift atanh

    sin (PiC x) = cos (PiC (x - 1 / 2))

    sin x = lift sin x

    cos (PiC x)
      | x >  1    = cos (PiC (x - 2))
      | x < -1    = cos (PiC (x + 2))
      | x == -1   = -1
      | x == -1/2 = 0
      | x == 0    = 1
      | x > 0     = 0

    cos x = lift cos x
