{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      :  Spiral.Exp
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Exp (
    Exp(..),

    flatten,

    rootOfUnity
  ) where

import Data.Complex
import Data.Ratio ((%),
                   denominator,
                   numerator)
import Test.QuickCheck
import Text.PrettyPrint.Mainland hiding (flatten)

import Spiral.Util.Lift

-- | Representation of scalar constants.
data Exp a where
    IntC      :: Int -> Exp Int
    IntegerC  :: Integer -> Exp Integer
    DoubleC   :: Double -> Exp Double
    RationalC :: Rational -> Exp Rational
    ComplexC  :: Exp Double -> Exp Double -> Exp (Complex Double)

    -- Root of unity, $e^{2 \pi i \frac{k}{n}}$
    RouC :: Rational -> Exp (Complex Double)

    -- Multiple of $\pi$
    PiC :: Rational -> Exp Double

-- | Lower an expression to its constant value.
lower :: Exp a -> a
lower (IntC x)       = x
lower (IntegerC x)   = x
lower (DoubleC x)    = x
lower (RationalC x)  = x
lower (ComplexC r i) = lower r :+ lower i
lower (RouC r)       = lower (cos (PiC (2*r))) :+ lower (sin (PiC (2*r)))
lower (PiC r)        = pi*fromRational r

deriving instance Eq (Exp a)
deriving instance Show (Exp a)

instance Ord (Exp a) where
    compare x y =
        case (flatten x, flatten y) of
            (IntC x, IntC y)                 -> compare x y
            (IntegerC x, IntegerC y)         -> compare x y
            (DoubleC x, DoubleC y)           -> compare x y
            (RationalC x, RationalC y)       -> compare x y
            (ComplexC r1 i1, ComplexC r2 i2) -> compare (r1, i1) (r2, i2)
            _                                -> error "can't happen"

instance Arbitrary (Exp Int) where
    arbitrary = IntC <$> arbitrary

instance Arbitrary (Exp Integer) where
    arbitrary = IntegerC <$> arbitrary

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
    ppr (IntegerC x)  = ppr x
    ppr (DoubleC x)   = ppr x
    ppr (RationalC x) = ppr x

    ppr x@(ComplexC r i)
      | r == 0 && i == 0    = char '0'
      | r == 0 && i == 1    = char 'i'
      | r == 0 && i == (-1) = text "-i"
      | r == 0              = ppr i <> char 'i'
      | i == 0              = ppr r
      | otherwise           = pprComplex (lower x)

    ppr (RouC r)
        | denominator r <= 4 = ppr (flatten (RouC r))
        | r < 0              = text "exp" <> parens (char '-' <> go (negate r))
        | otherwise          = text "exp" <> parens (go r)
      where
      go r = text "2*pi*i*" <> ppr (numerator r) <>
                               char '/' <>
                               ppr (denominator r)

    ppr (PiC r) = pprPrec appPrec1 r <+> ppr "pi"
      where
        appPrec1 = 6

-- | Convert a 'Complex Double' to a 'Constant'
fromComplex :: Complex Double -> Exp (Complex Double)
fromComplex (r :+ i) = ComplexC (DoubleC r) (DoubleC i)

-- | Normalize an expression's representation.
normalize :: Exp a -> Exp a
normalize e@(RouC r)
    | r > 1 || r < -1 = normalize (RouC ((n `rem` d) % d))
    | r < 0           = normalize (RouC (1 + r))
    | r == 0          = ComplexC 1 0
    | r == 1 % 4      = ComplexC 0 1
    | r == 1 % 2      = ComplexC (-1) 0
    | r == 3 % 4      = ComplexC 0 (-1)
    | otherwise       = e
  where
    n, d :: Integer
    n = numerator r
    d = denominator r

normalize e =
    e

-- | Flatten a constant's representation.
flatten :: Exp a -> Exp a
flatten (RouC r) = fromComplex (lower (normalize (RouC r)))
flatten (PiC r)  = DoubleC (fromRational r * pi)
flatten e        = e

-- | Return $e^{2 \pi i \frac{k}{n}$
rootOfUnity :: Rational -> Exp (Complex Double)
rootOfUnity = normalize . RouC

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
    liftNum_ _  f x@ComplexC{}  = fromComplex (f (lower x))
    liftNum_ op f c             = liftNum op f (flatten c)

    liftNum2_ Mul _ (RouC x) (RouC y)            = normalize $ RouC (x + y)
    liftNum2_ Mul f (RouC x) (ComplexC 1      0) = liftNum2 Mul f (RouC x) (RouC 0)
    liftNum2_ Mul f (RouC x) (ComplexC (-1)   0) = liftNum2 Mul f (RouC x) (RouC (1 % 2))
    liftNum2_ Mul f (RouC x) (ComplexC 0      1) = liftNum2 Mul f (RouC x) (RouC (1 % 4))
    liftNum2_ Mul f (RouC x) (ComplexC 0   (-1)) = liftNum2 Mul f (RouC x) (RouC (3 % 4))
    liftNum2_ Mul f x        y@RouC{}            = liftNum2 Mul f y x

    liftNum2_ _  f (IntC x)      (IntC y)      = IntC (f x y)
    liftNum2_ _  f (DoubleC x)   (DoubleC y)   = DoubleC (f x y)
    liftNum2_ _  f (RationalC x) (RationalC y) = RationalC (f x y)
    liftNum2_ _  f x@ComplexC{}  y@ComplexC{}  = fromComplex $ f (lower x) (lower y)
    liftNum2_ op f x             y             = liftNum2 op f (flatten x) (flatten y)

instance Num (Exp Int) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = IntC . fromInteger

instance Num (Exp Integer) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = IntegerC

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
