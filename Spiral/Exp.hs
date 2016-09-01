{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Spiral.Exp
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Exp (
    Var,
    Const(..),
    Exp(..),
    Unop(..),
    Binop(..),

    fromComplex,
    flatten,

    LiftNum(..),
    liftNumOpt,
    liftNum2Opt,

    rootOfUnity,

    intE,
    complexE
  ) where

import Data.Complex
import Data.Ratio ((%),
                   denominator,
                   numerator)
import Test.QuickCheck (Arbitrary(..))
import Text.PrettyPrint.Mainland hiding (flatten)

type Var = String

-- | Representation of scalar expressions.
data Const a where
    IntC      :: Int -> Const Int
    IntegerC  :: Integer -> Const Integer
    DoubleC   :: Double -> Const Double
    RationalC :: Rational -> Const Rational
    ComplexC  :: Const Double -> Const Double -> Const (Complex Double)

    -- Root of unity, $e^{2 \pi i \frac{k}{n}}$
    RouC :: Rational -> Const (Complex Double)

    -- Multiple of $\pi$
    PiC :: Rational -> Const Double

deriving instance Eq (Const a)
deriving instance Show (Const a)

lower :: Const a -> a
lower (IntC x)       = x
lower (IntegerC x)   = x
lower (DoubleC x)    = x
lower (RationalC x)  = x
lower (ComplexC r i) = lower r :+ lower i
lower (RouC r)       = lower (cos (PiC (2*r))) :+ lower (sin (PiC (2*r)))
lower (PiC r)        = pi*fromRational r

instance Ord (Const a) where
    compare x y =
        case (flatten x, flatten y) of
            (IntC x, IntC y)                 -> compare x y
            (IntegerC x, IntegerC y)         -> compare x y
            (DoubleC x, DoubleC y)           -> compare x y
            (RationalC x, RationalC y)       -> compare x y
            (ComplexC r1 i1, ComplexC r2 i2) -> compare (r1, i1) (r2, i2)
            _                                -> error "can't happen"

instance Arbitrary (Const Int) where
    arbitrary = IntC <$> arbitrary

instance Arbitrary (Const Integer) where
    arbitrary = IntegerC <$> arbitrary

instance Arbitrary (Const Double) where
    arbitrary = DoubleC <$> arbitrary

instance Arbitrary (Const Rational) where
    arbitrary = RationalC <$> arbitrary

instance Arbitrary (Const (Complex Double)) where
    arbitrary = ComplexC <$> arbitrary <*> arbitrary

instance Pretty (Const a) where
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
        go r = text "2*pi*i*" <>
               ppr (numerator r) <>
               char '/' <>
               ppr (denominator r)

    ppr (PiC r) = pprPrec appPrec1 r <+> ppr "pi"
      where
        appPrec1 = 6

pprComplex :: (Eq a, Num a, Pretty a) => Complex a -> Doc
pprComplex (r :+ 0) = ppr r
pprComplex (r :+ i) = ppr r <+> text "+" <+> ppr i <> char 'i'

-- | Convert a 'Complex Double' to a 'Constant'
fromComplex :: Complex Double -> Const (Complex Double)
fromComplex (r :+ i) = ComplexC (DoubleC r) (DoubleC i)

-- | Normalize an expression's representation.
normalize :: Const a -> Const a
normalize c@(RouC r)
    | r > 1 || r < -1 = normalize (RouC ((n `rem` d) % d))
    | r < 0           = normalize (RouC (1 + r))
    | r == 0          = ComplexC 1 0
    | r == 1 % 4      = ComplexC 0 1
    | r == 1 % 2      = ComplexC (-1) 0
    | r == 3 % 4      = ComplexC 0 (-1)
    | otherwise       = c
  where
    n, d :: Integer
    n = numerator r
    d = denominator r

normalize c =
    c

-- | Flatten a constant's representation.
flatten :: Const a -> Const a
flatten (RouC r) = fromComplex (lower (normalize (RouC r)))
flatten (PiC r)  = DoubleC (fromRational r * pi)
flatten e        = e

-- | Return $e^{2 \pi i \frac{k}{n}$
rootOfUnity :: Rational -> Exp (Complex Double)
rootOfUnity = ConstE . normalize . RouC

-- | Representation of scalar constants.
data Exp a where
    ConstE :: Const a -> Exp a
    VarE   :: Var -> Exp a

deriving instance Eq (Exp a)
deriving instance Ord (Exp a)
deriving instance Show (Exp a)

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

instance Pretty (Exp a) where
    ppr (ConstE c) = ppr c
    ppr (VarE v)   = ppr v

-- | Class to lift 'Num' operators.
class LiftNum b where
    -- | Lift a unary operation on 'Num' to the type 'b'
    liftNum :: Unop -> (forall a . Num a => a -> a) -> b -> b

    -- | Lift a binary operation on 'Num' to the type 'b
    liftNum2 :: Binop -> (forall a . Num a => a -> a -> a) -> b -> b -> b

-- | "Optimizing" version of 'liftNum'.
liftNumOpt :: (Ord b, Num b, LiftNum b)
           => Unop
           -> (forall a . Num a => a -> a)
           -> b
           -> b
liftNumOpt Neg _ 0 = 0

liftNumOpt op f x = liftNum op f x

-- | "Optimizing" version of 'liftNum2'.
liftNum2Opt :: (Eq b, Num b, LiftNum b)
            => Binop
            -> (forall a . Num a => a -> a -> a)
            -> b
            -> b
            -> b
liftNum2Opt Add _ x 0 = x
liftNum2Opt Add _ 0 x = x

liftNum2Opt Sub _ 0 x = negate x
liftNum2Opt Sub _ x 0 = x

liftNum2Opt Mul _ 0    _    = 0
liftNum2Opt Mul _ _    0    = 0
liftNum2Opt Mul _ x    1    = x
liftNum2Opt Mul _ 1    x    = x
liftNum2Opt Mul _ x    (-1) = -x
liftNum2Opt Mul _ (-1) x    = -x

liftNum2Opt op f x y = liftNum2 op f x y

instance LiftNum (Const a) where
    liftNum Neg _ (RouC r) = RouC (r + 1 % 2)

    liftNum _  f (IntC x)      = IntC (f x)
    liftNum _  f (IntegerC x)  = IntegerC (f x)
    liftNum _  f (DoubleC x)   = DoubleC (f x)
    liftNum _  f (RationalC x) = RationalC (f x)
    liftNum _  f x@ComplexC{}  = fromComplex (f (lower x))
    liftNum op f c             = liftNum op f (flatten c)

    liftNum2 Mul _ (RouC x) (RouC y)            = normalize $ RouC (x + y)
    liftNum2 Mul f (RouC x) (ComplexC 1      0) = liftNum2Opt Mul f (RouC x) (RouC 0)
    liftNum2 Mul f (RouC x) (ComplexC (-1)   0) = liftNum2Opt Mul f (RouC x) (RouC (1 % 2))
    liftNum2 Mul f (RouC x) (ComplexC 0      1) = liftNum2Opt Mul f (RouC x) (RouC (1 % 4))
    liftNum2 Mul f (RouC x) (ComplexC 0   (-1)) = liftNum2Opt Mul f (RouC x) (RouC (3 % 4))
    liftNum2 Mul f x        y@RouC{}            = liftNum2Opt Mul f y x

    liftNum2 _  f (IntC x)      (IntC y)      = IntC (f x y)
    liftNum2 _  f (IntegerC x)  (IntegerC y)  = IntegerC (f x y)
    liftNum2 _  f (DoubleC x)   (DoubleC y)   = DoubleC (f x y)
    liftNum2 _  f (RationalC x) (RationalC y) = RationalC (f x y)
    liftNum2 _  f x@ComplexC{}  y@ComplexC{}  = fromComplex $ f (lower x) (lower y)
    liftNum2 op f x             y             = liftNum2 op f (flatten x) (flatten y)

--- XXX Need UndecidableInstances for this...but we *must* call
--- liftNumOpt/liftNum2Opt on constants to ensure they are properly simplified.
--- The other option would be to forgo the Const instance and push it into the
--- Exp instance.
instance (Num (Const a), LiftNum (Const a)) => LiftNum (Exp a) where
    liftNum op f (ConstE c) =
        ConstE $ liftNumOpt op f c

    liftNum op _ _  =
      errordoc $ text "liftNum (Exp a): cannot lift" <+> (text . show) op

    liftNum2 op f (ConstE c1) (ConstE c2) =
        ConstE $ liftNum2Opt op f c1 c2

    liftNum2 op _ _ _ =
        errordoc $ text "liftNum (Exp a): cannot lift" <+> (text . show) op

instance Num (Const Int) where
    (+) = liftNum2Opt Add (+)
    (-) = liftNum2Opt Sub (-)
    (*) = liftNum2Opt Mul (*)

    abs = liftNumOpt Abs abs

    negate = liftNumOpt Neg negate

    signum  = liftNumOpt Signum signum

    fromInteger = IntC . fromInteger

instance Num (Const Integer) where
    (+) = liftNum2Opt Add (+)
    (-) = liftNum2Opt Sub (-)
    (*) = liftNum2Opt Mul (*)

    abs = liftNumOpt Abs abs

    negate = liftNumOpt Neg negate

    signum  = liftNumOpt Signum signum

    fromInteger = IntegerC

instance Num (Const Double) where
    (+) = liftNum2Opt Add (+)
    (-) = liftNum2Opt Sub (-)
    (*) = liftNum2Opt Mul (*)

    abs = liftNumOpt Abs abs

    negate = liftNumOpt Neg negate

    signum  = liftNumOpt Signum signum

    fromInteger x = DoubleC (fromInteger x)

instance Num (Const Rational) where
    (+) = liftNum2Opt Add (+)
    (-) = liftNum2Opt Sub (-)
    (*) = liftNum2Opt Mul (*)

    abs = liftNumOpt Abs abs

    negate = liftNumOpt Neg negate

    signum  = liftNumOpt Signum signum

    fromInteger x = RationalC (fromInteger x)

instance Num (Const (Complex Double)) where
    (+) = liftNum2Opt Add (+)
    (-) = liftNum2Opt Sub (-)
    (*) = liftNum2Opt Mul (*)

    abs = liftNumOpt Abs abs

    negate = liftNumOpt Neg negate

    signum  = liftNumOpt Signum signum

    fromInteger x = fromComplex (fromInteger x)

instance Fractional (Const Double) where
    fromRational = DoubleC . fromRational

    x / y = DoubleC $ lower x / lower y

lift :: (Double -> Double) -> Const Double -> Const Double
lift f = DoubleC . f . lower

instance Floating (Const Double) where
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

instance Num (Exp Int) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = ConstE . IntC . fromInteger

instance Num (Exp Integer) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = ConstE . IntegerC

instance Num (Exp Double) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = ConstE . DoubleC . fromInteger

instance Num (Exp Rational) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = ConstE . RationalC . fromInteger

instance Num (Exp (Complex Double)) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger x = complexE (fromInteger x)

intE :: Int -> Exp Int
intE = ConstE . IntC

complexE :: Complex Double -> Exp (Complex Double)
complexE (r :+ i) = ConstE $ ComplexC (DoubleC r) (DoubleC i)
