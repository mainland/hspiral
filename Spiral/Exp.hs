{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

    Type(..),
    Typed(..),

    fromComplex,
    flatten,

    LiftNum(..),
    liftNumOpt,
    liftNum2Opt,

    rootOfUnity,

    intE,
    complexE,
    unComplexE
  ) where

import Data.Complex
import Data.Ratio ((%),
                   denominator,
                   numerator)
import Data.String
import Data.Symbol
import Language.C.Quote (ToIdent(..))
import Test.QuickCheck (Arbitrary(..))
import Text.PrettyPrint.Mainland hiding (flatten)

import Spiral.Driver.Globals
import Spiral.ExtendedFloat
import Spiral.Util.Name
import Spiral.Util.Pretty
import Spiral.Util.Uniq

newtype Var = Var { unVar :: Name }
  deriving (Eq, Ord, Show)

instance Pretty Var where
    ppr v = ppr (unVar v)

instance IsString Var where
    fromString s = Var (fromString s)

instance Gensym Var where
    gensymAt s _ = do
        u <- newUnique
        return $ Var $ Name (intern s) (Just u)

    uniquify (Var (Name sym _)) = do
        u <- newUnique
        return $ Var $ Name sym (Just u)

instance ToIdent Var where
    toIdent v = toIdent (unVar v)

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

-- | Lower a 'Const a' to the value of type 'a' that it represents.
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

instance ExtendedFloat (Const (Complex Double)) where
    rootOfUnity = normalize . RouC

pprComplex :: (Eq a, Num a, Pretty a) => Complex a -> Doc
pprComplex (r :+ 0) = ppr r
pprComplex (r :+ i) = ppr r <+> text "+" <+> pprPrec appPrec1 i <> char 'i'

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

-- | Representation of scalar constants.
data Exp a where
    ConstE :: Const a -> Exp a
    VarE   :: Var -> Exp a
    UnopE  :: Unop -> Exp a -> Exp a
    BinopE :: Binop -> Exp a -> Exp a -> Exp a
    IdxE   :: Var -> [Exp Int] -> Exp a

    ComplexE :: (Typed a, Num (Exp a)) => Exp a -> Exp a -> Exp (Complex a)
    ReE :: Exp (Complex a) -> Exp a
    ImE :: Exp (Complex a) -> Exp a

deriving instance Eq (Exp a)
deriving instance Ord (Exp a)
deriving instance Show (Exp a)

instance ExtendedFloat (Exp (Complex Double)) where
    rootOfUnity = ConstE . rootOfUnity

-- | Unary operators
data Unop = Neg
          | Abs
          | Signum
  deriving (Eq, Ord, Show, Enum)

-- | Binary operators
data Binop = Add
           | Sub
           | Mul
           | Quot
           | Rem
           | Div
  deriving (Eq, Ord, Show, Enum)

instance HasFixity Unop where
    fixity Neg    = infixr_ 10
    fixity Abs    = infixr_ 10
    fixity Signum = infixr_ 10

instance HasFixity Binop where
    fixity Add  = infixl_ 8
    fixity Sub  = infixl_ 8
    fixity Mul  = infixl_ 9
    fixity Quot = infixl_ 9
    fixity Rem  = infixl_ 9
    fixity Div  = infixl_ 9

instance Pretty Unop where
    ppr Neg    = text "-"
    ppr Abs    = text "abs" <> space
    ppr Signum = text "signum" <> space

instance Pretty Binop where
    ppr Add  = text "+"
    ppr Sub  = text "-"
    ppr Mul  = text "*"
    ppr Quot = text "`div`"
    ppr Rem  = text "`mod`"
    ppr Div  = text "/"

instance Pretty (Exp a) where
    pprPrec p (ConstE c) = pprPrec p c
    pprPrec p (VarE v)   = pprPrec p v

    pprPrec p (UnopE op e) =
        parensIf (p > precOf op) $
        ppr op <> pprPrec (precOf op) e

    pprPrec p (BinopE op e1 e2) =
        infixop p op e1 e2

    pprPrec _ (IdxE ev eis) =
        ppr ev <> mconcat [brackets (ppr ei) | ei <- eis]

    pprPrec p (ComplexE er ei) =
        parensIf (p > addPrec) $
        pprComplex (er :+ ei)

    pprPrec p (ReE e) =
        parensIf (p > appPrec) $
        text "re" <+> pprPrec appPrec1 e

    pprPrec p (ImE e) =
        parensIf (p > appPrec) $
        text "im" <+> pprPrec appPrec1 e

-- | Representation of types.
data Type = IntT
          | IntegerT
          | DoubleT
          | ComplexT Type
  deriving (Eq, Ord, Show)

class Typed a where
    typeOf :: a -> Type

instance Typed Int where
    typeOf _ = IntT

instance Typed Integer where
    typeOf _ = IntegerT

instance Typed Double where
    typeOf _ = DoubleT

instance Typed a => Typed (Complex a) where
    typeOf _ = ComplexT (typeOf (undefined :: a))

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
    liftNum Neg _ (ComplexC a b) = ComplexC (-a) (-b)
    liftNum Neg _ (RouC r)       = RouC (r + 1 % 2)

    liftNum _  f (IntC x)      = IntC (f x)
    liftNum _  f (IntegerC x)  = IntegerC (f x)
    liftNum _  f (DoubleC x)   = DoubleC (f x)
    liftNum _  f (RationalC x) = RationalC (f x)
    liftNum _  f x@ComplexC{}  = fromComplex (f (lower x))
    liftNum op f c             = liftNum op f (flatten c)

    liftNum2 Add _ (ComplexC a b) (ComplexC c d) = ComplexC (a + c) (b + d)
    liftNum2 Sub _ (ComplexC a b) (ComplexC c d) = ComplexC (a - c) (b - d)
    liftNum2 Mul _ (ComplexC a b) (ComplexC c d) = ComplexC (a*c - b*d) (b*c + a*d)

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
    liftNum op f (ConstE c) = ConstE $ liftNumOpt op f c

    liftNum Neg _ (UnopE Neg x)  = x
    liftNum Neg _ (ComplexE a b) = ComplexE (-a) (-b)

    liftNum op _ e  = UnopE op e

    liftNum2 op f (ConstE c1) (ConstE c2) = ConstE $ liftNum2Opt op f c1 c2

    liftNum2 Add _ (ComplexE a b) (ComplexE c d) = ComplexE (a + c) (b + d)
    liftNum2 Sub _ (ComplexE a b) (ComplexE c d) = ComplexE (a - c) (b - d)
    liftNum2 Mul _ (ComplexE a b) (ComplexE c d) = ComplexE (a*c - b*d) (b*c + a*d)

    -- If we aren't using an explicit complex type in the code generator, then
    -- we want to make sure that both arguments to the operator have explicit
    -- real and imaginary parts so that we can cache them separately.
    liftNum2 Add f x@ComplexE{} y | not useComplexType = liftNum2 Add f x (mkComplexE y)
    liftNum2 Add f x y@ComplexE{} | not useComplexType = liftNum2 Add f (mkComplexE x) y
    liftNum2 Sub f x@ComplexE{} y | not useComplexType = liftNum2 Sub f x (mkComplexE y)
    liftNum2 Sub f x y@ComplexE{} | not useComplexType = liftNum2 Sub f (mkComplexE x) y
    liftNum2 Mul f x@ComplexE{} y | not useComplexType = liftNum2 Mul f x (mkComplexE y)
    liftNum2 Mul f x y@ComplexE{} | not useComplexType = liftNum2 Mul f (mkComplexE x) y

    liftNum2 op _ e1 e2 = BinopE op e1 e2

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

-- | Ensure that a complex expression is represented using its constituent real
-- and imaginary parts.
mkComplexE :: (Typed a, Num (Exp a)) => Exp (Complex a) -> Exp (Complex a)
mkComplexE e = ComplexE er ei
  where
    (er, ei) = unComplexE e

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

    cos x = lift cos x

instance Num (Exp Int) where
    (+) = liftNum2Opt Add (+)
    (-) = liftNum2Opt Sub (-)
    (*) = liftNum2Opt Mul (*)

    abs = liftNumOpt Abs abs

    negate = liftNumOpt Neg negate

    signum  = liftNumOpt Signum signum

    fromInteger = ConstE . IntC . fromInteger

instance Num (Exp Integer) where
    (+) = liftNum2Opt Add (+)
    (-) = liftNum2Opt Sub (-)
    (*) = liftNum2Opt Mul (*)

    abs = liftNumOpt Abs abs

    negate = liftNumOpt Neg negate

    signum  = liftNumOpt Signum signum

    fromInteger = ConstE . IntegerC

instance Num (Exp Double) where
    (+) = liftNum2Opt Add (+)
    (-) = liftNum2Opt Sub (-)
    (*) = liftNum2Opt Mul (*)

    abs = liftNumOpt Abs abs

    negate = liftNumOpt Neg negate

    signum  = liftNumOpt Signum signum

    fromInteger = ConstE . DoubleC . fromInteger

instance Num (Exp Rational) where
    (+) = liftNum2Opt Add (+)
    (-) = liftNum2Opt Sub (-)
    (*) = liftNum2Opt Mul (*)

    abs = liftNumOpt Abs abs

    negate = liftNumOpt Neg negate

    signum  = liftNumOpt Signum signum

    fromInteger = ConstE . RationalC . fromInteger

instance Num (Exp (Complex Double)) where
    (+) = liftNum2Opt Add (+)
    (-) = liftNum2Opt Sub (-)
    (*) = liftNum2Opt Mul (*)

    abs = liftNumOpt Abs abs

    negate = liftNumOpt Neg negate

    signum  = liftNumOpt Signum signum

    fromInteger x = complexE (fromInteger x)

instance Enum (Exp Int) where
    toEnum n = intE (fromIntegral n)

    fromEnum (ConstE (IntC i)) = fromIntegral i
    fromEnum _                 = error "Enum Exp: fromEnum not implemented"

instance Real (Exp Int) where
    toRational (ConstE (IntC i)) = toRational i
    toRational _                 = error "Real Exp: toRational not implemented"

instance Integral (Exp Int) where
    ConstE (IntC x) `quotRem` ConstE (IntC y) = (intE q, intE r)
      where
        (q, r) = x `quotRem` y

    x `quotRem` y =
        (BinopE Quot x y, BinopE Rem x y)

    toInteger (ConstE (IntC i)) = fromIntegral i
    toInteger _                 = error "Integral Exp: toInteger not implemented"

intE :: Int -> Exp Int
intE = ConstE . IntC

complexE :: Complex Double -> Exp (Complex Double)
complexE (r :+ i) = ConstE $ ComplexC (DoubleC r) (DoubleC i)

unComplexE :: Exp (Complex a) -> (Exp a, Exp a)
unComplexE (ConstE (ComplexC r i)) = (ConstE r, ConstE i)
unComplexE (ConstE (RouC r))       = (ConstE (cos (PiC (2*r))), ConstE (sin (PiC (2*r))))
unComplexE (ComplexE r i )         = (r, i)
unComplexE e                       = (ReE e, ImE e)
