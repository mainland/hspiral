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
-- Copyright   :  (c) 2016-2017 Drexel University
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

    simplify,

    LiftNum(..),

    rootOfUnity,

    intE,
    complexE,
    ensureComplexE,
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

import Data.Heterogeneous
import Spiral.ExtendedFloat
import Spiral.Globals
import Spiral.Util.Name
import Spiral.Util.Pretty
import Spiral.Util.Uniq

newtype Var = Var Name
  deriving (Eq, Ord, Show, IsString, ToIdent, Pretty, Named)

instance Gensym Var where
    gensymAt s _ = do
        u <- newUnique
        return $ Var $ Name (intern s) (Just u)

    uniquify (Var (Name sym _)) = do
        u <- newUnique
        return $ Var $ Name sym (Just u)

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

lift :: (a -> a) -> Const a -> Const a
lift f (IntC x)      = IntC (f x)
lift f (IntegerC x)  = IntegerC (f x)
lift f (DoubleC x)   = DoubleC (f x)
lift f (RationalC x) = RationalC (f x)
lift f x@ComplexC{}  = fromComplex (f (lower x))
lift f x             = lift f (flatten x)

lift2 :: (a -> a -> a) -> Const a -> Const a -> Const a
lift2 f (IntC x)      (IntC y)      = IntC (f x y)
lift2 f (IntegerC x)  (IntegerC y)  = IntegerC (f x y)
lift2 f (DoubleC x)   (DoubleC y)   = DoubleC (f x y)
lift2 f (RationalC x) (RationalC y) = RationalC (f x y)
lift2 f x@ComplexC{}  y@ComplexC{}  = fromComplex (f (lower x) (lower y))
lift2 f x             y             = lift2 f (flatten x) (flatten y)

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
    pprPrec _ (IntC x)      = ppr x
    pprPrec _ (IntegerC x)  = ppr x
    pprPrec _ (DoubleC x)   = ppr x
    pprPrec _ (RationalC x) = ppr x

    pprPrec p x@(ComplexC r i)
        | r == 0 && i == 0    = char '0'
        | r == 0 && i == 1    = char 'i'
        | r == 0 && i == (-1) = text "-i"
        | r == 0              = ppr i <> char 'i'
        | i == 0              = ppr r
        | otherwise           = pprComplex p (lower x)

    pprPrec _ (RouC r)
        | r < 0     = text "exp" <> parens (char '-' <> go (negate r))
        | otherwise = text "exp" <> parens (go r)
      where
        go r = text "2*pi*i*" <>
               ppr (numerator r) <>
               char '/' <>
               ppr (denominator r)

    pprPrec _ (PiC r) = pprPrec mulPrec1 r <> char '*' <> text "pi"

instance ExtendedFloat (Const (Complex Double)) where
    rootOfUnity = normalize . RouC

pprComplex :: (Eq a, Num a, Pretty a) => Int -> Complex a -> Doc
pprComplex _ (r :+ 0)    = ppr r
pprComplex _ (0 :+ 1)    = char 'i'
pprComplex _ (0 :+ (-1)) = text "-i"
pprComplex _ (0 :+ i)    = pprPrec mulPrec i <> char 'i'
pprComplex p (r :+ i)    = parensIf (p > addPrec) $
                           pprPrec addPrec r <+> text "+" <+> pprPrec mulPrec i <> char 'i'

-- | Convert a 'Complex Double' to a 'Constant'
fromComplex :: Complex Double -> Const (Complex Double)
fromComplex (r :+ i) = ComplexC (DoubleC r) (DoubleC i)

-- | Normalize an expression's representation.
normalize :: Const a -> Const a
normalize c@(RouC r)
    | r >= 1 || r < -1 = normalize (RouC ((n `rem` d) % d))
    | r < 0            = normalize (RouC (1 + r))
    | r == 0           = ComplexC 1 0
    | r == 1 % 4       = ComplexC 0 1
    | r == 1 % 2       = ComplexC (-1) 0
    | r == 3 % 4       = ComplexC 0 (-1)
    | otherwise        = c
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
    ReE :: Num (Exp a) => Exp (Complex a) -> Exp a
    ImE :: Num (Exp a) => Exp (Complex a) -> Exp a

deriving instance Eq (Exp a)
deriving instance Ord (Exp a)
--deriving instance Show (Exp a)

instance Show (Exp a) where
    showsPrec p = displayS . renderCompact . pprPrec p

instance IsString (Exp a) where
    fromString s = VarE (fromString s)

instance ExtendedFloat (Exp (Complex Double)) where
    rootOfUnity = ConstE . rootOfUnity

--
-- Heterogeneous quality and comparison
--

instance HEq Const where
    heq (IntC x)         (IntC y)         = x == y
    heq (IntegerC x)     (IntegerC y)     = x == y
    heq (DoubleC x)      (DoubleC y)      = x == y
    heq (RationalC x)    (RationalC y)    = x == y
    heq (ComplexC r1 i1) (ComplexC r2 i2) = r1 == r2 && i1 == i2
    heq (RouC x)         (RouC y)         = x == y
    heq (PiC x)          (PiC y)          = x == y
    heq _                _                = False

instance HEq Exp where
    heq (ConstE c1)          (ConstE c2)          = heq c1 c2
    heq (VarE x)             (VarE y)             = x == y
    heq (UnopE op1 e1)       (UnopE op2 e2)       = op1 == op2 && heq e1 e2
    heq (BinopE op1 e1a e1b) (BinopE op2 e2a e2b) = op1 == op2 && heq e1a e2a && heq e1b e2b
    heq (IdxE v1 es1)        (IdxE v2 es2)        = v1 == v2 && es1 == es2
    heq (ComplexE r1 i1)     (ComplexE r2 i2)     = heq r1 r2 && heq i1 i2
    heq (ReE e1)             (ReE e2)             = heq e1 e2
    heq (ImE e1)             (ImE e2)             = heq e1 e2
    heq _                    _                    = False

instance HOrd Const where
    hcompare (IntC x)         (IntC y)         = compare x y
    hcompare IntC{}           _                = LT

    hcompare IntegerC{}       IntC{}           = GT
    hcompare (IntegerC x)     (IntegerC y)     = compare x y
    hcompare IntegerC{}       _                = LT

    hcompare DoubleC{}        IntC{}           = GT
    hcompare DoubleC{}        IntegerC{}       = GT
    hcompare (DoubleC x)      (DoubleC y)      = compare x y
    hcompare DoubleC{}        _                = LT

    hcompare RationalC{}      IntC{}           = GT
    hcompare RationalC{}      IntegerC{}       = GT
    hcompare RationalC{}      DoubleC{}        = GT
    hcompare (RationalC x)    (RationalC y)    = compare x y
    hcompare RationalC{}      _                = LT

    hcompare ComplexC{}       IntC{}           = GT
    hcompare ComplexC{}       IntegerC{}       = GT
    hcompare ComplexC{}       DoubleC{}        = GT
    hcompare ComplexC{}       RationalC{}      = GT
    hcompare (ComplexC r1 i1) (ComplexC r2 i2) = compare (r1, i1) (r2, i2)
    hcompare ComplexC{}       _                = LT

    hcompare RouC{}           IntC{}           = GT
    hcompare RouC{}           IntegerC{}       = GT
    hcompare RouC{}           DoubleC{}        = GT
    hcompare RouC{}           RationalC{}      = GT
    hcompare RouC{}           ComplexC{}       = GT
    hcompare (RouC x)         (RouC y)         = compare x y
    hcompare RouC{}           _                = LT

    hcompare (PiC x)          (PiC y)          = compare x y
    hcompare PiC{}            _                = GT

instance HOrd Exp where
    hcompare (ConstE c1)          (ConstE c2)          = hcompare c1 c2
    hcompare ConstE{}             _                    = LT

    hcompare VarE{}               ConstE{}             = GT
    hcompare (VarE x)             (VarE y)             = compare x y
    hcompare VarE{}               _                    = LT

    hcompare UnopE{}              ConstE{}             = GT
    hcompare UnopE{}              VarE{}               = GT
    hcompare (UnopE op1 e1)       (UnopE op2 e2)       = case compare op1 op2 of
                                                           EQ -> hcompare e1 e2
                                                           other -> other
    hcompare UnopE{}              _                    = LT

    hcompare BinopE{}             ConstE{}             = GT
    hcompare BinopE{}             VarE{}               = GT
    hcompare BinopE{}             UnopE{}              = GT
    hcompare (BinopE op1 e1a e1b) (BinopE op2 e2a e2b) = case compare op1 op2 of
                                                           EQ -> case hcompare e1a e2a of
                                                                   EQ -> hcompare e1b e2b
                                                                   other -> other
                                                           other -> other
    hcompare BinopE{}             _                    = LT

    hcompare IdxE{}               ConstE{}             = GT
    hcompare IdxE{}               VarE{}               = GT
    hcompare IdxE{}               UnopE{}              = GT
    hcompare IdxE{}               BinopE{}             = GT
    hcompare (IdxE v1 es1)        (IdxE v2 es2)        = case compare v1 v2 of
                                                           EQ -> compare es1 es2
                                                           other -> other
    hcompare IdxE{}               _                    = LT

    hcompare ComplexE{}           ConstE{}             = GT
    hcompare ComplexE{}           VarE{}               = GT
    hcompare ComplexE{}           UnopE{}              = GT
    hcompare ComplexE{}           BinopE{}             = GT
    hcompare ComplexE{}           IdxE{}               = GT
    hcompare (ComplexE r1 i1)     (ComplexE r2 i2)     = case hcompare r1 r2 of
                                                           EQ -> hcompare i1 i2
                                                           other -> other
    hcompare ComplexE{}           _                    = LT

    hcompare ReE{}                ConstE{}             = GT
    hcompare ReE{}                VarE{}               = GT
    hcompare ReE{}                UnopE{}              = GT
    hcompare ReE{}                BinopE{}             = GT
    hcompare ReE{}                IdxE{}               = GT
    hcompare ReE{}                ComplexE{}           = GT
    hcompare (ReE e1)             (ReE e2)             = hcompare e1 e2
    hcompare ReE{}                _                    = LT

    hcompare (ImE e1)             (ImE e2)             = hcompare e1 e2
    hcompare ImE{}                _                    = GT

-- | Unary operators
data Unop = Neg
          | Abs
          | Signum
          | Pow Integer
  deriving (Eq, Ord, Show)

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
    fixity Pow{}  = infixr_ 10

instance HasFixity Binop where
    fixity Add  = infixl_ 8
    fixity Sub  = infixl_ 8
    fixity Mul  = infixl_ 9
    fixity Quot = infixl_ 9
    fixity Rem  = infixl_ 9
    fixity Div  = infixl_ 9

instance Pretty Unop where
    ppr Neg     = text "-"
    ppr Abs     = text "abs" <> space
    ppr Signum  = text "signum" <> space
    ppr (Pow n) = char '^' <> ppr n

instance Pretty Binop where
    ppr Add  = text "+"
    ppr Sub  = text "-"
    ppr Mul  = text "*"
    ppr Quot = text "`quot`"
    ppr Rem  = text "`rem`"
    ppr Div  = text "/"

instance Pretty (Exp a) where
    pprPrec p (ConstE c) = pprPrec p c
    pprPrec p (VarE v)   = pprPrec p v

    pprPrec p (UnopE op@(Pow n) e) =
        parensIf (p > opPrec) $
        pprPrec (opPrec+1) e <> char '^' <> ppr n
      where
        opPrec = precOf op

    pprPrec p (UnopE op e) =
        unop p op e

    pprPrec p (BinopE op e1 e2) =
        infixop p op e1 e2

    pprPrec _ (IdxE ev eis) =
        ppr ev <> mconcat [brackets (ppr ei) | ei <- eis]

    pprPrec p (ComplexE er ei) =
        pprComplex p (er :+ ei)

    pprPrec _ (ReE e) =
        text "re" <> parens (ppr e)

    pprPrec _ (ImE e) =
        text "im" <> parens (ppr e)

-- | Representation of types.
data Type a where
    IntT     :: Type Int
    IntegerT :: Type Integer
    DoubleT  :: Type Double
    ComplexT :: (Typed a, Num (Exp a)) => Type a -> Type (Complex a)

deriving instance Eq (Type a)
deriving instance Ord (Type a)
deriving instance Show (Type a)

instance Pretty (Type a) where
    ppr IntT           = text "int"
    ppr IntegerT       = text "integer"
    ppr DoubleT        = text "double"
    ppr (ComplexT tau) = text "complex" <+> ppr tau

class Typed a where
    typeOf :: a -> Type a

instance Typed Int where
    typeOf _ = IntT

instance Typed Integer where
    typeOf _ = IntegerT

instance Typed Double where
    typeOf _ = DoubleT

instance (Typed a, Num (Exp a)) => Typed (Complex a) where
    typeOf _ = ComplexT (typeOf (undefined :: a))

--------------------------------------------------------------------------------
--
-- Simplification
--
--------------------------------------------------------------------------------

simplify :: forall a . Num (Exp a) => Exp a -> Exp a
simplify (BinopE op e1 e2) = go op (simplify e1) (simplify e2)
  where
    go :: Binop -> Exp a -> Exp a -> Exp a
    go Add e1 (UnopE Neg e2) =
        simplify (e1 - e2)

    go Sub e1 (UnopE Neg e2) =
        simplify (e1 + e2)

    go Add e1 (BinopE Mul c2 e2) | isNeg c2 =
        simplify (e1 - ((-c2) * e2))

    go Sub e1 (BinopE Mul c2 e2) | isNeg c2 =
        simplify (e1 + ((-c2) * e2))

    go Add (BinopE Mul c1 e1) (BinopE Mul c2 e2)
      | Just x1 <- fromDouble c1, Just x2 <- fromDouble c2, x1 ~== x2 =
        simplify (c1 * simplify (e1 + e2))

    go Sub (BinopE Mul c1 e1) (BinopE Mul c2 e2)
      | Just x1 <- fromDouble c1, Just x2 <- fromDouble c2, x1 ~== x2 =
        simplify (c1 * simplify (e1 - e2))

    go Add (BinopE Mul c1 e1) (BinopE Mul c2 e2)
      | Just x1 <- fromDouble c1, Just x2 <- fromDouble c2, x1 < 0, x1 ~== -x2 =
        simplify (c1 * simplify (e1 - e2))

    go Sub (BinopE Mul c1 e1) (BinopE Mul c2 e2)
      | Just x1 <- fromDouble c1, Just x2 <- fromDouble c2, x1 < 0, x1 ~== -x2 =
        simplify (c1 * simplify (e1 + e2))

    -- When multiplying by a constant, make constant the first term
    go Mul e1 e2@ConstE{} =
        go Mul e2 e1

    go Mul c1@ConstE{} (BinopE Sub e1 e2) | isNeg c1 =
        simplify ((-c1) * (e2 - e1))

    go op e1 e2 =
        BinopE op e1 e2

simplify (UnopE op e) = go op (simplify e)
  where
    go :: Unop -> Exp a -> Exp a
    go Neg (UnopE Neg e) =
        simplify e

    go Neg (BinopE Sub e1 e2) =
        simplify (e2 - e1)

    go op e =
        UnopE op e

simplify e = e

fromDouble :: Exp a -> Maybe Double
fromDouble (ConstE (DoubleC x)) = return x
fromDouble (ConstE (PiC c))     = return (pi*fromRational c)
fromDouble _                    = fail "Not a double constant"

isNeg :: Exp a -> Bool
isNeg e | Just c <- fromDouble e = c < 0
isNeg _                          = False

infix 4 ~==

-- | Return 'True' is two numbers are approximately equal
(~==) :: (Ord a, Fractional a) => a -> a -> Bool
x ~== y = abs (x - y) < eps
  where
    eps = 1e-15

--------------------------------------------------------------------------------
--
-- Num instances
--
--------------------------------------------------------------------------------

-- | Class to lift 'Num' operators.
class LiftNum b where
    -- | Lift a unary operation on 'Num' to the type 'b'
    liftNum :: Unop -> (forall a . Num a => a -> a) -> b -> b

    -- | Lift a binary operation on 'Num' to the type 'b
    liftNum2 :: Binop -> (forall a . Num a => a -> a -> a) -> b -> b -> b

instance (Num a, Num (Const a)) => LiftNum (Const a) where
    liftNum Neg _ e
      | e == 0 = 0

    liftNum Neg _ (ComplexC a b) = ComplexC (-a) (-b)
    liftNum Neg _ (RouC r)       = normalize $ RouC (r + 1 % 2)
    liftNum Neg _ (PiC r)        = PiC (-r)

    liftNum _op f c = lift f (flatten c)

    liftNum2 Add _ e1 e2
      | e1 == 0 = e2
      | e2 == 0 = e1

    liftNum2 Sub _ e1 e2
      | e1 == 0 = -e2
      | e2 == 0 = e1

    liftNum2 Mul _ e1 e2
      | e1 ==  0 = 0
      | e2 ==  0 = 0
      | e1 ==  1 = e2
      | e2 ==  1 = e1
      | e1 == -1 = -e2
      | e2 == -1 = -e1

    liftNum2 Add _ (ComplexC a b) (ComplexC c d) = ComplexC (a + c) (b + d)
    liftNum2 Sub _ (ComplexC a b) (ComplexC c d) = ComplexC (a - c) (b - d)
    liftNum2 Mul _ (ComplexC a b) (ComplexC c d) = ComplexC (a*c - b*d) (b*c + a*d)

    liftNum2 Mul _ (RouC x) (RouC y)            = normalize $ RouC (x + y)
    liftNum2 Mul f (RouC x) (ComplexC 1      0) = liftNum2 Mul f (RouC x) (RouC 0)
    liftNum2 Mul f (RouC x) (ComplexC (-1)   0) = liftNum2 Mul f (RouC x) (RouC (1 % 2))
    liftNum2 Mul f (RouC x) (ComplexC 0      1) = liftNum2 Mul f (RouC x) (RouC (1 % 4))
    liftNum2 Mul f (RouC x) (ComplexC 0   (-1)) = liftNum2 Mul f (RouC x) (RouC (3 % 4))
    liftNum2 Mul f x        y@RouC{}            = liftNum2 Mul f y x

    liftNum2 _op f x y = lift2 f (flatten x) (flatten y)

--- XXX Need UndecidableInstances for this...but we *must* call
--- liftNum/liftNum2 on constants to ensure they are properly simplified.
--- The other option would be to forgo the Const instance and push it into the
--- Exp instance.
instance (Num (Const a), LiftNum (Const a), Num (Exp a)) => LiftNum (Exp a) where
    liftNum op f (ConstE c) = ConstE $ liftNum op f c

    liftNum Neg _ e
      | e == 0 = 0

    liftNum Neg _ (UnopE Neg x)  = x
    liftNum Neg _ (ComplexE a b) = ComplexE (-a) (-b)

    liftNum op _ e = UnopE op e

    liftNum2 op f (ConstE c1) (ConstE c2) = ConstE $ liftNum2 op f c1 c2

    liftNum2 Add _ e1 e2
      | e1 == 0 = e2
      | e2 == 0 = e1

    liftNum2 Sub _ e1 e2
      | e1 == 0 = -e2
      | e2 == 0 = e1

    liftNum2 Mul _ e1 e2
      | e1 ==  0 = 0
      | e2 ==  0 = 0
      | e1 ==  1 = e2
      | e2 ==  1 = e1
      | e1 == -1 = -e2
      | e2 == -1 = -e1

    liftNum2 Mul _ e1 e2 | Just (x, n) <- fromPow e1, Just (y, m) <- fromPow e2, x == y =
        UnopE (Pow (n+m)) x
      where
        fromPow :: Exp a -> Maybe (Exp a, Integer)
        fromPow (UnopE (Pow n) x@VarE{})                = return (x, n)
        fromPow (BinopE Div 1 (UnopE (Pow n) x@VarE{})) = return (x, -n)
        fromPow x@VarE{}                                = return (x, 1)
        fromPow (BinopE Div 1 x@VarE{})                 = return (x, -1)
        fromPow _                                       = fail "Can't destruct power"

    liftNum2 Add _ (ComplexE a b) (ComplexE c d) = ComplexE (a + c) (b + d)
    liftNum2 Sub _ (ComplexE a b) (ComplexE c d) = ComplexE (a - c) (b - d)

    liftNum2 Mul _ (ComplexE a b) (ComplexE c d)
      | minimizeAdds = ComplexE (a*c - b*d) (b*c + a*d)
      | otherwise    = ComplexE (t1 - t2) (t1 + t3)
      where
        t1 = a*(c+d)
        t2 = d*(b+a)
        t3 = c*(b-a)

    -- If we aren't using an explicit complex type in the code generator, then
    -- we want to make sure that both arguments to the operator have explicit
    -- real and imaginary parts so that we can cache them separately.
    liftNum2 Add _ x@ComplexE{} y | not useComplexType = x + ensureComplexE y
    liftNum2 Add _ x y@ComplexE{} | not useComplexType = ensureComplexE x + y
    liftNum2 Sub _ x@ComplexE{} y | not useComplexType = x - ensureComplexE y
    liftNum2 Sub _ x y@ComplexE{} | not useComplexType = ensureComplexE x - y
    liftNum2 Mul _ x@ComplexE{} y | not useComplexType = x * ensureComplexE y
    liftNum2 Mul _ x y@ComplexE{} | not useComplexType = ensureComplexE x * y

    liftNum2 Add _ e1 (UnopE Neg e2) = e1 - e2
    liftNum2 Sub _ e1 (UnopE Neg e2) = e1 + e2

    liftNum2 op _ e1 e2 = BinopE op e1 e2

instance Num (Const Int) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = IntC . fromInteger

instance Num (Const Integer) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = IntegerC

instance Num (Const Double) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger x = DoubleC (fromInteger x)

instance Num (Const Rational) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger x = RationalC (fromInteger x)

instance Num (Const (Complex Double)) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger x = fromComplex (fromInteger x)

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

--------------------------------------------------------------------------------
--
-- Enum/Real/Integral instances
--
--------------------------------------------------------------------------------

-- | Class to lift 'Integral' operators.
class LiftIntegral b where
    -- | Lift a binary operation on 'LiftIntegral' to the type 'b
    liftIntegral2 :: Binop -> (forall a . Integral a => a -> a -> a) -> b -> b -> b

instance LiftIntegral (Const Int) where
    liftIntegral2 _ f (IntC x) (IntC y) = IntC (f x y)

instance Enum (Const Int) where
    toEnum n          = IntC (fromIntegral n)
    fromEnum (IntC i) = fromIntegral i

instance Real (Const Int) where
    toRational (IntC i) = toRational i

instance Integral (Const Int) where
    quot = liftIntegral2 Quot quot
    rem  = liftIntegral2 Rem rem

    x `quotRem` y = (x `quot` y, x `rem` y)

    toInteger (IntC i) = fromIntegral i

instance (LiftIntegral (Const a), Integral (Exp a)) => LiftIntegral (Exp a) where
    liftIntegral2 op f (ConstE x) (ConstE y) = ConstE $ liftIntegral2 op f x y

    liftIntegral2 Quot _ (BinopE Mul  e1 e2) e2' | e2' == e2 = e1
    liftIntegral2 Rem  _ (BinopE Mul _e1 e2) e2' | e2' == e2 = 0

    liftIntegral2 Quot _ (BinopE Add (BinopE Mul  e1 e2) e3) e2' | e2' == e2 = e1 + e3 `quot` e2
    liftIntegral2 Rem  _ (BinopE Add (BinopE Mul _e1 e2) e3) e2' | e2' == e2 = e3 `rem` e2

    liftIntegral2 op _ e1 e2 = BinopE op e1 e2

instance Enum (Exp Int) where
    toEnum n = intE (fromIntegral n)

    fromEnum (ConstE (IntC i)) = fromIntegral i
    fromEnum _                 = error "Enum Exp: fromEnum not implemented"

instance Real (Exp Int) where
    toRational (ConstE (IntC i)) = toRational i
    toRational _                 = error "Real Exp: toRational not implemented"

instance Integral (Exp Int) where
    quot = liftIntegral2 Quot quot
    rem  = liftIntegral2 Rem rem

    x `quotRem` y = (x `quot` y, x `rem` y)

    toInteger (ConstE (IntC i)) = fromIntegral i
    toInteger _                 = error "Integral Exp: toInteger not implemented"

--------------------------------------------------------------------------------
--
-- Fractional/Floating instances
--
--------------------------------------------------------------------------------

-- | Class to lift 'Fractional' operators.
class LiftFrac b where
    -- | Lift a binary operation on 'Num' to the type 'b
    liftFrac2 :: Binop -> (forall a . Fractional a => a -> a -> a) -> b -> b -> b

instance (Fractional a, Num (Const a)) => LiftFrac (Const a) where
    liftFrac2 Div _ c1 1    = c1
    liftFrac2 Div _ c1 (-1) = -c1

    liftFrac2 Div _ 1 (RouC y) = normalize $ RouC (-y)

    liftFrac2 _op f c1 c2 = lift2 f c1 c2

instance (LiftFrac (Const a), Num (Exp a)) => LiftFrac (Exp a) where
    liftFrac2 op f (ConstE c1) (ConstE c2) = ConstE $ liftFrac2 op f c1 c2

    liftFrac2 Div _ e 1    = e
    liftFrac2 Div _ e (-1) = -e

    liftFrac2 Div _ 1 x@VarE{}                 = UnopE (Pow (-1)) x
    liftFrac2 Div _ 1 (UnopE (Pow n) x@VarE{}) = UnopE (Pow (-n)) x

    liftFrac2 op _ e1 e2 = BinopE op e1 e2

instance Fractional (Const Double) where
    (/) = liftFrac2 Div (/)

    fromRational = DoubleC . fromRational

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
      | x < 0     = cos (PiC (-x))
      | x > 1     = cos (PiC (x - 2))
      | x > 1/2   = -cos (PiC (1 - x))
      | x == 0    = 1
      | x == 1/2  = 0

    cos x = lift cos x

instance Fractional (Const (Complex Double)) where
    (/) = liftFrac2 Div (/)

    fromRational x = ComplexC (DoubleC (fromRational x)) 0

instance Fractional (Exp Double) where
    (/) = liftFrac2 Div (/)

    fromRational = ConstE . DoubleC . fromRational

instance Fractional (Exp (Complex Double)) where
    (/) = liftFrac2 Div (/)

    fromRational x = ConstE (ComplexC (fromRational x) 0)

--------------------------------------------------------------------------------
--
-- Smart constructors
--
--------------------------------------------------------------------------------

-- | Create an 'Exp Int' from an 'Int'.
intE :: Int -> Exp Int
intE = ConstE . IntC

-- | Create an 'Exp (Complex Double)' from a 'Complex Double'.
complexE :: Complex Double -> Exp (Complex Double)
complexE (r :+ i) = ConstE $ ComplexC (DoubleC r) (DoubleC i)

-- | Ensure that a complex expression is represented using its constituent real
-- and imaginary parts.
ensureComplexE :: (Typed a, Num (Exp a)) => Exp (Complex a) -> Exp (Complex a)
ensureComplexE e = ComplexE er ei
  where
    (er, ei) = unComplexE e

-- | Extract the complex and real portions of an 'Exp (Complex a)'.
unComplexE :: Num (Exp a) => Exp (Complex a) -> (Exp a, Exp a)
unComplexE (ConstE (ComplexC r i)) = (ConstE r, ConstE i)
unComplexE (ConstE (RouC r))       = (ConstE (cos (PiC (2*r))), ConstE (sin (PiC (2*r))))
unComplexE (ComplexE r i )         = (r, i)
unComplexE e                       = (ReE e, ImE e)
