{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    BBinop(..),

    Type(..),
    Typed(..),

    fromComplex,
    flatten,

    LiftNum(..),

    IfThenElse(..),
    IsEq(..),
    IsOrd(..),

    intE,
    complexE,
    ensureComplexE,
    unComplexE,

    true,
    false
  ) where

import Data.Complex
import Data.Complex.Cyclotomic hiding (toComplex)
import Data.Ratio
import Data.String
import Data.Symbol
import Language.C.Quote (ToIdent(..))
import Test.QuickCheck (Arbitrary(..))
import Text.PrettyPrint.Mainland hiding (flatten)
import Text.PrettyPrint.Mainland.Class

import Data.Heterogeneous
import Spiral.RootOfUnity
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
    BoolC     :: Bool -> Const Bool
    IntC      :: Int -> Const Int
    IntegerC  :: Integer -> Const Integer
    DoubleC   :: Double -> Const Double
    RationalC :: Rational -> Const Rational
    ComplexC  :: Const Double -> Const Double -> Const (Complex Double)

    -- | Constant multiple of root of unity tagged with n and k.
    W :: Fractional (Const a) => !Int -> !Int -> Const a -> Const a

    -- | Cyclotomic numbers
    CycC :: Cyclotomic -> Const (Complex Double)

    -- | Multiple of $\pi$
    PiC :: Rational -> Const Double

deriving instance Show (Const a)

instance Eq (Const a) where
    (==) = heq
    (/=) = hne

instance Ord (Const a) where
    compare = hcompare

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
lower (BoolC x)      = x
lower (IntC x)       = x
lower (IntegerC x)   = x
lower (DoubleC x)    = x
lower (RationalC x)  = x
lower (ComplexC r i) = lower r :+ lower i
lower (W _ _ x)      = lower x
lower (CycC x)       = toComplex x
lower (PiC r)        = pi*fromRational r

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
    pprPrec _ (BoolC x)     = if x then text "true" else text "false"
    pprPrec _ (IntC x)      = ppr x
    pprPrec _ (IntegerC x)  = ppr x
    pprPrec _ (DoubleC x)   = ppr x
    pprPrec _ (RationalC x) = ppr x

    pprPrec p x@ComplexC{} = pprComplex p (lower x)

    pprPrec _ (W _ 0 _) = text "1"
    pprPrec _ (W n 1 _) = text "ω_" <> ppr n
    pprPrec _ (W n k _) = text "ω_" <> ppr n <> char '^' <> ppr k

    pprPrec p (CycC x) = text (showsPrec p x "")
    pprPrec _ (PiC r)  = pprPrec mulPrec1 r <> char '*' <> text "pi"

instance RootOfUnity (Const (Complex Double)) where
    rootOfUnity n k = W n k (CycC (rootOfUnity n k))

pprComplex :: (Eq a, Num a, Pretty a) => Int -> Complex a -> Doc
pprComplex p (r :+ 0)    = pprPrec p r
pprComplex _ (0 :+ 1)    = char 'i'
pprComplex _ (0 :+ (-1)) = text "-i"
pprComplex _ (0 :+ i)    = pprPrec mulPrec i <> char 'i'
pprComplex p (r :+ i)    = parensIf (p > addPrec) $
                           pprPrec addPrec r <+> text "+" <+> pprPrec mulPrec i <> char 'i'

-- | Convert a 'Complex Double' to a 'Const (Complex Double)'
fromComplex :: Complex Double -> Const (Complex Double)
fromComplex (r :+ i) = ComplexC (DoubleC r) (DoubleC i)

-- | Convert a 'Cyclotomic' value to a 'Double Complex' value.
-- 'Data.Complex.Cyclotomic.toComplex' introduces some error that this function
-- avoids. Ugh!
toComplex :: Cyclotomic -> Complex Double
toComplex x = r :+ i
  where
    Just r = toReal (real x)
    Just i = toReal (imag x)

-- | Convert a 'Const (Complex Double)' into a 'Cyclotomic' value
unCycC :: Monad m => Const (Complex Double) -> m Cyclotomic
unCycC (ComplexC (DoubleC re) (DoubleC im)) | isIntegral re && isIntegral im =
    return $ fromIntegral re' + fromIntegral im' * i
  where
    re', im' :: Int
    re' = truncate re
    im' = truncate im

unCycC (W n k _) = return $ rootOfUnity n k
unCycC (CycC x)  = return x
unCycC _         = fail "Not a cyclotomic number"

isIntegral :: forall a . (RealFrac a, Eq a) => a -> Bool
isIntegral x = snd (properFraction x :: (Int, a)) == 0

-- | Flatten a constant's representation.
flatten :: Const a -> Const a
flatten (W _ _ x) = flatten x
flatten x@CycC{}  = fromComplex (lower x)
flatten (PiC r)   = DoubleC (fromRational r * pi)
flatten e         = e

-- | Representation of scalar constants.
data Exp a where
    ConstE :: Const a -> Exp a
    VarE   :: Var -> Exp a
    UnopE  :: Unop -> Exp a -> Exp a
    BinopE :: Num (Exp a) => Binop -> Exp a -> Exp a -> Exp a
    IdxE   :: Var -> [Exp Int] -> Exp a

    ComplexE :: (Typed a, Num (Exp a)) => Exp a -> Exp a -> Exp (Complex a)
    ReE :: Num (Exp a) => Exp (Complex a) -> Exp a
    ImE :: Num (Exp a) => Exp (Complex a) -> Exp a

    BBinopE :: Typed a => BBinop -> Exp a -> Exp a -> Exp Bool
    IfE     :: Exp Bool -> Exp a -> Exp a -> Exp a

deriving instance Show (Exp a)

instance Eq (Exp a) where
    (==) = heq
    (/=) = hne

instance Ord (Exp a) where
    compare = hcompare

instance IsString (Exp a) where
    fromString s = VarE (fromString s)

instance RootOfUnity (Exp (Complex Double)) where
    rootOfUnity n k = mkConstE (rootOfUnity n k)

--
-- Heterogeneous quality and comparison
--

instance HEq Const where
    heq (BoolC x)        (BoolC y)        = x == y
    heq (IntC x)         (IntC y)         = x == y
    heq (IntegerC x)     (IntegerC y)     = x == y
    heq (DoubleC x)      (DoubleC y)      = x == y
    heq (RationalC x)    (RationalC y)    = x == y
    heq (ComplexC r1 i1) (ComplexC r2 i2) = r1 == r2 && i1 == i2
    heq (W n k _)        (W n' k' _)      = (k' `mod` n') % n' == (k `mod` n) % n
    heq (CycC x)         (CycC y)         = x == y
    heq (PiC x)          (PiC y)          = x == y

    heq x@ComplexC{} y | Just x' <- unCycC x = heq (CycC x') y
    heq x y@ComplexC{} | Just y' <- unCycC y = heq x (CycC y')

    heq (W _ _ c) y = heq c y
    heq x (W _ _ c) = heq x c

    heq x@PiC{} y = heq (DoubleC (lower x)) y
    heq x y@PiC{} = heq x (DoubleC (lower y))

    heq _ _ = False

instance HEq Exp where
    heq (ConstE c1)           (ConstE c2)           = heq c1 c2
    heq (VarE x)              (VarE y)              = x == y
    heq (UnopE op1 e1)        (UnopE op2 e2)        = (op1, Some e1) == (op2, Some e2)
    heq (BinopE op1 e1a e1b)  (BinopE op2 e2a e2b)  = (op1, Some e1a, Some e1b) == (op2, Some e2a, Some e2b)
    heq (IdxE v1 es1)         (IdxE v2 es2)         = (v1, es1) == (v2, es2)
    heq (ComplexE r1 i1)      (ComplexE r2 i2)      = (Some r1, Some i1) == (Some r2, Some i2)
    heq (ReE e1)              (ReE e2)              = heq e1 e2
    heq (ImE e1)              (ImE e2)              = heq e1 e2
    heq (BBinopE op1 e1a e1b) (BBinopE op2 e2a e2b) = (op1, Some e1a, Some e1b) == (op2, Some e2a, Some e2b)
    heq (IfE e1a e1b e1c)     (IfE e2a e2b e2c)     = (Some e1a, Some e1b, Some e1c) == (Some e2a, Some e2b, Some e2c)
    heq _                     _                     = False

instance HOrd Const where
    hcompare (BoolC x)         (BoolC y)       = compare x y
    hcompare (IntC x)         (IntC y)         = compare x y
    hcompare (IntegerC x)     (IntegerC y)     = compare x y
    hcompare (DoubleC x)      (DoubleC y)      = compare x y
    hcompare (RationalC x)    (RationalC y)    = compare x y
    hcompare (ComplexC r1 i1) (ComplexC r2 i2) = compare (r1, i1) (r2, i2)
    hcompare (W n k _)        (W n' k' _)      = compare ((k' `mod` n') % n') ((k `mod` n) % n)
    hcompare x@CycC{}         y@CycC{}         = compare (flatten x) (flatten y)
    hcompare (PiC x)          (PiC y)          = compare x y

    hcompare x@ComplexC{} y | Just x' <- unCycC x = hcompare (CycC x') y
    hcompare x y@ComplexC{} | Just y' <- unCycC y = hcompare x (CycC y')

    hcompare (W _ _ c) y = hcompare c y
    hcompare x (W _ _ c) = hcompare x c

    hcompare x@PiC{} y = hcompare (DoubleC (lower x)) y
    hcompare x y@PiC{} = hcompare x (DoubleC (lower y))

    hcompare x y = compare (tag x) (tag y)
      where
        tag :: Const a -> Int
        tag BoolC{}     = 0
        tag IntC{}      = 1
        tag IntegerC{}  = 2
        tag DoubleC{}   = 3
        tag RationalC{} = 4
        tag ComplexC{}  = 5
        tag W{}         = 6
        tag CycC{}      = 7
        tag PiC{}       = 8

instance HOrd Exp where
    hcompare (ConstE c1)           (ConstE c2)           = hcompare c1 c2
    hcompare (VarE x)              (VarE y)              = compare x y
    hcompare (UnopE op1 e1)        (UnopE op2 e2)        = compare (op1, Some e1)(op2, Some e2)
    hcompare (BinopE op1 e1a e1b)  (BinopE op2 e2a e2b)  = compare (op1, Some e1a, Some e1b) (op2, Some e2a, Some e2b)
    hcompare (IdxE v1 es1)         (IdxE v2 es2)         = compare (v1, es1) (v2, es2)
    hcompare (ComplexE r1 i1)      (ComplexE r2 i2)      = compare (Some r1, Some i1) (Some r2, Some i2)
    hcompare (ReE e1)              (ReE e2)              = hcompare e1 e2
    hcompare (ImE e1)              (ImE e2)              = hcompare e1 e2
    hcompare (BBinopE op1 e1a e1b) (BBinopE op2 e2a e2b) = compare (op1, Some e1a, Some e1b) (op2, Some e2a, Some e2b)
    hcompare (IfE e1a e1b e1c)     (IfE e2a e2b e2c)     = compare (Some e1a, Some e1b, Some e1c) (Some e2a, Some e2b, Some e2c)
    hcompare e1                    e2                    = compare (tag e1) (tag e2)
      where
        tag :: Exp a -> Int
        tag ConstE{}   = 0
        tag VarE{}     = 1
        tag UnopE{}    = 2
        tag BinopE{}   = 3
        tag IdxE{}     = 4
        tag ComplexE{} = 5
        tag ReE{}      = 6
        tag ImE{}      = 7
        tag BBinopE{}  = 8
        tag IfE{}      = 9

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

-- | Boolean binary operators
data BBinop = Eq
            | Ne
            | Lt
            | Le
            | Ge
            | Gt
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

instance HasFixity BBinop where
    fixity Eq = infix_ 4
    fixity Ne = infix_ 4
    fixity Lt = infix_ 4
    fixity Le = infix_ 4
    fixity Ge = infix_ 4
    fixity Gt = infix_ 4

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

instance Pretty BBinop where
    ppr Eq = text "=="
    ppr Ne = text "/="
    ppr Lt = text "<"
    ppr Le = text "<="
    ppr Ge = text ">="
    ppr Gt = text ">"

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

    pprPrec p (BBinopE op e1 e2) =
        infixop p op e1 e2

    pprPrec _ (IfE e1 e2 e3) =
        text "if" <+> ppr e1 <+>
        text "then" <+> ppr e2 <+>
        text "else" <+> ppr e3

-- | Representation of types.
data Type a where
    BoolT    :: Type Bool
    IntT     :: Type Int
    IntegerT :: Type Integer
    DoubleT  :: Type Double
    ComplexT :: (Typed a, Num (Exp a)) => Type a -> Type (Complex a)

deriving instance Eq (Type a)
deriving instance Ord (Type a)
deriving instance Show (Type a)

instance Pretty (Type a) where
    ppr BoolT          = text "bool"
    ppr IntT           = text "int"
    ppr IntegerT       = text "integer"
    ppr DoubleT        = text "double"
    ppr (ComplexT tau) = text "complex" <+> ppr tau

class Typed a where
    typeOf :: a -> Type a

instance Typed Bool where
    typeOf _ = BoolT

instance Typed Int where
    typeOf _ = IntT

instance Typed Integer where
    typeOf _ = IntegerT

instance Typed Double where
    typeOf _ = DoubleT

instance (Typed a, Num (Exp a)) => Typed (Complex a) where
    typeOf _ = ComplexT (typeOf (undefined :: a))

toPow :: Num (Exp a) => Exp a -> Integer -> Exp a
toPow _                  0 = 1
toPow e                  1 = e
toPow (ConstE (W n k c)) l = mkConstE $ W n ((k*fromInteger l) `mod` n) (c^^l)
toPow (ConstE (CycC x))  n = mkConstE $ CycC (x^^n)
toPow e                  n = UnopE (Pow n) e

fromPow :: Num (Exp a) => Exp a -> Maybe (Exp a, Integer)
fromPow (UnopE (Pow n) x@VarE{})                = return (x, n)
fromPow (BinopE Div 1 (UnopE (Pow n) x@VarE{})) = return (x, -n)
fromPow x@VarE{}                                = return (x, 1)
fromPow (BinopE Div 1 x@VarE{})                 = return (x, -1)
fromPow _                                       = fail "Can't destruct power"

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
      | isZero e = 0

    liftNum Neg _ (ComplexC a b) = ComplexC (-a) (-b)
    liftNum Neg _ (CycC c)       = CycC (-c)
    liftNum Neg _ (PiC r)        = PiC (-r)

    liftNum Neg _ (W n k c) | even n =
        W n ((k + n `quot` 2) `mod` n) (-c)

    liftNum op f (W _ _ c) = liftNum op f c

    liftNum _op f c = lift f (flatten c)

    liftNum2 Add _ e1 e2
      | isZero e1 = e2
      | isZero e2 = e1

    liftNum2 Sub _ e1 e2
      | isZero e1 = -e2
      | isZero e2 = e1

    liftNum2 Mul _ e1 e2
      | isZero   e1 = 0
      | isZero   e2 = 0
      | isOne    e1 = e2
      | isOne    e2 = e1
      | isNegOne e1 = -e2
      | isNegOne e2 = -e1

    liftNum2 Add _ (ComplexC a b) (ComplexC c d) = ComplexC (a + c) (b + d)
    liftNum2 Sub _ (ComplexC a b) (ComplexC c d) = ComplexC (a - c) (b - d)
    liftNum2 Mul _ (ComplexC a b) (ComplexC c d) = ComplexC (a*c - b*d) (b*c + a*d)

    liftNum2 Mul _ (W n k x) (W m l y) = W n' (((k*m + l*n) `quot` d) `mod` n') (x*y)
      where
        n' = lcm n m
        d  = gcd n m

    liftNum2 op f (W _ _ c) y = liftNum2 op f c y
    liftNum2 op f x (W _ _ c) = liftNum2 op f x c

    -- Try to perform all operations in the cyclotomic domain
    liftNum2 _op f x0@ComplexC{} y0 | Just x <- unCycC x0, Just y <- unCycC y0 = CycC (f x y)
    liftNum2 _op f x0@CycC{}     y0 | Just x <- unCycC x0, Just y <- unCycC y0 = CycC (f x y)

    liftNum2 _op f x y = lift2 f (flatten x) (flatten y)

--- XXX Need UndecidableInstances for this...but we *must* call
--- liftNum/liftNum2 on constants to ensure they are properly simplified.
--- The other option would be to forgo the Const instance and push it into the
--- Exp instance.
instance (Num (Const a), LiftNum (Const a), Num (Exp a)) => LiftNum (Exp a) where
    liftNum Neg _ e
      | isZero e = 0

    liftNum Neg _ (UnopE Neg x)      = x
    liftNum Neg _ (BinopE Sub e1 e2) = e2 - e1
    liftNum Neg _ (ComplexE a b)     = ComplexE (-a) (-b)

    liftNum op f (ConstE c) = mkConstE $ liftNum op f c

    liftNum op _ e = UnopE op e

    liftNum2 op f (ConstE c1) (ConstE c2) = mkConstE $ liftNum2 op f c1 c2

    liftNum2 Add _ e1 e2
      | isZero e1 = e2
      | isZero e2 = e1

    liftNum2 Sub _ e1 e2
      | isZero e1 = -e2
      | isZero e2 = e1

    liftNum2 Mul _ e1 e2
      | isZero   e1 = 0
      | isZero   e2 = 0
      | isOne    e1 = e2
      | isOne    e2 = e1
      | isNegOne e1 = -e2
      | isNegOne e2 = -e1

    -- Choose canonical variable ordering
    liftNum2 Add _ e1@(VarE x) e2@(VarE y) | y < x =
        BinopE Add e2 e1

    liftNum2 Sub _ e1@(VarE x) e2@(VarE y) | y < x =
        UnopE Neg (BinopE Sub e2 e1)

    liftNum2 Mul _ e1@(VarE x) e2@(VarE y) | y < x =
        BinopE Mul e2 e1

    -- Constants always come first
    liftNum2 Mul f e1 e2@ConstE{} | not (isConstE e1) =
        liftNum2 Mul f e2 e1

    -- Push negation out
    liftNum2 Add _ e1 (UnopE Neg e2) =
        e1 - e2

    liftNum2 Add _ (UnopE Neg e1) e2 =
        e2 - e1

    liftNum2 Sub _ e1 (UnopE Neg e2) =
        e1 + e2

    liftNum2 Sub _ (UnopE Neg e1) e2 =
        -(e1 + e2)

    liftNum2 Mul _ e1 (UnopE Neg e2) =
        -(e1 * e2)

    liftNum2 Mul _ (UnopE Neg e1) e2 =
        -(e1 * e2)

    -- Take advantage of distributivity of multipliation
    liftNum2 op f (BinopE Mul c1 e1) (BinopE Mul c2 e2) | c1 == c2, isAddSubOp op =
        c1 * liftNum2 op f e1 e2

    -- Simplify powers
    liftNum2 Mul _ e1 e2 | Just (x, n) <- fromPow e1, Just (y, m) <- fromPow e2, x == y =
        toPow x (n+m)

    -- Standard component-wise add/subtract
    liftNum2 Add _ (ComplexE a b) (ComplexE c d) = ComplexE (a + c) (b + d)
    liftNum2 Sub _ (ComplexE a b) (ComplexE c d) = ComplexE (a - c) (b - d)

    -- Use the 3-multiply 4-add form when a and b are constants.
    liftNum2 Mul _ (ComplexE a0 b0) (ComplexE c d) | Just a <- unConstE a0, Just b <- unConstE b0 =
        let t1 = a0*(c+d)
            t2 = d*(b+a)
            t3 = c*(b-a)
        in
          ComplexE (t1 - t2) (t1 + t3)
      where
        unConstE :: Monad m => Exp b -> m (Exp b)
        unConstE e@ConstE{} =
            return e

        unConstE (UnopE Neg (ConstE (DoubleC x))) =
            return $ ConstE (DoubleC (-x))

        unConstE _ =
            fail "Not a constant"

    liftNum2 Mul _ (ComplexE a b) (ComplexE c d) =
        ComplexE (a*c - b*d) (b*c + a*d)

    -- Attempt to operate on complex values using their imaginary and real parts
    -- if at least on of the arguments to an operator is constructed with
    -- 'ComplexE'.
    liftNum2 op f x@ComplexE{} y | isAddSubMulOp op, Just y' <- ensureComplexE y =
        liftNum2 op f x y'

    liftNum2 op f x y@ComplexE{} | isAddSubMulOp op, Just x' <- ensureComplexE x =
        liftNum2 op f x' y

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

    fromInteger = mkConstE . IntC . fromInteger

instance Num (Exp Integer) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = mkConstE . IntegerC

instance Num (Exp Double) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = mkConstE . DoubleC . fromInteger

instance Num (Exp Rational) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = mkConstE . RationalC . fromInteger

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

    liftIntegral2 _ _ W{} _   = error "can't happen"
    liftIntegral2 _ _ _   W{} = error "can't happen"

instance Enum (Const Int) where
    toEnum n = IntC (fromIntegral n)

    fromEnum (IntC i) = fromIntegral i
    fromEnum W{}      = error "can't happen"

instance Real (Const Int) where
    toRational (IntC i) = toRational i
    toRational W{}      = error "can't happen"

instance Integral (Const Int) where
    quot = liftIntegral2 Quot quot
    rem  = liftIntegral2 Rem rem

    x `quotRem` y = (x `quot` y, x `rem` y)

    toInteger (IntC i) = fromIntegral i
    toInteger W{}      = error "can't happen"

instance (LiftIntegral (Const a), Integral (Exp a)) => LiftIntegral (Exp a) where
    liftIntegral2 op f (ConstE x) (ConstE y) = mkConstE $ liftIntegral2 op f x y

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

instance (Fractional a, Fractional (Const a)) => LiftFrac (Const a) where
    liftFrac2 Div _ c1 (W n k c) | isOne c1 =
        W n ((-k) `mod` n) (1/c)

    liftFrac2 Div f (W n k c) (W n' k' c') =
        liftFrac2 Div f (W n k c) (W n' (-k') (1/c'))

    liftFrac2 op f (W _ _ c) y = liftFrac2 op f c y
    liftFrac2 op f x (W _ _ c) = liftFrac2 op f x c

    liftFrac2 Div _ c1 c2 | isOne c2 =
        c1

    -- Try to perform all operations in the cyclotomic domain
    liftFrac2 _op f x0@ComplexC{} y0 | Just x <- unCycC x0, Just y <- unCycC y0 = CycC (f x y)
    liftFrac2 _op f x0@CycC{}     y0 | Just x <- unCycC x0, Just y <- unCycC y0 = CycC (f x y)

    liftFrac2 _op f c1 c2 = lift2 f c1 c2

instance (LiftFrac (Const a), Num (Exp a)) => LiftFrac (Exp a) where
    liftFrac2 op f (ConstE c1) (ConstE c2) = mkConstE $ liftFrac2 op f c1 c2

    liftFrac2 Div _ e1 e2 | isOne e2 =
        e1

    liftFrac2 Div _ e1 e2 | isOne e1, Just (x, n) <- fromPow e2 =
        toPow x (-n)

    liftFrac2 Div _ e1 e2 | Just (x, m) <- fromPow e1, Just (x', n) <- fromPow e2, x' == x =
        toPow x (m-n)

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

    fromRational = mkConstE . DoubleC . fromRational

instance Fractional (Exp (Complex Double)) where
    (/) = liftFrac2 Div (/)

    fromRational x = mkConstE (ComplexC (fromRational x) 0)

--------------------------------------------------------------------------------
--
-- Staged operations
--
--------------------------------------------------------------------------------

class IfThenElse a b where
    ifThenElse :: a -> b -> b -> b

instance IfThenElse Bool a where
    ifThenElse c t e = if c then t else e

infix 4 .==., ./=.

class IsEq a b | a -> b where
    (.==.) :: a -> a -> b
    (./=.) :: a -> a -> b

infix 4 .<., .<=., .>=., .>.

class IsEq a b => IsOrd a b where
    (.<.)  :: a -> a -> b
    (.<=.) :: a -> a -> b
    (.>=.) :: a -> a -> b
    (.>.)  :: a -> a -> b

instance (Typed a, Eq a) => IsEq (Exp a) (Exp Bool) where
    e1 .==. e2 = BBinopE Eq e1 e2
    e1 ./=. e2 = BBinopE Ne e1 e2

instance (Typed a, Eq a) => IsOrd (Exp a) (Exp Bool) where
    e1 .<.  e2 = BBinopE Lt e1 e2
    e1 .<=. e2 = BBinopE Le e1 e2
    e1 .>.  e2 = BBinopE Gt e1 e2
    e1 .>=. e2 = BBinopE Ge e1 e2

instance IfThenElse (Exp Bool) (Exp a) where
    ifThenElse = IfE

--------------------------------------------------------------------------------
--
-- Smart constructors
--
--------------------------------------------------------------------------------

class IsZeroOne a where
    isZero, isOne, isNegOne :: a -> Bool

instance IsZeroOne (Const a) where
    isZero (IntC 0)       = True
    isZero (IntegerC 0)   = True
    isZero (DoubleC 0)    = True
    isZero (RationalC 0)  = True
    isZero (ComplexC 0 0) = True
    isZero (CycC 0)       = True
    isZero _              = False

    isOne (IntC 1)       = True
    isOne (IntegerC 1)   = True
    isOne (DoubleC 1)    = True
    isOne (RationalC 1)  = True
    isOne (ComplexC 1 i) = isZero i
    isOne (W k n _)      = n % k == 1
    isOne (CycC 1)       = True
    isOne _              = False

    isNegOne (IntC (-1))       = True
    isNegOne (IntegerC (-1))   = True
    isNegOne (DoubleC (-1))    = True
    isNegOne (RationalC (-1))  = True
    isNegOne (ComplexC (-1) i) = isZero i
    isNegOne (W k n _)         = n % k == 1 % 2
    isNegOne (CycC (-1))       = True
    isNegOne _                 = False

instance IsZeroOne (Exp a) where
    isZero (ConstE c) = isZero c
    isZero _          = False

    isOne (ConstE c) = isOne c
    isOne _          = False

    isNegOne (ConstE c)             = isNegOne c
    isNegOne (UnopE Neg (ConstE c)) = isOne c
    isNegOne _                      = False

mkConstE :: Const a -> Exp a
mkConstE (DoubleC x) | x < 0 =
    UnopE Neg (ConstE (DoubleC (-x)))

mkConstE x = ConstE x

isConstE :: Exp a -> Bool
isConstE ConstE{} = True
isConstE _        = False

isAddSubMulOp :: Binop -> Bool
isAddSubMulOp Add = True
isAddSubMulOp Sub = True
isAddSubMulOp Mul = True
isAddSubMulOp _   = False

isAddSubOp :: Binop -> Bool
isAddSubOp Add = True
isAddSubOp Sub = True
isAddSubOp _   = False

-- | Create an 'Exp Int' from an 'Int'.
intE :: Int -> Exp Int
intE = ConstE . IntC

-- | Create an 'Exp (Complex Double)' from a 'Complex Double'.
complexE :: Complex Double -> Exp (Complex Double)
complexE (r :+ i) = ConstE $ ComplexC (DoubleC r) (DoubleC i)

-- | Ensure that a complex expression is represented using its constituent real
-- and imaginary parts.
ensureComplexE :: (Num (Exp a), Monad m)
               => Exp (Complex a)
               -> m (Exp (Complex a))
ensureComplexE (ConstE (ComplexC r i)) = return $ ComplexE (mkConstE r) (mkConstE i)
ensureComplexE (ConstE (W _ _ c))      = ensureComplexE (mkConstE c)
ensureComplexE (ConstE x@CycC{})       = return $ ComplexE (mkConstE (DoubleC r)) (mkConstE (DoubleC i))
  where
    r :+ i = lower x
ensureComplexE e@ComplexE{}            = return e
ensureComplexE _                       = fail "Can't construct ComplexE"

-- | Extract the complex and real portions of an 'Exp (Complex a)'.
unComplexE :: Num (Exp a) => Exp (Complex a) -> (Exp a, Exp a)
unComplexE (ConstE (ComplexC r i)) = (mkConstE r, mkConstE i)
unComplexE (ConstE (W _ _ c))      = unComplexE (mkConstE c)
unComplexE (ConstE x@CycC{})       = (mkConstE (DoubleC r), mkConstE (DoubleC i))
  where
    r :+ i = lower x
unComplexE (ComplexE r i )         = (r, i)
unComplexE e                       = (ReE e, ImE e)

true, false :: Exp Bool
true  = mkConstE (BoolC True)
false = mkConstE (BoolC False)
