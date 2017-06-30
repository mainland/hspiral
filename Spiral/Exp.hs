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

    ToConst(..),
    RealFloatConst,
    fromConst,
    fromComplex,
    lower,

    LiftNum(..),
    LiftFloating(..),

    IfThenElse(..),
    IsEq(..),
    IsOrd(..),

    intE,
    complexE,
    unComplexE,
    forceUnComplexE,

    isConstE,

    IsZeroOne(..),
    isI,
    isNegI,

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
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Data.Heterogeneous
import Spiral.Globals
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
    FloatC    :: Float -> Const Float
    DoubleC   :: Double -> Const Double
    RationalC :: FractionalConst a => Rational -> Const a
    ComplexC  :: RealFloatConst a => Const a -> Const a -> Const (Complex a)

    -- | Constant multiple of root of unity tagged with n and k.
    W :: RootOfUnity (Const a) => !Int -> !Int -> Const a -> Const a

    -- | Cyclotomic numbers
    CycC :: RealFloatConst a => Cyclotomic -> Const (Complex a)

deriving instance Show (Const a)

instance Eq (Const a) where
    (==) = heq
    (/=) = hne

instance Ord (Const a) where
    compare = hcompare

class (Eq a, Pretty a) => ToConst a where
    toConst :: a -> Const a

instance ToConst Bool where
    toConst = BoolC

instance ToConst Int where
    toConst = IntC

instance ToConst Integer where
    toConst = IntegerC

instance ToConst Float where
    toConst x | isIntegral x = RationalC (fromIntegral (truncate x :: Int))
              | otherwise    = FloatC x

instance ToConst Double where
    toConst x | isIntegral x = RationalC (fromIntegral (truncate x :: Int))
              | otherwise    = DoubleC x

instance ToConst Rational where
    toConst = RationalC

instance RealFloatConst a => ToConst (Complex a) where
    toConst (r :+ i) = ComplexC (toConst r) (toConst i)

class ( Fractional a
      , Typed a
      , ToConst a
      ) => FractionalConst a where

instance FractionalConst Rational where

instance FractionalConst Float where

instance FractionalConst Double where

instance FractionalConst (Complex Float) where

instance FractionalConst (Complex Double) where

class ( Eq a
      , RealFloat a
      , Pretty a
      , FractionalConst a
      , Num (Const a)
      , Num (Exp a)
      , Num (Exp (Complex a))
      ) => RealFloatConst a where

instance RealFloatConst Float where

instance RealFloatConst Double where

-- | Convert a 'Const a' to the value of type 'a' that it represents.
fromConst :: Const a -> a
fromConst (BoolC x)      = x
fromConst (IntC x)       = x
fromConst (IntegerC x)   = x
fromConst (FloatC x)     = x
fromConst (DoubleC x)    = x
fromConst (RationalC x)  = fromRational x
fromConst (ComplexC r i) = fromConst r :+ fromConst i
fromConst (W _ _ x)      = fromConst x
fromConst (CycC x)       = toComplex x

lift :: ToConst a => (a -> a) -> Const a -> Const a
lift f (IntC x)      = IntC (f x)
lift f (IntegerC x)  = IntegerC (f x)
lift f (FloatC x)    = FloatC (f x)
lift f (DoubleC x)   = DoubleC (f x)
lift f x@RationalC{} = toConst (f (fromConst x))
lift f x@ComplexC{}  = toConst (f (fromConst x))
lift f x             = lift f (lower x)

lift2 :: ToConst a => (a -> a -> a) -> Const a -> Const a -> Const a
lift2 f (IntC x)      (IntC y)      = IntC (f x y)
lift2 f (IntegerC x)  (IntegerC y)  = IntegerC (f x y)
lift2 f (FloatC x)    (FloatC y)    = FloatC (f x y)
lift2 f (DoubleC x)   (DoubleC y)   = DoubleC (f x y)
lift2 f x@RationalC{} y@RationalC{} = toConst (f (fromConst x) (fromConst y))
lift2 f x@ComplexC{}  y@ComplexC{}  = toConst (f (fromConst x) (fromConst y))
lift2 f x             y             = lift2 f (lower x) (lower y)

instance Arbitrary (Const Int) where
    arbitrary = IntC <$> arbitrary

instance Arbitrary (Const Integer) where
    arbitrary = IntegerC <$> arbitrary

instance Arbitrary (Const Double) where
    arbitrary = DoubleC <$> arbitrary

instance Arbitrary (Const Float) where
    arbitrary = FloatC <$> arbitrary

instance Arbitrary (Const Rational) where
    arbitrary = RationalC <$> arbitrary

instance Arbitrary (Const (Complex Double)) where
    arbitrary = ComplexC <$> arbitrary <*> arbitrary

instance Pretty (Const a) where
    pprPrec _ (BoolC x)     = if x then text "true" else text "false"
    pprPrec _ (IntC x)      = ppr x
    pprPrec _ (IntegerC x)  = ppr x
    pprPrec _ (FloatC x)    = ppr x <> char 'f'
    pprPrec _ (DoubleC x)   = ppr x
    pprPrec _ (RationalC x) = ppr x

    pprPrec p x@ComplexC{} = pprComplex p (fromConst x)

    pprPrec _ (W _ 0 _) = text "1"
    pprPrec _ (W n 1 _) = text "ω_" <> ppr n
    pprPrec _ (W n k _) = text "ω_" <> ppr n <> char '^' <> ppr k

    pprPrec p (CycC x) = text (showsPrec p x "")

instance RootOfUnity (Const (Complex Float)) where
    rootOfUnity n k = mkW (k%n) (CycC (rootOfUnity n k))

instance RootOfUnity (Const (Complex Double)) where
    rootOfUnity n k = mkW (k%n) (CycC (rootOfUnity n k))

pprComplex :: (Eq a, Num a, Pretty a) => Int -> Complex a -> Doc
pprComplex p (r :+ 0)    = pprPrec p r
pprComplex _ (0 :+ 1)    = char 'i'
pprComplex _ (0 :+ (-1)) = text "-i"
pprComplex _ (0 :+ i)    = pprPrec mulPrec i <> char 'i'
pprComplex p (r :+ i)    = parensIf (p > addPrec) $
                           pprPrec addPrec r <+> text "+" <+> pprPrec mulPrec i <> char 'i'

-- | Convert a 'Complex Double' to a 'Const (Complex Double)'
fromComplex :: RealFloatConst a => Complex a -> Const (Complex a)
fromComplex (r :+ i) = ComplexC (toConst r) (toConst i)

-- | Convert a 'Cyclotomic' value to a 'Double Complex' value.
-- 'Data.Complex.Cyclotomic.toComplex' introduces some error that this function
-- avoids. Ugh!
toComplex :: RealFloat a => Cyclotomic -> Complex a
toComplex x = r :+ i
  where
    Just r = (fromRational . toRational) <$> toReal (real x)
    Just i = (fromRational . toRational) <$> toReal (imag x)

-- | Convert a constant into the most general value that can be used for exact
-- comparison.
exact :: forall m a . Monad m => Const a -> m (Const a)
exact x@BoolC{}     = return x
exact x@IntC{}      = return x
exact x@IntegerC{}  = return x
exact x@FloatC{}    = return x
exact x@DoubleC{}   = return x

exact x@(RationalC r) =
    case tau of
      FloatT     -> return $ FloatC $ fromConst x
      DoubleT    -> return $ DoubleC $ fromConst x
      RationalT  -> return x
      ComplexT{} -> return $ CycC $ fromRational r
      _          -> fail "Cannot represent exactly"
  where
    tau :: Type a
    tau = typeOf (undefined :: a)

exact (ComplexC cre cim) | isIntegral re && isIntegral im =
    return $ CycC $ fromIntegral re' + fromIntegral im' * i
  where
    re, im :: a ~ Complex b => b
    re = fromConst cre
    im = fromConst cim

    re', im' :: Int
    re' = truncate re
    im' = truncate im

exact (W _ _ x) = exact x
exact x@CycC{}  = return x
exact _         = fail "Cannot represent exactly"

isIntegral :: forall a . (RealFrac a, Eq a) => a -> Bool
isIntegral x = snd (properFraction x :: (Int, a)) == 0

-- | Lower a constant's representation from a specialized representation to a
-- standard representation. This ensures the constant is not constructed with
-- the 'W' or 'CycC' data constructors.
lower :: forall a . Const a -> Const a
lower x@(RationalC r) =
    case tau of
      FloatT     -> FloatC $ fromConst x
      DoubleT    -> DoubleC $ fromConst x
      ComplexT{} -> lower $ ComplexC (RationalC r) 0
      _          -> x
  where
    tau :: Type a
    tau = typeOf (undefined :: a)

lower (ComplexC r i) = ComplexC (lower r) (lower i)
lower (W _ _ x)      = lower x
lower x@CycC{}       = toConst (fromConst x)
lower e              = e

-- | Representation of scalar constants.
data Exp a where
    ConstE :: Const a -> Exp a
    VarE   :: Var -> Exp a
    UnopE  :: Unop -> Exp a -> Exp a
    BinopE :: Num (Exp a) => Binop -> Exp a -> Exp a -> Exp a
    IdxE   :: Var -> [Exp Int] -> Exp a

    ComplexE :: (Typed a, Num (Exp a)) => Exp a -> Exp a -> Exp (Complex a)
    ReE :: RealFloatConst a => Exp (Complex a) -> Exp a
    ImE :: RealFloatConst a => Exp (Complex a) -> Exp a

    BBinopE :: (Typed a, ToConst a) => BBinop -> Exp a -> Exp a -> Exp Bool
    IfE     :: Exp Bool -> Exp a -> Exp a -> Exp a

deriving instance Show (Exp a)

instance Eq (Exp a) where
    (==) = heq
    (/=) = hne

instance Ord (Exp a) where
    compare = hcompare

instance IsString (Exp a) where
    fromString s = VarE (fromString s)

instance RootOfUnity (Exp (Complex Float)) where
    rootOfUnity n k = ConstE (rootOfUnity n k)

instance RootOfUnity (Exp (Complex Double)) where
    rootOfUnity n k = ConstE (rootOfUnity n k)

--
-- Heterogeneous quality and comparison
--

instance HEq Const where
    heq x y =
      case (exact x, exact y) of
        (Just x', Just y') -> go x' y'
        _                  -> go (lower x) (lower y)
      where
        go :: Const a -> Const b -> Bool
        go (BoolC x)        (BoolC y)        = x == y
        go (IntC x)         (IntC y)         = x == y
        go (IntegerC x)     (IntegerC y)     = x == y
        go (FloatC x)       (FloatC y)       = x == y
        go (DoubleC x)      (DoubleC y)      = x == y
        go (RationalC x)    (RationalC y)    = x == y
        go (ComplexC r1 i1) (ComplexC r2 i2) = (Some r1, Some i1) == (Some r2, Some i2)
        go (CycC x)         (CycC y)         = x == y
        go _                _                = False

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
    hcompare x y =
      case (exact x, exact y) of
        (Just x', Just y') -> go x' y'
        _                  -> go (lower x) (lower y)
      where
        go :: Const a -> Const b -> Ordering
        go (BoolC x)        (BoolC y)        = compare x y
        go (IntC x)         (IntC y)         = compare x y
        go (IntegerC x)     (IntegerC y)     = compare x y
        go (FloatC x)       (FloatC y)       = compare x y
        go (DoubleC x)      (DoubleC y)      = compare x y
        go (RationalC x)    (RationalC y)    = compare x y
        go (ComplexC r1 i1) (ComplexC r2 i2) = compare (Some r1, Some i1) (Some r2, Some i2)
        go (CycC x)         (CycC y)         = compare (xr, xi) (yr, yi)
          where
            xr :+ xi = fromConst (CycC x :: Const (Complex Double))
            yr :+ yi = fromConst (CycC y :: Const (Complex Double))

        go x y = compare (tag x) (tag y)
          where
            tag :: Const a -> Int
            tag BoolC{}     = 0
            tag IntC{}      = 1
            tag IntegerC{}  = 2
            tag FloatC{}    = 3
            tag DoubleC{}   = 4
            tag RationalC{} = 5
            tag ComplexC{}  = 6
            tag W{}         = 7
            tag CycC{}      = 8

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
          | Exp
          | Log
          | Sin
          | Cos
          | Asin
          | Acos
          | Atan
          | Sinh
          | Cosh
          | Asinh
          | Acosh
          | Atanh
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
    fixity Exp    = infixr_ 10
    fixity Log    = infixr_ 10
    fixity Sin    = infixr_ 10
    fixity Cos    = infixr_ 10
    fixity Asin   = infixr_ 10
    fixity Acos   = infixr_ 10
    fixity Atan   = infixr_ 10
    fixity Sinh   = infixr_ 10
    fixity Cosh   = infixr_ 10
    fixity Asinh  = infixr_ 10
    fixity Acosh  = infixr_ 10
    fixity Atanh  = infixr_ 10

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
    ppr Exp     = text "exp" <> space
    ppr Log     = text "log" <> space
    ppr Sin     = text "sin" <> space
    ppr Cos     = text "cos" <> space
    ppr Asin    = text "asin" <> space
    ppr Acos    = text "acos" <> space
    ppr Atan    = text "atan" <> space
    ppr Sinh    = text "sinh" <> space
    ppr Cosh    = text "cosh" <> space
    ppr Asinh   = text "asinh" <> space
    ppr Acosh   = text "acosh" <> space
    ppr Atanh   = text "atanh" <> space

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
    BoolT     :: Type Bool
    IntT      :: Type Int
    IntegerT  :: Type Integer
    FloatT    :: Type Float
    DoubleT   :: Type Double
    RationalT :: Type Rational
    ComplexT  :: RealFloatConst a => Type a -> Type (Complex a)

deriving instance Eq (Type a)
deriving instance Ord (Type a)
deriving instance Show (Type a)

instance Pretty (Type a) where
    ppr BoolT          = text "bool"
    ppr IntT           = text "int"
    ppr IntegerT       = text "integer"
    ppr FloatT         = text "float"
    ppr DoubleT        = text "double"
    ppr RationalT      = text "rational"
    ppr (ComplexT tau) = text "complex" <+> ppr tau

class Typed a where
    typeOf :: a -> Type a

instance Typed Bool where
    typeOf _ = BoolT

instance Typed Int where
    typeOf _ = IntT

instance Typed Integer where
    typeOf _ = IntegerT

instance Typed Float where
    typeOf _ = FloatT

instance Typed Double where
    typeOf _ = DoubleT

instance Typed Rational where
    typeOf _ = RationalT

instance RealFloatConst a => Typed (Complex a) where
    typeOf _ = ComplexT (typeOf (undefined :: a))

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

instance (Typed a, Num a, ToConst a, Num (Const a)) => LiftNum (Const a) where
    liftNum Neg _ e
      | isZero e = 0

    liftNum Neg _ (ComplexC a b) = ComplexC (-a) (-b)
    liftNum Neg _ (CycC c)       = CycC (-c)

    liftNum Neg _ (W n k c) = mkW (k % n + 1 % 2) (-c)

    liftNum _op f (RationalC x) = RationalC (f x)

    liftNum op f (W _ _ c) = liftNum op f c

    liftNum _op f c = lift f c

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

    liftNum2 Mul _ (W n k x) (W n' k' y) = mkW (k % n + k' % n') (x*y)

    liftNum2 _op f (RationalC x) (RationalC y) = RationalC (f x y)

    liftNum2 op f (W _ _ c) y = liftNum2 op f c y
    liftNum2 op f x (W _ _ c) = liftNum2 op f x c

    liftNum2 _op f (CycC x) (CycC y) = CycC (f x y)

    liftNum2 op f x y
      | Just x'@CycC{} <- exact x, Just y'@CycC{} <- exact y = liftNum2 op f x' y'
      | otherwise                                            = lift2 f x y

--- XXX Need UndecidableInstances for this...but we *must* call
--- liftNum/liftNum2 on constants to ensure they are properly simplified.
--- The other option would be to forgo the Const instance and push it into the
--- Exp instance.
instance (Typed a, Num (Const a), LiftNum (Const a), Num (Exp a)) => LiftNum (Exp a) where
    liftNum Neg _ e
      | isZero e = 0

    liftNum Neg _ (UnopE Neg x)      = x
    liftNum Neg _ (BinopE Sub e1 e2) = e2 - e1
    liftNum Neg _ (ComplexE a b)     = ComplexE (-a) (-b)

    liftNum op f (ConstE c) = ConstE $ liftNum op f c

    liftNum op _ e = UnopE op e

    liftNum2 op f (ConstE c1) (ConstE c2) = ConstE $ liftNum2 op f c1 c2

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

    -- Simplify multiplication by i and -i
    liftNum2 Mul _ (ConstE k) (ComplexE a b)
      | isI k    = ComplexE (-b) a
      | isNegI k = ComplexE b (-a)

    liftNum2 Mul _ (ComplexE a b) (ConstE k)
      | isI k    = ComplexE (-b) a
      | isNegI k = ComplexE b (-a)

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

    -- Standard component-wise add/subtract
    liftNum2 Add _ (ComplexE a b) (ComplexE c d) = ComplexE (a + c) (b + d)
    liftNum2 Sub _ (ComplexE a b) (ComplexE c d) = ComplexE (a - c) (b - d)

    -- 3-multiply/5-add complex multiplication
    liftNum2 Mul _ (ComplexE a b) (ComplexE c d) | threeMults =
          ComplexE (t1 - t2) (t1 + t3)
      where
        t1 = a*(c+d)
        t2 = d*(b+a)
        t3 = c*(b-a)

    -- Usual 4-multiply/4-add complex multiplication
    liftNum2 Mul _ (ComplexE a b) (ComplexE c d) =
        ComplexE (a*c - b*d) (b*c + a*d)

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

instance Num (Const Rational) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = RationalC . fromInteger

instance Num (Const Float) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = RationalC . fromInteger

instance Num (Const Double) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = RationalC . fromInteger

instance Num (Const (Complex Float)) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = RationalC . fromInteger

instance Num (Const (Complex Double)) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = RationalC . fromInteger

instance Num (Exp Int) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = ConstE . fromInteger

instance Num (Exp Integer) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = ConstE . fromInteger

instance Num (Exp Float) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = ConstE . fromInteger

instance Num (Exp Double) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = ConstE . fromInteger

instance Num (Exp Rational) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = ConstE . fromInteger

instance Num (Exp (Complex Float)) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = ConstE . fromInteger

instance Num (Exp (Complex Double)) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = ConstE . fromInteger

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

    liftIntegral2 _op f c1 c2 = lift2 f c1 c2

instance Enum (Const Int) where
    toEnum n = IntC (fromIntegral n)

    fromEnum (IntC i)    = fromIntegral i
    fromEnum RationalC{} = error "can't happen"
    fromEnum W{}         = error "can't happen"

instance Real (Const Int) where
    toRational (IntC i)    = toRational i
    toRational RationalC{} = error "can't happen"
    toRational W{}         = error "can't happen"

instance Integral (Const Int) where
    quot = liftIntegral2 Quot quot
    rem  = liftIntegral2 Rem rem

    x `quotRem` y = (x `quot` y, x `rem` y)

    toInteger (IntC i)    = fromIntegral i
    toInteger RationalC{} = error "can't happen"
    toInteger W{}         = error "can't happen"

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
-- Fractional instances
--
--------------------------------------------------------------------------------

-- | Class to lift 'Fractional' operators.
class LiftFrac b where
    -- | Lift a binary operation on 'Num' to the type 'b
    liftFrac2 :: Binop -> (forall a . Fractional a => a -> a -> a) -> b -> b -> b

instance (Fractional a, ToConst a, Fractional (Const a)) => LiftFrac (Const a) where
    liftFrac2 Div _ c1 (W n k x) | isOne c1 = mkW (-k % n) (1/x)

    liftFrac2 Div _ (W n k x) (W n' k' y) = mkW (k % n - k' % n') (x/y)

    liftFrac2 _op f (RationalC x) (RationalC y) = RationalC (f x y)

    liftFrac2 op f (W _ _ c) y = liftFrac2 op f c y
    liftFrac2 op f x (W _ _ c) = liftFrac2 op f x c

    liftFrac2 Div _ c1 c2 | isOne c2 =
        c1

    liftFrac2 _op f (CycC x) (CycC y) = CycC (f x y)

    liftFrac2 op f x y
      | Just x'@CycC{} <- exact x, Just y'@CycC{} <- exact y = liftFrac2 op f x' y'
      | otherwise                                            = lift2 f x y

instance (LiftFrac (Const a), Num (Exp a)) => LiftFrac (Exp a) where
    liftFrac2 op f (ConstE c1) (ConstE c2) = ConstE $ liftFrac2 op f c1 c2

    liftFrac2 Div _ e1 e2 | isOne e2 =
        e1

    liftFrac2 op _ e1 e2 = BinopE op e1 e2

instance Fractional (Const Rational) where
    (/) = liftFrac2 Div (/)

    fromRational = RationalC

instance Fractional (Const Float) where
    (/) = liftFrac2 Div (/)

    fromRational = RationalC

instance Fractional (Const Double) where
    (/) = liftFrac2 Div (/)

    fromRational = RationalC

instance Fractional (Const (Complex Float)) where
    (/) = liftFrac2 Div (/)

    fromRational = RationalC

instance Fractional (Const (Complex Double)) where
    (/) = liftFrac2 Div (/)

    fromRational = RationalC

instance Fractional (Exp Float) where
    (/) = liftFrac2 Div (/)

    fromRational = ConstE . fromRational

instance Fractional (Exp Double) where
    (/) = liftFrac2 Div (/)

    fromRational = ConstE . fromRational

instance Fractional (Exp (Complex Float)) where
    (/) = liftFrac2 Div (/)

    fromRational = ConstE . fromRational

instance Fractional (Exp (Complex Double)) where
    (/) = liftFrac2 Div (/)

    fromRational = ConstE . fromRational

--------------------------------------------------------------------------------
--
-- Floating instances
--
--------------------------------------------------------------------------------

-- | Class to lift 'Floating' operators.
class LiftFloating b where
    -- | Lift a binary operation on 'Floating' to the type 'b
    liftFloating :: Unop -> (forall a . Floating a => a -> a) -> b -> b

instance (Floating a, Typed a, ToConst a) => LiftFloating (Const a) where
    liftFloating _op f c = lift f c

instance LiftFloating (Const a) => LiftFloating (Exp a) where
    liftFloating op f (ConstE c) = ConstE $ liftFloating op f c

    liftFloating op _ e = UnopE op e

instance Floating (Const Float) where
    pi = toConst pi

    exp   = liftFloating Exp exp
    log   = liftFloating Log log
    asin  = liftFloating Asin asin
    acos  = liftFloating Acos acos
    atan  = liftFloating Atan atan
    sinh  = liftFloating Sinh sinh
    cosh  = liftFloating Cosh cosh
    asinh = liftFloating Asinh asinh
    acosh = liftFloating Acosh acosh
    atanh = liftFloating Atanh atanh
    sin   = liftFloating Sin sin
    cos   = liftFloating Cos cos

instance Floating (Const Double) where
    pi = toConst pi

    exp   = liftFloating Exp exp
    log   = liftFloating Log log
    asin  = liftFloating Asin asin
    acos  = liftFloating Acos acos
    atan  = liftFloating Atan atan
    sinh  = liftFloating Sinh sinh
    cosh  = liftFloating Cosh cosh
    asinh = liftFloating Asinh asinh
    acosh = liftFloating Acosh acosh
    atanh = liftFloating Atanh atanh
    sin   = liftFloating Sin sin
    cos   = liftFloating Cos cos

instance Floating (Const (Complex Float)) where
    pi = toConst pi

    exp   = liftFloating Exp exp
    log   = liftFloating Log log
    asin  = liftFloating Asin asin
    acos  = liftFloating Acos acos
    atan  = liftFloating Atan atan
    sinh  = liftFloating Sinh sinh
    cosh  = liftFloating Cosh cosh
    asinh = liftFloating Asinh asinh
    acosh = liftFloating Acosh acosh
    atanh = liftFloating Atanh atanh
    sin   = liftFloating Sin sin
    cos   = liftFloating Cos cos

instance Floating (Const (Complex Double)) where
    pi = toConst pi

    exp   = liftFloating Exp exp
    log   = liftFloating Log log
    asin  = liftFloating Asin asin
    acos  = liftFloating Acos acos
    atan  = liftFloating Atan atan
    sinh  = liftFloating Sinh sinh
    cosh  = liftFloating Cosh cosh
    asinh = liftFloating Asinh asinh
    acosh = liftFloating Acosh acosh
    atanh = liftFloating Atanh atanh
    sin   = liftFloating Sin sin
    cos   = liftFloating Cos cos

instance Floating (Exp Float) where
    pi = ConstE pi

    exp   = liftFloating Exp exp
    log   = liftFloating Log log
    asin  = liftFloating Asin asin
    acos  = liftFloating Acos acos
    atan  = liftFloating Atan atan
    sinh  = liftFloating Sinh sinh
    cosh  = liftFloating Cosh cosh
    asinh = liftFloating Asinh asinh
    acosh = liftFloating Acosh acosh
    atanh = liftFloating Atanh atanh
    sin   = liftFloating Sin sin
    cos   = liftFloating Cos cos

instance Floating (Exp Double) where
    pi = ConstE pi

    exp   = liftFloating Exp exp
    log   = liftFloating Log log
    asin  = liftFloating Asin asin
    acos  = liftFloating Acos acos
    atan  = liftFloating Atan atan
    sinh  = liftFloating Sinh sinh
    cosh  = liftFloating Cosh cosh
    asinh = liftFloating Asinh asinh
    acosh = liftFloating Acosh acosh
    atanh = liftFloating Atanh atanh
    sin   = liftFloating Sin sin
    cos   = liftFloating Cos cos

instance Floating (Exp (Complex Float)) where
    pi = ConstE pi

    exp   = liftFloating Exp exp
    log   = liftFloating Log log
    asin  = liftFloating Asin asin
    acos  = liftFloating Acos acos
    atan  = liftFloating Atan atan
    sinh  = liftFloating Sinh sinh
    cosh  = liftFloating Cosh cosh
    asinh = liftFloating Asinh asinh
    acosh = liftFloating Acosh acosh
    atanh = liftFloating Atanh atanh
    sin   = liftFloating Sin sin
    cos   = liftFloating Cos cos

instance Floating (Exp (Complex Double)) where
    pi = ConstE pi

    exp   = liftFloating Exp exp
    log   = liftFloating Log log
    asin  = liftFloating Asin asin
    acos  = liftFloating Acos acos
    atan  = liftFloating Atan atan
    sinh  = liftFloating Sinh sinh
    cosh  = liftFloating Cosh cosh
    asinh = liftFloating Asinh asinh
    acosh = liftFloating Acosh acosh
    atanh = liftFloating Atanh atanh
    sin   = liftFloating Sin sin
    cos   = liftFloating Cos cos

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

instance (Typed a, ToConst a, Eq a) => IsEq (Exp a) (Exp Bool) where
    e1 .==. e2 = BBinopE Eq e1 e2
    e1 ./=. e2 = BBinopE Ne e1 e2

instance (Typed a, ToConst a, Eq a) => IsOrd (Exp a) (Exp Bool) where
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

isI :: Const (Complex a) -> Bool
isI RationalC{}    = False
isI (ComplexC r i) = isZero r && isOne i
isI (W _ _ c)      = isI c
isI (CycC c)       = c == i

isNegI :: Const (Complex a) -> Bool
isNegI RationalC{}    = False
isNegI (ComplexC r i) = isZero r && isNegOne i
isNegI (W _ _ c)      = isNegI c
isNegI (CycC c)       = c == -i

instance IsZeroOne (Const a) where
    isZero (IntC 0)       = True
    isZero (IntegerC 0)   = True
    isZero (FloatC 0)     = True
    isZero (DoubleC 0)    = True
    isZero (RationalC 0)  = True
    isZero (ComplexC 0 0) = True
    isZero (CycC 0)       = True
    isZero _              = False

    isOne (IntC 1)       = True
    isOne (IntegerC 1)   = True
    isOne (FloatC 1)     = True
    isOne (DoubleC 1)    = True
    isOne (RationalC 1)  = True
    isOne (ComplexC 1 i) = isZero i
    isOne (W n k _)      = k % n == 0 || k % n == 1
    isOne (CycC 1)       = True
    isOne _              = False

    isNegOne (IntC (-1))       = True
    isNegOne (IntegerC (-1))   = True
    isNegOne (FloatC (-1))    = True
    isNegOne (DoubleC (-1))    = True
    isNegOne (RationalC (-1))  = True
    isNegOne (ComplexC (-1) i) = isZero i
    isNegOne (W _ _ c)         = isNegOne c
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

mkW :: RootOfUnity (Const a) => Ratio Int -> Const a -> Const a
mkW r c = W n (k `mod` n) c
  where
    k = numerator r
    n = denominator r

isConstE :: Exp a -> Bool
isConstE ConstE{} = True
isConstE _        = False

-- | Create an 'Exp Int' from an 'Int'.
intE :: Int -> Exp Int
intE = ConstE . IntC

-- | Create an 'Exp (Complex Double)' from a 'Complex Double'.
complexE :: RealFloatConst a => Complex a -> Exp (Complex a)
complexE (r :+ i) = ConstE $ ComplexC (toConst r) (toConst i)

-- | Extract the complex and real portions of a 'Const (Complex a)'.
unComplexC :: RealFloatConst a => Const (Complex a) -> (Const a, Const a)
unComplexC (RationalC r)  = (RationalC r, 0)
unComplexC (ComplexC r i) = (r, i)
unComplexC (W _ _ c)      = unComplexC c
unComplexC x@CycC{}       = (toConst r, toConst i)
  where
    r :+ i = fromConst x

-- | Extract the complex and real portions of an 'Exp (Complex a)'.
unComplexE :: RealFloatConst a => Exp (Complex a) -> (Exp a, Exp a)
unComplexE (ConstE c) =
    (ConstE cr, ConstE ci)
  where
    (cr, ci) = unComplexC c

unComplexE (ComplexE er ei) =
    (er, ei)

unComplexE e =
    (ReE e, ImE e)

-- | Force extraction of the complex and real portions of an 'Exp (Complex a)'.
forceUnComplexE :: RealFloatConst a => Exp (Complex a) -> (Exp a, Exp a)
forceUnComplexE (ConstE c) =
    (ConstE cr, ConstE ci)
  where
    (cr, ci) = unComplexC c

forceUnComplexE (ComplexE er ei) =
    (er, ei)

forceUnComplexE (UnopE Neg e) =
    forceUnComplexE $ - (ensureComplexE e)

forceUnComplexE (BinopE Add e1 e2) =
    forceUnComplexE $ ensureComplexE e1 + ensureComplexE e2

forceUnComplexE (BinopE Sub e1 e2) =
    forceUnComplexE $ ensureComplexE e1 - ensureComplexE e2

forceUnComplexE (BinopE Mul e1 e2) =
    forceUnComplexE $ ensureComplexE e1 * ensureComplexE e2

forceUnComplexE e =
    (ReE e, ImE e)

-- | Ensure that a complex expression is represented using its constituent real
-- and imaginary parts.
ensureComplexE :: RealFloatConst a => Exp (Complex a) -> Exp (Complex a)
ensureComplexE e = ComplexE er ei
  where
    (er, ei) = forceUnComplexE e

true, false :: Exp Bool
true  = ConstE (BoolC True)
false = ConstE (BoolC False)
