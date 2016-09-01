{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Spiral.Backend.C.CExp
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.CExp (
    CExp(..),
    ToCExp(..),
    unComplex
  ) where

import Data.Complex
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Text.PrettyPrint.Mainland hiding (flatten)

import Spiral.Backend.C.Util
import Spiral.Exp

-- | A compiled C expression.
data CExp a where
    -- A known integer constant
    CInt :: Int -> CExp Int

    -- A known double constant
    CDouble :: Rational -> CExp Double

    -- A complex number.
    CComplex :: CExp Double -> CExp Double -> CExp (Complex Double)

    -- C expression
    CExp :: C.Exp -> CExp a

    -- C initializer
    CInit :: C.Initializer -> CExp a

unComplex :: CExp (Complex Double) -> (CExp Double, CExp Double)
unComplex (CComplex r i) = (r, i)
unComplex ce             = (CExp [cexp|creal($ce)|], CExp [cexp|cimag($ce)|])

deriving instance Eq (CExp a)
deriving instance Show (CExp a)

instance Ord (CExp a) where
    compare (CInt x) (CInt y) = compare x y
    compare CInt{}   _        = LT

    compare (CDouble x) (CDouble y) = compare x y
    compare CDouble{}   _           = LT

    compare (CComplex r i) (CComplex r' i') = compare (r, i) (r', i')
    compare CComplex{}     _               = LT

    compare (CExp e1) (CExp e2)  = compare e1 e2
    compare CExp{}    CInt{}     = GT
    compare CExp{}    CDouble{}  = GT
    compare CExp{}    CComplex{} = GT
    compare CExp{}    _          = LT

    compare (CInit i1) (CInit i2) = compare i1 i2
    compare CInit{}    _          = GT

instance Pretty (CExp a) where
    ppr (CInt x)      = ppr x
    ppr (CDouble x)   = ppr x
    ppr ce@CComplex{} = ppr [cexp|$ce|]
    ppr (CExp ce)     = ppr ce
    ppr (CInit cinit) = ppr cinit

instance ToExp (CExp a) where
    toExp (CInt i)    = const [cexp|$int:i|]
    toExp (CDouble x) = const [cexp|$double:x|]
    toExp (CExp ce)   = const ce
    toExp (CInit _)   = error "ToExp CExp: cannot convert CInit to a C expression"

    toExp (CComplex ce1 ce2)
      | isZero ce1 && isZero ce2   = const [cexp|0|]
      | isZero ce1 && isOne ce2    = const [cexp|I|]
      | isZero ce1 && isNegOne ce2 = const [cexp|-I|]
      | isZero ce1                 = const [cexp|$ce2 * I|]
      | isZero ce2                 = toExp ce1
      | otherwise                  = const [cexp|$ce1 + $ce2 * I|]

instance ToInitializer (CExp a) where
    toInitializer (CInit cinit) = cinit
    toInitializer ce            = [cinit|$ce|]

instance LiftNum (CExp a) where
    isIntegral x (CInt y)                 = y == fromInteger x
    isIntegral x (CDouble y)              = y == fromInteger x
    isIntegral x (CComplex r i)           = isIntegral x r && isZero i
    isIntegral x (CExp [cexp|$int:y|])    = y == fromInteger x
    isIntegral x (CExp [cexp|$double:y|]) = y == fromInteger x
    isIntegral _ _                        = False

    liftNum_ _ f (CInt x)    = CInt (f x)
    liftNum_ _ f (CDouble x) = CDouble (f x)

    liftNum_ Neg _ (CComplex cr ci) =
        CComplex (-cr) (-ci)

    liftNum_ Neg _ (CExp [cexp|-$ce|]) = CExp ce

    liftNum_ Neg    _ ce  = CExp [cexp|-$ce|]
    liftNum_ Abs    _ _ce = error "LiftNum CExp: cannot lift abs"
    liftNum_ Signum _ ce  = CExp [cexp|$ce == 0 ? 0 : ($ce > 0 ? 1 : -1)|]

    liftNum2_ _ f (CInt x)    (CInt y)    = CInt (f x y)
    liftNum2_ _ f (CDouble x) (CDouble y) = CDouble (f x y)

    liftNum2_ Add _ (CComplex a b) (CComplex c d) =
        CComplex (a + c) (b + d)

    liftNum2_ Sub _ (CComplex a b) (CComplex c d) =
        CComplex (a - c) (b - d)

    liftNum2_ Mul _ (CComplex a b) (CComplex c d) =
        CComplex (a*c - b*d) (b*c + a*d)

    liftNum2_ Add _ ce1 (CExp [cexp|-$ce2|]) = CExp [cexp|$ce1 - $ce2|]

    liftNum2_ Sub _ ce1 (CExp [cexp|-$ce2|]) = CExp [cexp|$ce1 + $ce2|]

    liftNum2_ Add _ ce1 ce2 = CExp [cexp|$ce1 + $ce2|]
    liftNum2_ Sub _ ce1 ce2 = CExp [cexp|$ce1 - $ce2|]
    liftNum2_ Mul _ ce1 ce2 = CExp [cexp|$ce1 * $ce2|]
    liftNum2_ Div _ ce1 ce2 = CExp [cexp|$ce1 / $ce2|]

instance Num (CExp Int) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs ce@CInt{} = liftNum Abs abs ce
    abs ce        = CExp [cexp|abs($ce)|]

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = CInt . fromIntegral

instance Num (CExp Double) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs ce@CDouble{} = liftNum Abs abs ce
    abs ce           = CExp [cexp|fabs($ce)|]

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = CDouble . fromInteger

instance Num (CExp (Complex Double)) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs ce@CComplex{} = liftNum Abs abs ce
    abs ce            = CExp [cexp|cabs($ce)|]

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger x = CComplex (CDouble (fromInteger x)) 0

instance Enum (CExp Int) where
    toEnum n = CInt (fromIntegral n)

    fromEnum (CInt n) = fromIntegral n
    fromEnum _        = error "Enum Exp: fromEnum not implemented"

instance Real (CExp Int) where
    toRational (CInt n) = toRational n
    toRational _        = error "Real CExp: toRational not implemented"

instance Integral (CExp Int) where
    CExp [cexp|$int:x|] `quotRem` y = CInt (fromIntegral x) `quotRem` y
    x `quotRem` CExp [cexp|$int:y|] = x `quotRem` CInt (fromIntegral y)

    CInt x `quotRem` CInt y = (CInt q, CInt r)
      where
        (q, r) = x `quotRem` y

    ce1 `quotRem` ce2 =
        (CExp [cexp|$ce1 / $ce2|], CExp [cexp|$ce1 % $ce2|])

    toInteger (CInt i) = fromIntegral i
    toInteger _        = error "Integral CExp: fromInteger not implemented"

-- | Compile a value to a C expression.
class ToCExp a b | a -> b where
    toCExp :: a -> CExp b

instance ToCExp Int Int where
    toCExp = CInt

instance ToCExp Double Double where
    toCExp = CDouble . toRational

instance ToCExp (CExp a) a where
    toCExp ce = ce

instance ToCExp (Const Int) Int where
    toCExp (IntC x) = CInt x

instance ToCExp (Const Integer) Int where
    toCExp (IntegerC x) = CInt (fromIntegral x)

instance ToCExp (Const Double) Double where
    toCExp (PiC x)     = CDouble (toRational (fromRational x * pi :: Double))
    toCExp (DoubleC x) = CDouble (toRational x)

instance ToCExp (Const (Complex Double)) (Complex Double) where
    toCExp (ComplexC e1 e2) = CComplex (toCExp e1) (toCExp e2)
    toCExp e@RouC{}         = toCExp (flatten e)

-- XXX Need UndecidableInstances for this one :(
instance ToCExp (Const a) a => ToCExp (Exp a) a where
    toCExp (ConstE c) = toCExp c
    toCExp (VarE v)   = CExp [cexp|$id:v|]
