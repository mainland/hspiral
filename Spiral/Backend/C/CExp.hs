{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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

    toExp (CComplex 0 0)     = const [cexp|0|]
    toExp (CComplex 0 1)     = const [cexp|I|]
    toExp (CComplex 0 (-1))  = const [cexp|-I|]
    toExp (CComplex 0 ce2)   = const [cexp|$ce2 * I|]
    toExp (CComplex ce1 0)   = toExp ce1
    toExp (CComplex ce1 ce2) = const [cexp|$ce1 + $ce2 * I|]

instance ToInitializer (CExp a) where
    toInitializer (CInit cinit) = cinit
    toInitializer ce            = [cinit|$ce|]

instance LiftNum (CExp a) where
    liftNum _ f (CInt x)    = CInt (f x)
    liftNum _ f (CDouble x) = CDouble (f x)

    liftNum Neg _ (CComplex cr ci)    = CComplex (-cr) (-ci)
    liftNum Neg _ (CExp [cexp|-$ce|]) = CExp ce

    liftNum Neg    _ ce  = CExp [cexp|-$ce|]
    liftNum Abs    _ _ce = error "LiftNum CExp: cannot lift abs"
    liftNum Signum _ ce  = CExp [cexp|$ce == 0 ? 0 : ($ce > 0 ? 1 : -1)|]

    liftNum2 _ f (CInt x)    (CInt y)    = CInt (f x y)
    liftNum2 _ f (CDouble x) (CDouble y) = CDouble (f x y)

    liftNum2 Add _ (CComplex a b) (CComplex c d) =
        CComplex (a + c) (b + d)

    liftNum2 Sub _ (CComplex a b) (CComplex c d) =
        CComplex (a - c) (b - d)

    liftNum2 Mul _ (CComplex a b) (CComplex c d) =
        CComplex (a*c - b*d) (b*c + a*d)

    liftNum2 Add _ ce1 (CExp [cexp|-$ce2|]) = CExp [cexp|$ce1 - $ce2|]

    liftNum2 Sub _ ce1 (CExp [cexp|-$ce2|]) = CExp [cexp|$ce1 + $ce2|]

    liftNum2 Add _ ce1 ce2 = CExp [cexp|$ce1 + $ce2|]
    liftNum2 Sub _ ce1 ce2 = CExp [cexp|$ce1 - $ce2|]
    liftNum2 Mul _ ce1 ce2 = CExp [cexp|$ce1 * $ce2|]
    liftNum2 Div _ ce1 ce2 = CExp [cexp|$ce1 / $ce2|]
    liftNum2 Rem _ ce1 ce2 = CExp [cexp|$ce1 % $ce2|]

instance Num (CExp Int) where
    (+) = liftNum2Opt Add (+)
    (-) = liftNum2Opt Sub (-)
    (*) = liftNum2Opt Mul (*)

    abs ce@CInt{} = liftNumOpt Abs abs ce
    abs ce        = CExp [cexp|abs($ce)|]

    negate = liftNumOpt Neg negate

    signum  = liftNumOpt Signum signum

    fromInteger = CInt . fromIntegral

instance Num (CExp Double) where
    (+) = liftNum2Opt Add (+)
    (-) = liftNum2Opt Sub (-)
    (*) = liftNum2Opt Mul (*)

    abs ce@CDouble{} = liftNumOpt Abs abs ce
    abs ce           = CExp [cexp|fabs($ce)|]

    negate = liftNumOpt Neg negate

    signum  = liftNumOpt Signum signum

    fromInteger = CDouble . fromInteger

instance Num (CExp (Complex Double)) where
    (+) = liftNum2Opt Add (+)
    (-) = liftNum2Opt Sub (-)
    (*) = liftNum2Opt Mul (*)

    abs ce@CComplex{} = liftNumOpt Abs abs ce
    abs ce            = CExp [cexp|cabs($ce)|]

    negate = liftNumOpt Neg negate

    signum  = liftNumOpt Signum signum

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

instance Fractional (CExp Double) where
    fromRational = CDouble . fromRational

    CDouble x / CDouble y = CDouble (x / y)
    ce1 / ce2 = CExp [cexp|$ce1 / $ce2|]

instance Fractional (CExp (Complex Double)) where
    fromRational x = CComplex (fromRational x) 0

    CComplex a b / CComplex c d =
        CComplex ((a*c + b*d) / (c*c + d*d)) ((b*c + a*d) / (c*c + d*d))

    ce1 / ce2 = CExp [cexp|$ce1 / $ce2|]

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

instance ToCExp (Exp Int) Int where
    toCExp (ConstE c) = toCExp c
    toCExp (VarE v)   = CExp [cexp|$id:v|]

    toCExp (UnopE op e) =
        go op (toCExp e)
      where
        go Neg ce    = -ce
        go Abs ce    = abs ce
        go Signum ce = signum ce

    toCExp (BinopE op e1 e2) =
        go op (toCExp e1) (toCExp e2)
      where
        go Add ce1 ce2 = ce1 + ce2
        go Sub ce1 ce2 = ce1 - ce2
        go Mul ce1 ce2 = ce1 * ce2
        go Div ce1 ce2 = ce1 `div` ce2
        go Rem ce1 ce2 = ce1 `rem` ce2

instance ToCExp (Exp Double) Double where
    toCExp (ConstE c) = toCExp c
    toCExp (VarE v)   = CExp [cexp|$id:v|]

    toCExp (UnopE op e) =
        go op (toCExp e)
      where
        go Neg ce    = -ce
        go Abs ce    = abs ce
        go Signum ce = signum ce

    toCExp (BinopE op e1 e2) =
        go op (toCExp e1) (toCExp e2)
      where
        go Add ce1 ce2 = ce1 + ce2
        go Sub ce1 ce2 = ce1 - ce2
        go Mul ce1 ce2 = ce1 * ce2
        go Div ce1 ce2 = ce1 / ce2
        go Rem _ _ = error "can't happen"

instance ToCExp (Exp (Complex Double)) (Complex Double) where
    toCExp (ConstE c) = toCExp c
    toCExp (VarE v)   = CExp [cexp|$id:v|]

    toCExp (UnopE op e) =
        go op (toCExp e)
      where
        go Neg ce    = -ce
        go Abs ce    = abs ce
        go Signum ce = signum ce

    toCExp (BinopE op e1 e2) =
        go op (toCExp e1) (toCExp e2)
      where
        go Add ce1 ce2 = ce1 + ce2
        go Sub ce1 ce2 = ce1 - ce2
        go Mul ce1 ce2 = ce1 * ce2
        go Div ce1 ce2 = ce1 / ce2
        go Rem _ _ = error "can't happen"
