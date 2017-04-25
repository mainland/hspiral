{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Spiral.Backend.C.CExp
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.CExp (
    CExp(..),
    isComplex,
    unComplex
  ) where

import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Text.PrettyPrint.Mainland hiding (flatten)

import Spiral.Backend.C.Util
import Spiral.Exp

-- | A compiled C expression.
data CExp -- A known integer constant
          = CInt Int
          -- A known long long (at least 64 bit) constant
          | CLLInt Integer
          -- A known double constant
          | CDouble Rational
          -- A complex number.
          | CComplex CExp CExp
          -- C expression
          | CExp C.Exp
          -- C initializer
          | CInit C.Initializer
  deriving (Ord, Show)

instance Eq CExp where
    CInt x       == CInt x'        = x' == x
    CLLInt x     == CLLInt x'      = x' == x
    CDouble x    == CDouble x'     = x' == x
    CComplex r i == CComplex r' i' = r' == r && i' == i
    CExp e       == CExp e'        = e' == e
    CInit i      == CInit i'       = i' == i

    x@CDouble{}  == x'@CInt{}    = x' == x
    x@CComplex{} == x'@CDouble{} = x' == x
    x@CComplex{} == x'@CInt{}    = x' == x

    x@CInt{} == CComplex x' 0 = x' == x
    CInt x   == CDouble x'    = x' == fromIntegral x

    _ == _ = False

isComplex :: CExp -> Bool
isComplex CComplex{} = True
isComplex _          = False

unComplex :: CExp -> (CExp, CExp)
unComplex (CComplex r i) = (r, i)
unComplex ce             = (CExp [cexp|creal($ce)|], CExp [cexp|cimag($ce)|])

instance Pretty CExp where
    ppr (CInt x)      = ppr x
    ppr (CLLInt x)    = ppr x
    ppr (CDouble x)   = ppr x
    ppr ce@CComplex{} = ppr [cexp|$ce|]
    ppr (CExp ce)     = ppr ce
    ppr (CInit cinit) = ppr cinit

instance ToExp CExp where
    toExp (CInt i)    = const [cexp|$int:i|]
    toExp (CLLInt i)  = const [cexp|$llint:i|]
    toExp (CDouble x) = const [cexp|$double:x|]
    toExp (CExp ce)   = const ce
    toExp (CInit _)   = error "ToExp CExp: cannot convert CInit to a C expression"

    toExp (CComplex 0 0)     = const [cexp|0|]
    toExp (CComplex 0 1)     = const [cexp|I|]
    toExp (CComplex 0 (-1))  = const [cexp|-I|]
    toExp (CComplex 0 ce2)   = const [cexp|$ce2 * I|]
    toExp (CComplex ce1 0)   = toExp ce1
    toExp (CComplex ce1 ce2) = const [cexp|$ce1 + $ce2 * I|]

instance ToInitializer CExp where
    toInitializer (CInit cinit) = cinit
    toInitializer ce            = [cinit|$ce|]

instance LiftNum CExp where
    liftNum _ f (CInt x)    = CInt (f x)
    liftNum _ f (CLLInt x)  = CLLInt (f x)
    liftNum _ f (CDouble x) = CDouble (f x)

    liftNum Neg _ (CComplex cr ci)    = CComplex (-cr) (-ci)
    liftNum Neg _ (CExp [cexp|-$ce|]) = CExp ce

    liftNum Neg    _ ce  = CExp [cexp|-$ce|]
    liftNum Abs    _ _ce = error "LiftNum CExp: cannot lift abs"
    liftNum Signum _ ce  = CExp [cexp|$ce == 0 ? 0 : ($ce > 0 ? 1 : -1)|]

    liftNum2 _ f (CInt x)    (CInt y)    = CInt (f x y)
    liftNum2 _ f (CLLInt x)  (CLLInt y)  = CLLInt (f x y)
    liftNum2 _ f (CDouble x) (CDouble y) = CDouble (f x y)

    liftNum2 Add _ (CComplex a b) (CComplex c d) =
        CComplex (a + c) (b + d)

    liftNum2 Sub _ (CComplex a b) (CComplex c d) =
        CComplex (a - c) (b - d)

    liftNum2 Mul _ (CComplex a b) (CComplex c d) =
        CComplex (a*c - b*d) (b*c + a*d)

    liftNum2 Add _ ce1 (CExp [cexp|-$ce2|]) = CExp [cexp|$ce1 - $ce2|]

    liftNum2 Sub _ ce1 (CExp [cexp|-$ce2|]) = CExp [cexp|$ce1 + $ce2|]

    liftNum2 Add  _ ce1 ce2 = CExp [cexp|$ce1 + $ce2|]
    liftNum2 Sub  _ ce1 ce2 = CExp [cexp|$ce1 - $ce2|]
    liftNum2 Mul  _ ce1 ce2 = CExp [cexp|$ce1 * $ce2|]
    liftNum2 Quot _ ce1 ce2 = CExp [cexp|$ce1 / $ce2|]
    liftNum2 Rem  _ ce1 ce2 = CExp [cexp|$ce1 % $ce2|]
    liftNum2 Div  _ ce1 ce2 = CExp [cexp|$ce1 / $ce2|]

instance Num CExp where
    (+) = liftNum2Opt Add (+)
    (-) = liftNum2Opt Sub (-)
    (*) = liftNum2Opt Mul (*)

    abs ce@CInt{} = liftNumOpt Abs abs ce
    abs ce        = CExp [cexp|abs($ce)|]

    negate = liftNumOpt Neg negate

    signum  = liftNumOpt Signum signum

    fromInteger = CInt . fromIntegral

instance Enum CExp where
    toEnum n = CInt (fromIntegral n)

    fromEnum (CInt n) = fromIntegral n
    fromEnum _        = error "Enum Exp: fromEnum not implemented"

instance Real CExp where
    toRational (CInt n) = toRational n
    toRational _        = error "Real CExp: toRational not implemented"

instance Integral CExp where
    CExp [cexp|$int:x|] `quotRem` y = CInt (fromIntegral x) `quotRem` y
    x `quotRem` CExp [cexp|$int:y|] = x `quotRem` CInt (fromIntegral y)

    CLLInt x `quotRem` CLLInt y = (CLLInt q, CLLInt r)
      where
        (q, r) = x `quotRem` y

    CInt x `quotRem` CInt y = (CInt q, CInt r)
      where
        (q, r) = x `quotRem` y

    ce1 `quotRem` ce2 =
        (CExp [cexp|$ce1 / $ce2|], CExp [cexp|$ce1 % $ce2|])

    toInteger (CInt i)   = fromIntegral i
    toInteger (CLLInt i) = fromIntegral i
    toInteger _          = error "Integral CExp: toInteger not implemented"

instance Fractional CExp where
    fromRational = CDouble . fromRational

    CDouble x / CDouble y =
        CDouble (x / y)

    CComplex a b / CComplex c d =
        CComplex ((a*c + b*d) / (c*c + d*d)) ((b*c + a*d) / (c*c + d*d))

    ce1 / ce2 =
        CExp [cexp|$ce1 / $ce2|]
