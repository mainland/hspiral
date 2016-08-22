{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      :  Spiral.Backend.C.CExp
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.CExp (
    CExp(..)
  ) where

import Data.Complex
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Text.PrettyPrint.Mainland

import Spiral.Backend.C.Util
import Spiral.Util.Lift

data CExp a where
    -- | A known integer constant
    CInt :: Integer -> CExp Integer

    -- | A known double constant
    CDouble :: Rational -> CExp Double

    -- | A complex number.
    CComplex :: CExp Double -> CExp Double -> CExp (Complex Double)

    -- | C expression
    CExp :: C.Exp -> CExp a

    -- | C initializer
    CInit :: C.Initializer -> CExp a

deriving instance Eq (CExp a)
deriving instance Show (CExp a)

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
    isIntegral x (CInt y)       = y == fromInteger x
    isIntegral x (CDouble y)    = y == fromInteger x
    isIntegral x (CComplex r i) = isIntegral x r && isZero i
    isIntegral _ _              = False

    liftNum_ _ f (CInt x)    = CInt (f x)
    liftNum_ _ f (CDouble x) = CDouble (f x)

    liftNum_ Neg    _ ce = CExp [cexp|-$ce|]
    liftNum_ Abs    _ ce = CExp [cexp|abs($ce)|]
    liftNum_ Signum _ _  = error "LiftNum CExp: cannot lift signum"

    liftNum2_ _ f (CInt x)    (CInt y)    = CInt (f x y)
    liftNum2_ _ f (CDouble x) (CDouble y) = CDouble (f x y)

    liftNum2_ Add _ ce1 (CExp [cexp|-$ce2|]) = CExp [cexp|$ce1 - $ce2|]

    liftNum2_ Sub _ ce1 (CExp [cexp|-$ce2|]) = CExp [cexp|$ce1 + $ce2|]

    liftNum2_ Add _ ce1 ce2 = CExp [cexp|$ce1 + $ce2|]
    liftNum2_ Sub _ ce1 ce2 = CExp [cexp|$ce1 - $ce2|]
    liftNum2_ Mul _ ce1 ce2 = CExp [cexp|$ce1 * $ce2|]
    liftNum2_ Div _ ce1 ce2 = CExp [cexp|$ce1 / $ce2|]

instance Num (CExp Integer) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = CInt

instance Num (CExp Double) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = CDouble . fromInteger

instance Num (CExp (Complex Double)) where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger x = CComplex (CDouble (fromInteger x)) 0
