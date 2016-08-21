{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      :  Spiral.Backend.C.CExp
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.CExp (
    CExp(..)
  ) where

import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Text.PrettyPrint.Mainland

import Spiral.Backend.C.Util
import Spiral.Util.Lift

data CExp -- | A known integer constant
          = CInt Integer
          -- | A known double constant
          | CDouble Rational
          -- | C expression
          | CExp C.Exp
          -- | C initializer
          | CInit C.Initializer
  deriving (Eq, Ord, Show)

instance Pretty CExp where
    ppr (CInt x)      = ppr x
    ppr (CDouble x)   = ppr x
    ppr (CExp ce)     = ppr ce
    ppr (CInit cinit) = ppr cinit

instance ToExp CExp where
    toExp (CInt i)    = const [cexp|$int:i|]
    toExp (CDouble x) = const [cexp|$double:x|]
    toExp (CExp ce)   = const ce
    toExp (CInit _)   = error "ToExp CExp: cannot convert CInit to a C expression"

instance ToInitializer CExp where
    toInitializer (CInit cinit) = cinit
    toInitializer ce            = [cinit|$ce|]

instance LiftNum CExp where
    isIntegral x (CInt y)   = y == fromInteger x
    isIntegral x (CDouble y) = y == fromInteger x
    isIntegral _ _           = False

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

instance Num CExp where
    (+) = liftNum2 Add (+)
    (-) = liftNum2 Sub (-)
    (*) = liftNum2 Mul (*)

    abs = liftNum Abs abs

    negate = liftNum Neg negate

    signum  = liftNum Signum signum

    fromInteger = CInt
