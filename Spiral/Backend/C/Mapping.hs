{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Backend.C.Mapping
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.Mapping (
    zipWithP,
    (+^), (-^), (*^)
  ) where

import Language.C.Pretty ()

import Spiral.Backend.C.Array
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Monad
import Spiral.Monad (MonadCg)
import Spiral.SPL

zipWithP :: forall r1 r2 sh a b c .
            ( IsArray  r1 sh (CExp a)
            , IsCArray r1 sh a
            , IsArray  r2 sh (CExp b)
            , IsCArray r2 sh b
            )
         => (CExp a -> CExp b -> CExp c)
         -> Array r1 sh (CExp a)
         -> Array r2 sh (CExp b)
         -> Array CD sh (CExp c)
zipWithP f a b = fromCFunctions (intersectDim (extent a) (extent b)) cidx cidxm
  where
    cidx :: CShape sh -> CExp c
    cidx ix = f (cindex a ix) (cindex b ix)

    cidxm :: MonadCg m => CShape sh -> Cg m (CExp c)
    cidxm ix = f <$> cindexM a ix <*> cindexM b ix

infixl 6 +^, -^
infixl 7 *^

(+^), (-^), (*^) :: (IsCArray r1 sh a, IsCArray r2 sh a, Num (CExp a))
                 => Array r1 sh (CExp a)
                 -> Array r2 sh (CExp a)
                 -> Array CD sh (CExp a)
(+^) = zipWithP (+)
(-^) = zipWithP (-)
(*^) = zipWithP (*)
