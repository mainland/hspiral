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

import Prelude hiding ((!!))

import Language.C.Pretty ()

import Spiral.Backend.C.Array
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Monad
import Spiral.Monad (MonadCg)
import Spiral.SPL

zipWithP :: forall r1 r2 sh a b c .
            ( CArray r1 sh a
            , CArray r2 sh b
            )
         => (CExp a -> CExp b -> CExp c)
         -> Array r1 sh (CExp a)
         -> Array r2 sh (CExp b)
         -> Array CD sh (CExp c)
zipWithP f a b = fromCFunction (intersectDim (extent a) (extent b)) cidx
  where
    cidx :: MonadCg m => CShapeOf sh -> Cg m (CExp c)
    cidx ix = f <$> (cindex a ix >>= cacheCExp) <*> (cindex b ix >>= cacheCExp)

infixl 6 +^, -^
infixl 7 *^

(+^), (-^), (*^) :: (CArray r1 sh a, CArray r2 sh a, Num (CExp a))
                 => Array r1 sh (CExp a)
                 -> Array r2 sh (CExp a)
                 -> Array CD sh (CExp a)
(+^) = zipWithP (+)
(-^) = zipWithP (-)
(*^) = zipWithP (*)
