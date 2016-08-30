{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Spiral.Backend.C.Types
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.Types (
    ToCType(..)
  ) where

import Data.Complex
import qualified Language.C.Syntax as C
import Language.C.Quote.C

import Spiral.Array
import Spiral.Globals
import Spiral.Shape

-- | Compile a value to a C type.
class ToCType a where
    toCType :: a -> C.Type

instance ToCType Int where
    toCType _ = [cty|int|]

instance ToCType Double where
    toCType _ = [cty|double|]

instance ToCType (Complex Double) where
    toCType _ | useComplexType = [cty|double _Complex|]
              | otherwise      = error "Cannot convert complex double to C type"

mkArrayT :: Shape sh => C.Type -> sh -> C.Type
mkArrayT ctau sh = foldl cidx ctau (listOfShape sh)
  where
    cidx :: C.Type -> Int -> C.Type
    cidx ctau i = [cty|$ty:ctau[static $int:i]|]

instance (ToCType a, IsArray r Z a) => ToCType (Array r Z a) where
    toCType _ = toCType (undefined :: a)

instance (Shape sh, IsArray r (sh :. Int) Int) => ToCType (Array r (sh :. Int) Int) where
    toCType a = mkArrayT (toCType (undefined :: Int)) (extent a)

instance (Shape sh, IsArray r (sh :. Int) Double) => ToCType (Array r (sh :. Int) Double) where
    toCType a = mkArrayT (toCType (undefined :: Double)) (extent a)

instance (Shape sh, IsArray r (sh :. Int) (Complex Double)) => ToCType (Array r (sh :. Int) (Complex Double)) where
    toCType a
        | useComplexType = mkArrayT (toCType (undefined :: Complex Double)) sh
        | otherwise      = mkArrayT (toCType (undefined :: Double)) (sh0 :. 2*n)
      where
        sh@(sh0 :. n) = extent a
