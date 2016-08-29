{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Spiral.Shape

-- | Compile a value to a C type.
class ToCType a where
    toCType :: a -> C.Type

instance ToCType Int where
    toCType _ = [cty|int|]

instance ToCType Double where
    toCType _ = [cty|double|]

instance ToCType (Complex Double) where
    toCType _ = [cty|double _Complex|]

instance (ToCType a, IsArray r DIM1 a) => ToCType (Array r DIM1 a) where
    toCType a = [cty|$ty:(toCType (undefined :: a))[static $int:n]|]
      where
        Z :. n = extent a

instance (ToCType a, IsArray r DIM2 a) => ToCType (Array r DIM2 a) where
    toCType a = [cty|$ty:(toCType (undefined :: a))[static $int:m][static $int:n]|]
      where
        Z :. m :. n = extent a
