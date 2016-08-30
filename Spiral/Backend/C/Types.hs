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

mkArrayT :: Shape sh => C.Type -> sh -> C.Type
mkArrayT ctau sh = foldl cidx ctau (listOfShape sh)
  where
    cidx :: C.Type -> Int -> C.Type
    cidx ctau i = [cty|$ty:ctau[static $int:i]|]

instance (ToCType a, Shape sh, IsArray r sh a) => ToCType (Array r sh a) where
    toCType a = mkArrayT (toCType (undefined :: a)) (extent a)
