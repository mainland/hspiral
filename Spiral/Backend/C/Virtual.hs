{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Backend.C.Virtual
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.Virtual (
    V,
    Array(..),
    newVirtual
  ) where

import qualified Data.Vector.Mutable as MV

import Spiral.Array
import Spiral.Backend.C.Array
import Spiral.Backend.C.Monad
import Spiral.Backend.C.Types
import Spiral.Monad
import Spiral.Shape

-- | Type tag for a virtual array.s
data V

instance IsArray V sh e where
    -- | A matrix whose entries are manifest.
    data Array V sh e = V sh (MV.IOVector e)

    extent (V sh _) = sh

instance (ToCShape sh, ToCType e) => CArray V sh e where
    cindex (V sh v) cix = MV.read v (toIndex sh (fromCShape cix))

instance (ToCShape sh, ToCType e) => MCArray V sh e where
    cwrite (V sh v) cix = MV.write v (toIndex sh (fromCShape cix))

newVirtual :: (Shape sh, MonadCg m) => sh -> a -> Cg m (Array V sh a)
newVirtual sh x = do
    mv <- MV.replicate (size sh) x
    return $ V sh mv
