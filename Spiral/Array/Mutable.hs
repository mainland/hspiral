{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      :  Spiral.Array.Mutable
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Mutable (
    MArray(..)
  ) where

import Prelude hiding ((!!), read)

import Spiral.Array.Base
import Spiral.Array.Shape
import Spiral.Monad
import {-# SOURCE #-} Spiral.Program.Monad

-- | A mutable array.
class IsArray r sh e => MArray r sh e where
    -- | Read an element of an array.
    read  :: MonadSpiral m => Array r sh e -> ExpShapeOf sh -> P m e

    -- | Write an element of an array.
    write :: MonadSpiral m => Array r sh e -> ExpShapeOf sh -> e -> P m ()
