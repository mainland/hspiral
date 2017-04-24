{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Monad
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Monad (
    MonadSpiral
  ) where

import Control.Monad.Primitive (PrimMonad(..),
                                RealWorld)
import Control.Monad.Ref (MonadRef(..))
import Data.IORef (IORef)

import Spiral.Config
import Spiral.Util.Trace
import Spiral.Util.Uniq

class ( PrimMonad m
      , PrimState m ~ RealWorld
      , MonadRef IORef m
      , MonadConfig m
      , MonadUnique m
      , MonadTrace m
      ) => MonadSpiral m where
