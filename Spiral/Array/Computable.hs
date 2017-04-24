{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Spiral.Array.Repr.Compute
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Computable (
    ForShape(..),
    Computable(..)
  ) where

import Prelude hiding (read)

import Spiral.Array.Base
import Spiral.Array.Mutable
import Spiral.Array.Shape
import Spiral.Monad
import {-# SOURCE #-} Spiral.Program.Monad

class Shape sh => ForShape sh where
    -- | Allow iterating over all indices in a shape.
    forShapeP :: MonadSpiral m
              => sh
              -> (ExpShapeOf sh -> P m ())
              -> P m ()

instance ForShape Z where
    forShapeP _ k = k Z

instance ForShape sh => ForShape (sh :. Int) where
    forShapeP (sh :. i) k =
      forShapeP sh $ \csh ->
        forP 0 i $ \ci ->
          k (csh :. ci)

class IsArray r sh e => Computable r sh e where
    -- | Compute @a = b@, that is, compute an array's elements in the given
    -- destination.
    computeP :: ( ForShape sh
                , MArray r' sh e
                , MonadSpiral m
                )
             => Array r' sh e -- ^ Destination
             -> Array r  sh e -- ^ Source
             -> P m ()

instance Shape sh => Computable DS sh e where
    computeP a b =
        forShapeP (extent b) $ \ix ->
            write a ix (indexS b ix)
