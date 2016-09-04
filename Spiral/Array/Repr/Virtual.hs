{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Array.Repr.Virtual
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Repr.Virtual (
    V,
    Array(..),

    replicateV,
    freezeV
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Spiral.Array
import Spiral.Array.Program
import Spiral.Exp

-- | Type tag for a virtual array.
data V

instance Shape sh => IsArray V sh (Exp a) where
    -- | A matrix whose entries are manifest.
    data Array V sh (Exp a) = V sh (MV.IOVector (Exp a))

    extent (V sh _) = sh

instance (Shape sh, Typed a) => MArray V sh (Exp a) where
    read  (V sh v) cix = do
        ix <- fromExpShape cix
        MV.read v (toIndex sh ix)

    write (V sh v) cix e = do
        ix <- fromExpShape cix
        e' <- cache e
        MV.write v (toIndex sh ix) e'

-- | Replicate the given value according to the specified shape to produce a
-- virtual array.
replicateV :: (Shape sh, MonadP m) => sh -> Exp a -> m (Array V sh (Exp a))
replicateV sh x = V sh <$> MV.replicate (size sh) x

freezeV :: MonadP m => Array V sh (Exp a) -> m (Array M sh (Exp a))
freezeV (V sh mv) = do
    v <- V.freeze mv
    return $ M sh v
