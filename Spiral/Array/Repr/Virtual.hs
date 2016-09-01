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

-- | Type tag for a virtual array.
data V

instance Shape sh => IsArray V sh e where
    -- | A matrix whose entries are manifest.
    data Array V sh e = V sh (MV.IOVector e)

    extent (V sh _) = sh

instance Shape sh => MArray V sh e where
    read  (V sh v) cix = do
        ix <- fromExpShape cix
        MV.read  v (toIndex sh ix)

    write (V sh v) cix = do
        ix <- fromExpShape cix
        MV.write v (toIndex sh ix)

-- | Replicate the given value according to the specified shape to produce a
-- virtual array.
replicateV :: (Shape sh, MonadP m) => sh -> a -> m (Array V sh a)
replicateV sh x = V sh <$> MV.replicate (size sh) x

freezeV :: MonadP m => Array V sh a -> m (Array M sh a)
freezeV (V sh mv) = do
    v <- V.freeze mv
    return $ M sh v
