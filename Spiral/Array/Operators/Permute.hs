{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  Spiral.Array.Operators.Permute
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Operators.Permute (
    Array(..),

    Permutation(..),

    Permute(..),

    BP,
    idBackpermute
  ) where

import Prelude hiding (read)

import Spiral.Array
import Spiral.Exp
import Spiral.Monad
import Spiral.Permutation
import Spiral.Program.Monad

-- | Array representations that can be permuted.
class Permute r where
    -- | Permute a vector, AKA scatter.
    permuteP :: MonadSpiral m => Permutation -> Vector r b -> P m (Vector r b)

    -- | Backpermute a vector, AKA gather.
    backpermuteP :: MonadSpiral m => Permutation -> Vector r b -> P m (Vector r b)

-- | Cast an index mapping function to a reindexing function.
toReindex :: (a -> a)
          -> Z :. a
          -> Z :. a
toReindex f (Z :. i) = Z :. f i

instance Permute D where
    permuteP p (D sh f) = return $ D sh (f . g)
      where
        g = toReindex (toIdxMapping (invert p))

    backpermuteP p (D sh f) = return $ D sh (f . g)
      where
        g = toReindex (toIdxMapping p)

instance Permute DS where
    permuteP p (DS sh f) = do
        g <- toReindex <$> toSIdxMapping (invert p)
        return $ DS sh (f . g)

    backpermuteP p (DS sh f) = do
        g <- toReindex <$> toSIdxMapping p
        return $ DS sh (f . g)

data BP r

-- | A backpermuted vector.
instance IsArray r DIM1 a => IsArray (BP r) DIM1 a where
    data Array (BP r) DIM1 a = BP (Int -> Int) (Exp Int -> Exp Int) (Vector r a)

    extent (BP _ _ a) = extent a

instance IArray r DIM1 a => IArray (BP r) DIM1 a where
    index (BP f _g a) (Z :. i) = index a (Z :. f i)

instance SArray r DIM1 a => SArray (BP r) DIM1 a where
    indexS (BP f  _g a) (Z :. ConstE (IntC i)) = indexS a (Z :. intE (f i))
    indexS (BP _f g  a) (Z :. i)               = indexS a (Z :. g i)

instance MArray r DIM1 e => MArray (BP r) DIM1 e where
    read  (BP f  _g a) (Z :. ConstE (IntC i)) = read  a (Z :. intE (f i))
    read  (BP _f g  a) (Z :. i)               = read  a (Z :. g i)

    write (BP f  _g a) (Z :. ConstE (IntC i)) = write a (Z :. intE (f i))
    write (BP _f g  a) (Z :. i)               = write a (Z :. g i)

-- XXX Do we want to be able to compute a 'BP r'? It is usually better to
-- permute the destination instead of backpermuting the source, so I've
-- commented out the Compute instance.

{-
instance SArray r DIM1 a => Compute (BP r) DIM1 a where
    computeP a b =
        forShapeP (extent b) $ \ix ->
            write a ix (indexS b ix)
-}

instance Permute (BP r) where
    permuteP p (BP f g a) = do
        g' <- toSIdxMapping (invert p)
        return $ BP (f . f') (g . g') a
      where
        f' = toIdxMapping (invert p)

    backpermuteP p (BP f g a) = do
        g' <- toSIdxMapping p
        return $ BP (f . f') (g . g') a
      where
        f' = toIdxMapping p

-- | Create a backpermuted vector where the permutation is the identity
-- permutation.
idBackpermute :: Vector r a -> Vector (BP r) a
idBackpermute = BP id id
