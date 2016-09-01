{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Spiral.Shape
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Shape (
    Shape(..),

    Z(..),
    (:.)(..),

    DIM0,
    DIM1,
    DIM2,
    ix1,
    ix2
  ) where

import Spiral.Exp

-- | Array shapes
class Eq sh => Shape sh where
    -- | The corresponding expression shape.
    type ExpShapeOf sh

    -- | Get the number of dimensions in a shape.
    rank :: sh -> Int

    -- | The shape of an array of size zero, with a particular dimensionality.
    zeroDim :: sh

    -- | The shape of an array with size one, with a particular dimensionality.
    unitDim :: sh

    -- | Compute the intersection of two shapes.
    intersectDim :: sh -> sh -> sh

    -- | Add the coordinates of two shapes componentwise
    addDim :: sh -> sh -> sh

    -- | Get the total number of elements in an array with this shape.
    size :: sh -> Int

    -- | Convert a shape into its list of dimensions.
    listOfShape :: sh -> [Int]

    -- | Convert a list of dimensions to a shape
    shapeOfList :: [Int] -> sh

    -- | Convert an index into its equivalent flat, linear, row-major version.
    toIndex :: sh  -- ^ Shape of the array.
            -> sh  -- ^ Index into the array.
            -> Int

    -- | Inverse of `toIndex`.
    fromIndex :: sh  -- ^ Shape of the array.
              -> Int -- ^ Index into linear representation.
              -> sh

    -- | Convert a shape to its corresponding expression shape.
    toExpShape :: sh -> ExpShapeOf sh

    -- | Convert an expression shape to its corresponding shape.
    fromExpShape :: Monad m => ExpShapeOf sh -> m sh

-- | An index of dimension zero
data Z = Z
  deriving (Show, Read, Eq, Ord)

instance Shape Z where
    type ExpShapeOf Z = Z

    rank _ = 0

    zeroDim = Z

    unitDim = Z

    intersectDim _ _ = Z

    addDim _ _ = Z

    size _ = 1

    listOfShape _ = []

    shapeOfList [] = Z
    shapeOfList _  = error "shapeOfList: non-empty list when converting to Z"

    toIndex _ _ = 0

    fromIndex _ _ = Z

    toExpShape sh = sh

    fromExpShape = return

-- | Our index type, used for both shapes and indices.
infixl 3 :.

data (:.) tail head = tail :. head
  deriving (Eq, Ord, Show)

instance Shape sh => Shape (sh :. Int) where
    type ExpShapeOf (sh :. Int) = ExpShapeOf sh :. Exp Int

    rank _ = rank (undefined :: sh) + 1

    zeroDim = zeroDim :. 0

    unitDim = unitDim :. 1

    intersectDim (sh1 :. n1) (sh2 :. n2) =
        intersectDim sh1 sh2 :. min n1 n2

    addDim (sh1 :. n1) (sh2 :. n2) =
        addDim sh1 sh2 :. (n1 + n2)

    size (sh1 :. n) =
        size sh1 * n

    listOfShape (sh :. n) =
        n : listOfShape sh

    shapeOfList []     = error "shapeOfList: empty list when converting to  (_ :. Int)"
    shapeOfList (x:xs) = shapeOfList xs :. x

    toIndex (sh1 :. sh2) (sh1' :. sh2') =
        toIndex sh1 sh1' * sh2 + sh2'

    fromIndex (ds :. d) n =
        fromIndex ds q :. r
      where
        (q, r) = n `quotRem` d

    toExpShape (sh :. i) = toExpShape sh :. intE (fromIntegral i)

    fromExpShape (sh :. ConstE (IntC i)) =
        (:.) <$> fromExpShape sh <*> pure (fromIntegral i)

    fromExpShape _ =
        fail "fromExpShape: non-literal shape"

-- | 0-d (scalar) shape.
type DIM0 = Z

-- | 1-d integer shape.
type DIM1 = DIM0 :. Int

-- | 2-d integer shape.
type DIM2 = DIM1 :. Int

-- | Construct 1-d integer shape.
ix1 :: Int -> DIM1
ix1 x = Z :. x

-- | Construct 2-d integer shape.
ix2 :: Int -> Int -> DIM2
ix2 y x = Z :. y :. x
