{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Spiral.Array.Base
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Base (
    IsArray(..),
    Array(..),
    Vector,
    Matrix,

    IArray(..),
    Index(..),

    SArray(..),
    SIndex(..),

    identity,

    M,
    fromList,
    toList,
    fromLists,
    toLists,
    matrix,
    fromVector,
    toVector,
    (++),
    concat,
    slice,

    D,
    fromFunction,
    toFunction,
    delay,

    DS,
    fromSFunction,
    toSFunction,
    delayS,

    coerceSymbolic
  ) where

import Prelude hiding ((++), concat)
import qualified Prelude

import qualified Data.Vector as V
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral.Array.Shape
import Spiral.Exp

-- | A 1-d array, i.e., a vector.
type Vector r e = Array r DIM1 e

-- | A 2-d array, i.e., a matrix.
type Matrix r e = Array r DIM2 e

-- | An array with an extent.
class IsArray r sh e where
    -- | Arrays with a representation tag, shape, and element type.  Use one of
    -- the type tags like 'M', 'D', etc. for @r@, and one of 'DIM1', DIM2', etc.
    -- for @sh@.
    data Array r sh e

    -- | Take the extent (size) of an array.
    extent :: Shape sh => Array r sh e -> sh

-- | An array that can be indexed.
class (Shape sh, IsArray r sh e) => IArray r sh e where
    -- | Shape polymorphic indexing.
    index :: Array r sh e -> sh -> e

    -- | Create a manifest version of an indexed array.
    manifest :: Array r sh e -> Array M sh e
    manifest a = M sh $ V.generate (size sh) (\i -> a ! fromIndex sh i)
      where
        sh :: sh
        sh = extent a

-- | An array index.
class Index r sh ix e where
    (!) :: Array r sh e -> ix -> e

instance IArray r sh e => Index r sh sh e where
    a ! i = index a i

instance IArray r DIM1 e => Index r DIM1 Int e where
    a ! i = a ! (Z :. i)

instance IArray r DIM2 e => Index r DIM2 (Int, Int) e where
    a ! (i, j) = a ! (Z :. i :. j)

-- | An array that can be indexed symbolically.
class IsArray r sh e => SArray r sh e where
    -- | Shape polymorphic indexing.
    indexS :: Array r sh e -> ExpShapeOf sh -> e

-- | A symbolic array index.
class SIndex r sh ix e where
    (!!) :: Array r sh e -> ix -> e

instance SArray r DIM1 e => SIndex r DIM1 Int e where
    a !! i = indexS a (Z :. intE i)

instance SArray r DIM2 e => SIndex r DIM2 (Int, Int) e where
    a !! (i, j) = indexS a (Z :. intE i :. intE j)

instance SArray r DIM1 e => SIndex r DIM1 (Exp Int) e where
    a !! i = indexS a (Z :. i)

instance SArray r DIM2 e => SIndex r DIM2 (Exp Int, Exp Int) e where
    a !! (i, j) = indexS a (Z :. i :. j)


-- | The n x n identity matrix
identity :: Num a => Int -> Matrix D a
identity n = fromFunction (ix2 n n) f
  where
    f (Z :. i :. j)
      | i == j    = 1
      | otherwise = 0

-- | Type tag for a matrix whose entries are manifest.
data M

instance IsArray M sh e where
    -- | A matrix whose entries are manifest.
    data Array M sh e = M sh (V.Vector e)
      deriving (Eq, Ord, Show)

    extent (M sh _) = sh

instance Shape sh => IArray M sh e where
    index (M sh es) i = es V.! toIndex sh i

    manifest a = a

instance Functor (Array M sh) where
    fmap f (M sh es) = M sh (fmap f es)

instance (Shape sh, Pretty e) => Pretty (Array M sh e) where
    ppr arr = first (reverse (listOfShape (extent arr))) []
        where
          first :: [Int] -> [Int] -> Doc
          first []     ix  = ppr (index arr (shapeOfList ix))
          first (n:ix) ix' = brackets $ align $ stack $ punctuate comma $ map (\i -> go ix (i : ix')) [0..n-1]

          go :: [Int] -> [Int] -> Doc
          go []     ix  = ppr (index arr (shapeOfList ix))
          go (n:ix) ix' = brackets $ align $ sep $ punctuate comma $ map (\i -> go ix (i : ix')) [0..n-1]

-- | Create a vector from a list of lists of values.
fromList :: [e] -> Vector M e
fromList xs = M (ix1 (length xs)) (V.fromList xs)

-- | Convert a vector to a list of elements.
toList :: IArray r DIM1 e
       => Vector r e
       -> [e]
toList e = V.toList v
  where
    M _ v = manifest e

-- | Create a matrix from a list of lists of values.
fromLists :: [[e]] -> Matrix M e
fromLists [] =
    M (ix2 0 0) V.empty

fromLists (r:rs) | all ((== n) . length) rs =
    M (ix2 m n) (V.fromList (Prelude.concat (r:rs)))
  where
    m = 1 + length rs
    n = length r

fromLists _ = error "matrix: rows have differing lengths"

-- | Alias for fromLists.
matrix :: [[e]] -> Matrix M e
matrix = fromLists

-- | Create a vector from a V.Vector
fromVector :: V.Vector e -> Vector M e
fromVector v = M (ix1 (V.length v)) v

-- | Convert a Vector to a V.Vector
toVector :: IArray r DIM1 a
         => Vector r a
         -> V.Vector a
toVector x = v
  where
    M _ v = manifest x

-- | Append two Vectors
infixr 5 ++
(++) :: ( IArray r1 DIM1 a
        , IArray r2 DIM1 a
        )
     => Vector r1 a
     -> Vector r2 a
     -> Vector M a
x ++ y = M (ix1 (V.length v1 + V.length v2)) (v1 V.++ v2)
  where
    v1 = toVector x
    v2 = toVector y

-- | Concatenate Vectors
concat :: IArray r DIM1 a
      => [Vector r a]
      -> Vector M a
concat vs = M (ix1 (sum (map V.length vs'))) (V.concat vs')
  where
    vs' = map toVector vs

-- | Slice a Vector
slice :: IArray r DIM1 a
      => Int -- ^ Initial element
      -> Int -- ^ Stride
      -> Int -- ^ Length
      -> Vector r a
      -> Vector D a
slice b s l v = fromFunction (ix1 l) f
  where
    f (Z :. i) = v ! (b + i * s)

-- | Convert a matrix to a list of rows, where each row is a list of elements.
toLists :: IArray r DIM2 e
        => Matrix r e
        -> [[e]]
toLists e = [[a ! ix2 i j | j <- [0..n-1]] | i <- [0..m-1]]
  where
    a@(M (Z :. m :. n) _) = manifest e

-- | Type tag for a delayed matrix.
data D

instance IsArray D sh e where
    -- | A "delayed" matrix, i.e., functional representation.
    data Array D sh e = D sh (sh -> e)

    extent (D sh _) = sh

instance Shape sh => IArray D sh e where
    index (D _sh f) = f

instance Functor (Array D sh) where
    fmap f (D sh g) = D sh (f . g)

instance (Shape sh, Eq e) => Eq (Array D sh e) where
    a == b = manifest a == manifest b
    a /= b = manifest a /= manifest b

instance (Shape sh, Ord sh, Ord e) => Ord (Array D sh e) where
    compare a b = compare (manifest a) (manifest b)

instance (Shape sh, Show sh, Show e) => Show (Array D sh e) where
    show a = show (manifest a)

instance Pretty e => Pretty (Matrix D e) where
    ppr = ppr . manifest

-- | Create a delayed array from a function mapping indices to elements.
fromFunction :: sh -> (sh -> a) -> Array D sh a
fromFunction = D

-- | Produce the extent of an array and a function to retrieve an arbitrary
-- element.
toFunction :: Shape sh
           => Array D sh e
           -> (sh, sh -> e)
toFunction a =
    case delay a of
      D sh f -> (sh, f)

-- | Delay an array. This ensures that the internal representation of the array
-- is a function from indices to elements.
delay :: IArray r sh e
      => Array r sh e
      -> Array D sh e
delay a = D (extent a) (index a)

-- | Type tag for a delayed symbolic arrays..
data DS

instance IsArray DS sh e where
    -- | A "delayed" matrix, i.e., functional representation.
    data Array DS sh e = DS sh (ExpShapeOf sh -> e)

    extent (DS sh _) = sh

instance SArray DS sh e where
    indexS (DS _sh f) = f

instance Shape sh => IArray DS sh e where
    index a = indexS a . toExpShape

-- | Create a delayed symbolic array from a function mapping expression indices
-- to elements.
fromSFunction :: sh -> (ExpShapeOf sh -> a) -> Array DS sh a
fromSFunction = DS

-- | Produce the extent of an array and a function to retrieve an arbitrary
-- element given a symbolic index.
toSFunction :: Shape sh
            => Array DS sh e
            -> (sh, ExpShapeOf sh -> e)
toSFunction a =
    case delayS a of
      DS sh f -> (sh, f)

-- | Delay an array symbolically. This ensures that the internal representation
-- of the array is a function from symbolic indices to elements.
delayS :: (Shape sh, SArray r sh e)
       => Array r sh e
       -> Array DS sh e
delayS a = DS (extent a) (indexS a)

-- | Coerce an 'IArray' to an array that can be index symbolically. This is only
-- safe if we know that the array will only be indexed by constant
-- expressions.
coerceSymbolic :: forall r sh a .
                  ( Shape sh
                  , IArray r sh a
                  )
               => Array r sh a
               -> Array DS sh a
coerceSymbolic x = fromSFunction (extent x) f
  where
    f :: ExpShapeOf sh -> a
    f eix = index x (shapeOfList (map unExp (listOfExpShape eix)))

    unExp :: Exp Int -> Int
    unExp (ConstE (IntC i)) = i
    unExp e                 = errordoc $ text "coerceSymbolic: non-constant:" <+> ppr e
