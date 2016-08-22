{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Array
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array (
    IsArray(..),
    Array(..),
    Vector,
    Matrix,

    (!),

    M,
    fromLists,
    toLists,
    matrix,

    D,
    fromFunction,
    toFunction,
    delay
  ) where

import qualified Data.Vector as V
import Text.PrettyPrint.Mainland

import Spiral.Shape

-- | A 1-d array, i.e., a vector.
type Vector r e = Array r DIM1 e

-- | A 2-d array, i.e., a matrix.
type Matrix r e = Array r DIM2 e

-- | An array with an extent and a method for accessing values by index.
class IsArray r sh e where
    -- | Arrays with a representation tag, shape, and element type.  Use one of
    -- the type tags like 'M', 'D', etc. for @r@, and one of 'DIM1', DIM2', etc.
    -- for @sh@.
    data Array r sh e

    -- | Take the extent (size) of an array.
    extent :: Shape sh => Array r sh e -> sh

    -- | Shape polymorphic indexing.
    index :: Shape sh => Array r sh e -> sh -> e

    -- | Create a manifest version of an array.
    manifest :: Shape sh => Array r sh e -> Array M sh e
    manifest a = M sh $ V.fromList [a ! fromIndex sh i | i <- [0..size sh-1]]
      where
        sh :: sh
        sh = extent a

-- | Alias for 'index'
(!) :: (Shape sh, IsArray r sh e) => Array r sh e -> sh -> e
(!) = index

-- | Type tag for a matrix whose entries are manifest.
data M

instance IsArray M sh e where
    -- | A matrix whose entries are manifest.
    data Array M sh e = M sh (V.Vector e)
      deriving (Eq, Ord, Show)

    extent (M sh _) = sh

    index (M sh es) i = es V.! toIndex sh i

    manifest a = a

instance Functor (Array M sh) where
    fmap f (M sh es) = M sh (fmap f es)

instance (Num e, Pretty e) => Pretty (Matrix M e) where
      ppr a =
          brackets $ align $
          folddoc (\d1 d2 -> d1 <> comma </> d2) $
          map ppr $
          toLists a

-- | Create a matrix from a list of lists of values.
fromLists :: [[e]] -> Matrix M e
fromLists [] =
    M (ix2 0 0) V.empty

fromLists (r:rs) | all ((== n) . length) rs =
    M (ix2 m n) (V.fromList (concat (r:rs)))
  where
    m = 1 + length rs
    n = length r

fromLists _ = error "matrix: rows have differing lengths"

-- | Alias for fromLists.
matrix :: [[e]] -> Matrix M e
matrix = fromLists

-- | Convert a matrix to a list of lists of elements.
toLists :: (Num e, IsArray r DIM2 e)
        => Matrix r e
        -> [[e]]
toLists e = [[a ! ix2 i j | i <- [0..m-1]] | j <- [0..n-1]]
  where
    a@(M (Z :. m :. n) _) = manifest e

-- | Type tag for a delayed matrix.
data D

instance IsArray D sh e where
    -- | A "delayed" matrix, i.e., functional representation.
    data Array D sh e = D sh (sh -> e)

    extent (D sh _) = sh

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

instance (Num e, Pretty e) => Pretty (Matrix D e) where
    ppr = ppr . manifest

-- | Create a delayed array from a function mapping indices to elements.
fromFunction :: sh -> (sh -> a) -> Array D sh a
fromFunction = D

-- | Produce the extent of an array and a function to retrieve an arbitrary
-- element.
toFunction :: (Shape sh, IsArray D sh e)
           => Array D sh e
           -> (sh, sh -> e)
toFunction a =
    case delay a of
      D sh f -> (sh, f)

-- | Delay an array. This ensures that the internal representation of the array
-- is a function from indices to elements.
delay :: (Shape sh, IsArray r sh e)
      => Array r sh e
      -> Array D sh e
delay a = D (extent a) (index a)
