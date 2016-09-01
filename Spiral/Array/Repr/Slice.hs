{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Array.Repr.Slice
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Repr.Slice (
    S,
    Array(..),

    slice
  ) where

import Prelude hiding (read)

import Text.PrettyPrint.Mainland

import Spiral.Array
import Spiral.Array.Program
import Spiral.Exp

-- | Type tag for a vector slice.
data S r

-- | A vector slice in @begin:stride:len@ form. @begin@ is symbolic, whereas
-- @stride@ and @len@ are statically known. Note that the last element of the
-- slice is element @begin + stride * len - 1@ of the original array.
instance IsArray (S r) DIM1 a where
    data Array (S r) DIM1 a where
        S :: Array r DIM1 a
          -> Exp Int
          -> Int
          -> Int
          -> Array (S r) DIM1 a

    extent (S _ _b _s len) = Z :. len

instance Pretty (Array (S r) DIM1 (Exp a)) where
    ppr (S _ b s len) =
        brackets $ colonsep [text "...", ppr b, ppr s, ppr len]
      where
        colonsep :: [Doc] -> Doc
        colonsep = align . sep . punctuate colon

instance SArray r DIM1 a => SArray (S r) DIM1 a where
    indexS (S a b s _len) (Z :. ci) = indexS a (Z :. b + ci * fromIntegral s)

instance MArray r DIM1 e => MArray (S r) DIM1 e where
    read  (S a b s _len) (Z :. ci) = read  a (Z :. b + ci * fromIntegral s)
    write (S a b s _len) (Z :. ci) = write a (Z :. b + ci * fromIntegral s)

-- | Take a slice of an array.
slice :: Array r DIM1 (Exp a)
      -> Exp Int
      -> Int
      -> Int
      -> Array (S r) DIM1 (Exp a)
slice = S
