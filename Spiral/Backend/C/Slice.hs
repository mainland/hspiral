{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Backend.C.Slice
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.Slice (
    S,
    Array(..),
    slice
  ) where

import Text.PrettyPrint.Mainland

import Spiral.Array
import Spiral.Backend.C.Array
import Spiral.Backend.C.CExp
import Spiral.Shape

-- | Type tag for a vector slice.
data S r

-- | A vector slice in @begin:stride:len@ form. @begin@ is symbolic, where as
-- @stride@ and @len@ are statically known. Note that the end of the slice is
-- @begin + len - 1@.
instance IsArray (S r) DIM1 (CExp a) where
    data Array (S r) DIM1 (CExp a) where
        S :: Array r DIM1 (CExp a)
          -> CExp Int
          -> Int
          -> Int
          -> Array (S r) DIM1 (CExp a)

    extent (S _ _b _s len) = Z :. len

instance Pretty (Array (S r) DIM1 (CExp a)) where
    ppr (S _ b s e) = brackets $ colonsep [text "...", ppr b, ppr s, ppr e]
      where
        colonsep :: [Doc] -> Doc
        colonsep = align . sep . punctuate colon

instance CArray r DIM1 e => CArray (S r) DIM1 e where
    cindex (S a b s _len) (Z :. ci) = cindex a (Z :. b + ci * fromIntegral s)

instance MCArray r DIM1 e => MCArray (S r) DIM1 e where
    cwrite (S a b s _len) (Z :. ci) = cwrite a (Z :. b + ci * fromIntegral s)

slice :: Array r DIM1 (CExp a)
      -> CExp Int
      -> Int
      -> Int
      -> Array (S r) DIM1 (CExp a)
slice = S
