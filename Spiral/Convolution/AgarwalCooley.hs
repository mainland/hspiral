{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Spiral.Convolution.AgarwalCooley (
  agarwalCooleyC,
  agarwalCooleyB,
  agarwalCooleyA
  ) where

import Spiral.NumberTheory (euclid)
import Spiral.RootOfUnity
import Spiral.SPL

import Spiral.Convolution.Core (HPoly(..))
import Spiral.Convolution.Winograd

import Data.List (foldl1')

agarwalCooleyA :: forall a . (RootOfUnity a, Show a, Eq a)
               => Int
               -> Int
               -> SPL a
               -> SPL a
               -> SPL a
agarwalCooleyA r s ar as = (ar ⊗ as) × _Q
  where
    (u, v) = euclid r s
    er     = s * v
    es     = r * u

    pi :: Permutation
    pi = CRT r s er es

    _Q :: SPL a
    _Q = Pi pi

agarwalCooleyB :: forall a . (RootOfUnity a, Show a, Eq a)
               => Int
               -> Int
               -> SPL a
               -> SPL a
               -> SPL a
agarwalCooleyB r s br bs = (br ⊗ bs) × _Q
  where
    (u, v) = euclid r s
    er     = s * v
    es     = r * u

    pi :: Permutation
    pi = CRT r s er es

    _Q :: SPL a
    _Q = Pi pi

agarwalCooleyC :: forall a . (RootOfUnity a, Show a, Eq a)
               => Int
               -> Int
               -> SPL a
               -> SPL a
               -> SPL a
agarwalCooleyC r s cr cs = _Q' × (cr ⊗ cs)
  where
    (u, v) = euclid r s
    er     = s * v
    es     = r * u

    pi :: Permutation
    pi = CRT r s er es

    _Q, _Q' :: SPL a
    _Q  = Pi pi
    _Q' = backpermute pi
