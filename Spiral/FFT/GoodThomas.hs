{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.FFT.GoodThomas
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.FFT.GoodThomas (
    CRT,
    Good,
    Perm(..),

    goodThomas
  ) where

import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral.ExtendedFloat
import Spiral.SPL

goodThomas :: forall a . (ExtendedFloat a, Fractional a) => Int -> Int -> a -> SPL a
goodThomas r s w = _Q' × (RDFT r (w^^er) ⊗ RDFT s (w^^es)) × _Q
  where
    (u, v) = euclid r s
    er     = s * v
    es     = r * u

    pi :: Perm CRT
    pi = CRT r s er es

    _Q :: SPL a
    _Q = Pi pi

    _Q' :: SPL a
    _Q' = Pi (invert pi)

data CRT

instance Permutation CRT where
    -- | The CRT permutation.
    data Perm CRT = CRT Int Int Int Int
                  | CRT' Int Int Int Int
      deriving (Eq, Ord, Show)

    toPermute (CRT m n _em _en) = f
      where
        f :: Integral a => a -> a
        f ix = fromIntegral $ n*i + j
          where
            i = fromIntegral ix `rem` m
            j = fromIntegral ix `rem` n

    toPermute (CRT' m n em en) = f
        where
        f :: Integral a => a -> a
        f ix = fromIntegral $ (i*em + j*en) `mod` (m*n)
          where
            i = fromIntegral ix `quot` n
            j = fromIntegral ix `rem` n

    dim (CRT m n _ _)  = m*n
    dim (CRT' m n _ _) = m*n

    invert (CRT m n em en)  = CRT' m n em en
    invert (CRT' m n em en) = CRT m n em en

instance Pretty (Perm CRT) where
    ppr (CRT m n _ _)  = text "CRT_" <> ppr m <> char '_' <> ppr n
    ppr (CRT' m n _ _) = text "CRT'_" <> ppr m <> char '_' <> ppr n

data Good

instance Permutation Good where
    -- | The Good (Ruritanian) map.
    data Perm Good = Good Int Int Int Int
                   | Good' Int Int Int Int
      deriving (Eq, Ord, Show)

    toPermute (Good m n u v) = f
      where
        f :: Integral a => a -> a
        f ix = fromIntegral $ ((ix' * v) `mod` m)*n + ((ix' * u) `mod` n)
          where
            ix' :: Int
            ix' = fromIntegral ix

    toPermute (Good' m n _u _v) = f
      where
        f :: Integral a => a -> a
        f ix = fromIntegral $ (i*n + j*m) `mod` (m*n)
          where
            i = fromIntegral ix `quot` n
            j = fromIntegral ix `rem` n

    dim (Good m n _ _)  = m*n
    dim (Good' m n _ _) = m*n

    invert (Good m n em en)  = Good' m n em en
    invert (Good' m n em en) = Good m n em en

instance Pretty (Perm Good) where
    ppr (Good m n _ _)  = text "Good_" <> ppr m <> char '_' <> ppr n
    ppr (Good' m n _ _) = text "Good'_" <> ppr m <> char '_' <> ppr n

-- | The extended Euclidean algorithm. Find @s@ and @t@ such that @s*a + t*b =
-- gcd a b@
euclid :: forall a . Integral a => a -> a -> (a, a)
euclid a b | a < b = (t,s)
  where
    (s,t) = euclid b a

euclid a b = go a b 1 0 0 1
  where
    go a b sa sb ta tb | b == 0    = (sa, ta)
                       | otherwise = go b r sb (sa - q*sb) tb (ta - q*tb)
      where
        (q, r) = a `quotRem` b
