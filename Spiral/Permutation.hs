{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Permutation
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Permutation (
    Permutation(..),
    toIdxMapping,
    toSIdxMapping,
    dim,
    invert
  ) where

import Prelude hiding (read)

import qualified Data.Vector as V
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral.Array
import Spiral.Exp
import Spiral.Monad
import Spiral.NumberTheory (modExp)
import Spiral.Program

data Permutation -- | The $L^{mn}_n$ $mn \times mn$ stride permutation with
                 -- stride $n$.
                 = L Int Int
                 -- | The reverse identity permutation.
                 | J Int
                 -- | The CRT permutation. Components are m, n, e_m, and e_n
                 | CRT Int Int Int Int
                 -- | The inverse CRT permutation.
                 | CRT' Int Int Int Int
                 -- | The Good (Ruritanian) map.
                 | Good Int Int Int Int
                 -- | The inverse Good map.
                 | Good' Int Int Int Int
                 -- | Rader mapping. Components are a prime p and a root of
                 -- unity a.
                 | R Int Int
                 -- | A concrete mapping paired with its inverse. Only used when
                 -- we can't efficiently compute the inverse of a permutation.
                 | Inv Permutation (V.Vector Int)
      deriving (Eq, Ord, Show)

instance Pretty Permutation where
    ppr (L mn n)        = text "L^" <> ppr mn <> char '_' <> ppr n
    ppr (J n)           = text "J_" <> ppr n
    ppr (CRT m n _ _)   = text "CRT_" <> ppr m <> char '_' <> ppr n
    ppr (CRT' m n _ _)  = text "CRT'_" <> ppr m <> char '_' <> ppr n
    ppr (Good m n _ _)  = text "Good_" <> ppr m <> char '_' <> ppr n
    ppr (Good' m n _ _) = text "Good'_" <> ppr m <> char '_' <> ppr n
    ppr (R p a)         = text "R_"  <> ppr p <> char '_' <> ppr a
    ppr (Inv p _)       = text "inv" <> parens (ppr p)

-- | Convert a permutation to an explicit index mapping.
toIdxMapping :: Permutation -> Int -> Int
toIdxMapping = toIdxMapping'

-- | Return a symbolic explicit index mapping.
toSIdxMapping :: forall m . MonadSpiral m
              => Permutation
              -> P m (Exp Int -> Exp Int)
toSIdxMapping = go
  where
    go :: Permutation -> P m (Exp Int -> Exp Int)
    go p@R{}   = mkSIdxMapping p
    go p@Inv{} = mkSIdxMapping p
    go p       = return $ toIdxMapping' p

    mkSIdxMapping :: Permutation -> P m (Exp Int -> Exp Int)
    mkSIdxMapping p = do
        table <- cacheArray $ fromList [intE (f i) | i <- [0..dim p-1]]
        return $ \i -> indexS table (Z :. i)
      where
        f :: Int -> Int
        f = toIdxMapping p

-- | Convert a permutation to an explicit index mapping. This function is safe
-- to use on any 'Integral' type where equality is real, i.e., it may not work
-- for staged types like 'Spiral.Exp'.
toIdxMapping' :: forall a. Integral a => Permutation -> a -> a
toIdxMapping' (L mn n0) = f
  where
    -- See [Voronenko08] p. 24
    f :: a -> a
    f i = i `quot` n + m * (i `rem` n)
      where
        m, n :: a
        m = fromIntegral (mn `quot` n0)
        n = fromIntegral n0

toIdxMapping' (J n0) = f
  where
    f :: a -> a
    f i = n - i
      where
        n :: a
        n = fromIntegral n0

toIdxMapping' (CRT m0 n0 _em _en) = f
  where
    f :: a -> a
    f ix = n*i + j
      where
        i, j :: a
        i = ix `rem` m
        j = ix `rem` n

        m, n :: a
        m = fromIntegral m0
        n = fromIntegral n0

toIdxMapping' (CRT' m0 n0 em0 en0) = f
    where
    f :: a -> a
    f ix = (i*em + j*en) `mod` (m*n)
      where
        i, j :: a
        i = ix `quot` n
        j = ix `rem` n

        m, n, em, en :: a
        m  = fromIntegral m0
        n  = fromIntegral n0
        em = fromIntegral em0
        en = fromIntegral en0

toIdxMapping' (Good m0 n0 u0 v0) = f
  where
    f :: Integral a => a -> a
    f ix = ((ix * v) `mod` m)*n + ((ix * u) `mod` n)
      where
        m, n, u, v :: a
        m = fromIntegral m0
        n = fromIntegral n0
        u = fromIntegral u0
        v = fromIntegral v0

toIdxMapping' (Good' m0 n0 _u _v) = f
  where
    f :: Integral a => a -> a
    f ix = (i*n + j*m) `mod` (m*n)
      where
        i, j :: a
        i = ix `quot` n
        j = ix `rem` n

        m, n :: a
        m = fromIntegral m0
        n = fromIntegral n0

toIdxMapping' (R p a) = f
  where
    f :: a -> a
    f 0 = 0
    f i = fromIntegral $ modExp a (i-1) p

toIdxMapping' (Inv _ v) = f
  where
    f :: a -> a
    f i = fromIntegral $ v V.! fromIntegral i

-- | Return the dimension of a permutation.
dim :: Permutation -> Int
dim (L mn _n)       = mn
dim (J n)           = n
dim (CRT m n _ _)   = m*n
dim (CRT' m n _ _)  = m*n
dim (Good m n _ _)  = m*n
dim (Good' m n _ _) = m*n
dim (R p _)         = p
dim (Inv p _)       = dim p

-- | Invert a permutation.
invert :: Permutation -> Permutation
invert (L mn n) = L mn m
  where
    m = mn `quot` n

invert p@J{} = p

invert (CRT m n em en) =
    CRT' m n em en

invert (CRT' m n em en) =
    CRT m n em en

invert (Good m n em en) =
    Good' m n em en

invert (Good' m n em en) =
    Good m n em en

invert (Inv p _) =
    p

invert p = Inv p v
  where
    f :: Int -> Int
    f = toIdxMapping p

    v :: V.Vector Int
    v = V.replicate (dim p) 0 V.// [(f i, i) | i <- [0..dim p-1]]
