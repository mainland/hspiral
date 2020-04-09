{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Spiral.Convolution.Core (
    bilinear,
    modMatrix,

    HPoly(..),
    Polynomial,
    PolyField(),

    Bilinear(..),
    Convolution(..),
  ) where

import qualified Spiral.Array as A
import Spiral.SPL

import qualified Data.Vector as V
import Data.Poly hiding (Poly)
import qualified Data.Euclidean as Euclid
import qualified Data.Semiring as Semi

-- | Abstraction over polynomials for HSpiral
class HPoly p where
  -- | Degree of the polynomial
  degree :: (PolyField a) => p a -> Int

  -- | Generates the remainder polynomial of dividing the first by the second
  remainder :: (PolyField a) => p a -> p a -> p a
  quotient :: (PolyField a) => p a -> p a -> p a

  -- | Multiplies two polynomials
  mult :: (PolyField a) => p a -> p a -> p a

  -- | Extract a list of coefficients
  coeffs  :: (PolyField a, Fractional b) => p a -> [b] -- (little endian)
  coeffs' :: (PolyField a, Fractional b) => p a -> [b] -- (big endian)

  -- | Given a list of coefficients, construct a polynomial
  construct  :: (PolyField a, Real b) => [b] -> p a  -- (little endian)
  construct' :: (PolyField a, Real b) => [b] -> p a -- (big endian)

  -- Given a power and a coefficient, constructs a polynomial
  mono :: (PolyField a, Real b) => Int -> b -> p a

instance HPoly VPoly where
  degree p = (Semi.fromNatural $ Euclid.degree p) - 1
  remainder a b = snd $ Euclid.quotRem a b
  quotient a b = fst $ Euclid.quotRem a b
  mult = (*)

  coeffs  = (map toFrac) . V.toList . unPoly
  coeffs' = reverse . coeffs

  construct  = toPoly . V.fromList . (map fromFrac)
  construct' = construct . reverse

  mono p a = monomial (fromIntegral p) (fromFrac a)

class (Eq a, Fractional a, Semi.Ring a, Euclid.Euclidean a, Euclid.GcdDomain a) => PolyField a where
  toFrac :: (Fractional b) => a -> b
  fromFrac :: (Real b) => b -> a

instance PolyField Rational where
  toFrac = fromRational
  fromFrac = toRational

instance PolyField Float where
  toFrac = realToFrac
  fromFrac = realToFrac

instance PolyField Double where
  toFrac = realToFrac
  fromFrac = realToFrac

type Polynomial a = VPoly a

-- | Given a polynomial and a degree, creates a reduction matrix from
-- | polynomials of the given degree to the quotient ring mod p
modMatrix :: forall a b . (Fractional a, Eq a, Show a, PolyField b) => Polynomial b -> Int -> SPL a
modMatrix _ 0   = I 1
modMatrix p deg = transpose $ fromLists convert_and_extend
  where
    deg' = degree p

    convert_and_extend :: [[a]]
    convert_and_extend = let cs = map coeffs $! map ((flip remainder) p) [(mono el (1 :: Rational)) | el <- [0..deg]]
                          in [c ++ replicate (deg' - length c) 0 | c <- cs]

class Bilinear b where
  getA :: (Show a, Num a, Eq a) => b a -> SPL a
  getB :: (Show a, Num a, Eq a) => b a -> SPL a
  getC :: (Show a, Num a, Eq a) => b a -> SPL a

class (Bilinear c) => Convolution c where
  toCyclicA :: (Show a, Num a, Eq a) => c a -> SPL a
  toCyclicB :: (Show a, Fractional a, Eq a, PolyField b) => Polynomial b -> c a -> SPL a
  toCyclicC :: (Show a, Num a, Eq a) => c a -> SPL a

  toLinearA :: (Show a, Num a, Eq a) => c a -> SPL a
  toLinearB :: (Show a, Num a, Eq a) => c a -> SPL a
  toLinearC :: (Show a, Num a, Eq a) => c a -> SPL a

  getSize :: c a -> Int

  convolve :: (Show a, Num a, Eq a) => c a -> [a] -> SPL a
  convolve conv xs = bilinear xs (getC conv) (getB conv) (getA conv)

-- | Basic bilinear calculation
bilinear :: forall a . Num a => [a] -> SPL a -> SPL a -> SPL a -> SPL a
bilinear g c b a = c × d × a
  where
    d = diag $ A.toList $ b #> A.fromList g
