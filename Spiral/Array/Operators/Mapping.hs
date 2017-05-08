{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Array.Operators.Mapping
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Operators.Mapping (
    mapI,
    mapS,

    zipWithI,
    zipWithS,
    (.+), (.-), (.*)
  ) where

import Prelude hiding ((!!))

import Spiral.Array
import Spiral.Exp

-- | Map a function over the elements of an array, returning a delayed
-- array.
mapI :: forall sh r a b . (Shape sh, IArray r sh (Exp a))
     => (Exp a -> Exp b)
     -> Array r sh (Exp a)
     -> Array D sh (Exp b)
mapI f a = fromFunction (extent a) g
  where
    g :: sh -> Exp b
    g ix = f (index a ix)

-- | Map a function over the elements of an array, returning a symbolic delayed
-- array.
mapS :: forall sh r a b . (Shape sh, SArray r sh (Exp a))
     => (Exp a -> Exp b)
     -> Array r sh (Exp a)
     -> Array DS sh (Exp b)
mapS f a = fromSFunction (extent a) g
  where
    g :: ExpShapeOf sh -> Exp b
    g ix = f (indexS a ix)

-- | Zip a function over the elements of an array, returning a delayed array.
zipWithI :: forall sh r1 r2 a b c .
            ( IArray r1 sh (Exp a)
            , IArray r2 sh (Exp b)
            )
         => (Exp a -> Exp b -> Exp c)
         -> Array r1 sh (Exp a)
         -> Array r2 sh (Exp b)
         -> Array D  sh (Exp c)
zipWithI f a b = fromFunction (intersectDim (extent a) (extent b)) g
  where
    g :: sh -> Exp c
    g ix = f (index a ix) (index b ix)

-- | Zip a function over the elements of an array, returning a symbolic delayed
-- array.
zipWithS :: forall sh r1 r2 a b c .
            ( Shape sh
            , SArray r1 sh (Exp a)
            , SArray r2 sh (Exp b)
            )
         => (Exp a -> Exp b -> Exp c)
         -> Array r1 sh (Exp a)
         -> Array r2 sh (Exp b)
         -> Array DS sh (Exp c)
zipWithS f a b = fromSFunction (intersectDim (extent a) (extent b)) g
  where
    g :: ExpShapeOf sh -> Exp c
    g ix = f (indexS a ix) (indexS b ix)

infixl 6 .+, .-
infixl 7 .*

(.+), (.-), (.*) :: ( Shape sh
                    , Num (Exp a)
                    , SArray r1 sh (Exp a)
                    , SArray r2 sh (Exp a)
                    )
                 => Array r1 sh (Exp a)
                 -> Array r2 sh (Exp a)
                 -> Array DS sh (Exp a)
-- | Point-wise addition of two arrays. Returns a symbolic delayed array.
(.+) = zipWithS (+)

-- | Point-wise subtraction of two arrays. Returns a symbolic delayed array.
(.-) = zipWithS (-)

-- | Point-wise multiplication of two arrays. Returns a symbolic delayed array.
(.*) = zipWithS (*)
