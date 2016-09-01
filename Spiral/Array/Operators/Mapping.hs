{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Array.Operators.Mapping
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Operators.Mapping (
    mapS,

    zipWithI,
    zipWithS,
    (.+), (.-), (.*)
  ) where

import Prelude hiding ((!!))

import Spiral.Array
import Spiral.Exp

mapS :: (Shape sh, SArray r sh (Exp a))
     => (Exp a -> Exp b)
     -> Array r sh (Exp a)
     -> Array DS sh (Exp b)
mapS f a = fromSFunction (extent a) g
  where
    g ix = f (indexS a ix)

zipWithI :: ( Shape sh
            , IArray r1 sh (Exp a)
            , IArray r2 sh (Exp b)
            )
         => (Exp a -> Exp b -> Exp c)
         -> Array r1 sh (Exp a)
         -> Array r2 sh (Exp b)
         -> Array D  sh (Exp c)
zipWithI f a b = fromFunction (intersectDim (extent a) (extent b)) g
  where
    g ix = f (index a ix) (index b ix)

zipWithS :: ( Shape sh
            , SArray r1 sh (Exp a)
            , SArray r2 sh (Exp b)
            )
         => (Exp a -> Exp b -> Exp c)
         -> Array r1 sh (Exp a)
         -> Array r2 sh (Exp b)
         -> Array DS sh (Exp c)
zipWithS f a b = fromSFunction (intersectDim (extent a) (extent b)) g
  where
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
(.+) = zipWithS (+)
(.-) = zipWithS (-)
(.*) = zipWithS (*)
