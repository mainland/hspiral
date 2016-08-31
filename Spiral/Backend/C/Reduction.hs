{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Spiral.Backend.C.Reduction
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.Reduction (
    foldP,
    sumP
  ) where

import Prelude hiding ((!!))

import Language.C.Pretty ()

import Spiral.Array
import Spiral.Backend.C.Array
import Spiral.Backend.C.Assign
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Monad
import Spiral.Backend.C.Types
import Spiral.Shape

foldP :: forall a b r .
         ( ToCType a
         , ToCType b
         , CTemp a (CExp a)
         , CAssign (CExp a) (CExp a)
         , CArray r DIM1 b
         )
      => (CExp a -> CExp b -> CExp a)
      -> CExp a
      -> Array r (Z :. Int) (CExp b)
      -> Array CD Z (CExp a)
foldP f z xs =
    fromCFunction Z cidx
  where
    Z :. n = extent xs

    cidx _ =
        shouldUnroll n >>= go
      where
        go True = do
            ces <- mapM (xs !!) [0..n-1]
            return $ foldl f z ces

        go _ = do
            temp <- cgTemp (undefined :: a)
            temp .:=. z
            cgFor 0 n $ \i -> do
              xi <- xs !! i
              temp .:=. f temp xi
            return temp

sumP :: forall a r .
         ( Num (CExp a)
         , ToCType a
         , CTemp a (CExp a)
         , CAssign (CExp a) (CExp a)
         , CArray r DIM1 a
         )
     => Array r (Z :. Int) (CExp a)
     -> Array CD Z (CExp a)
sumP = foldP (+) 0
