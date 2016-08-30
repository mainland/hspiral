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

import Spiral.Backend.C.Array
import Spiral.Backend.C.Assign
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Monad
import Spiral.Backend.C.Types
import Spiral.Config
import Spiral.SPL

foldP :: forall a b r .
         ( IsArray r (Z :. Int) (CExp b)
         , Index  r (Z :. Int) (CExp Int) (CExp b)
         , CIndex r (Z :. Int) Int b
         , ToCType a
         , ToCType b
         , CAssign (CExp a))
      => (CExp a -> CExp b -> CExp a)
      -> CExp a
      -> Array r (Z :. Int) (CExp b)
      -> Array CD Z (CExp a)
foldP f z xs =
    fromCFunctions Z cidx cidxm
  where
    Z :. n = extent xs

    cidx _ = foldl f z (map ((xs !) . CInt) [0..n-1])

    cidxm _ =
        asksConfig maxUnroll >>= go
      where
        go maxun | n <= maxun = do
            ces <- mapM (cacheCExp . (xs !) . CInt) [0..n-1]
            return $ foldl f z ces

        go _ = do
            temp <- cgTemp (undefined :: a)
            temp .:=. z
            cgFor 0 n $ \i -> do
              xi <- lookupCExp $ xs ! i
              temp .:=. f temp xi
            return temp

sumP :: forall a r .
         ( IsArray r (Z :. Int) (CExp a)
         , Index  r (Z :. Int) (CExp Int) (CExp a)
         , CIndex r (Z :. Int) Int a
         , Num (CExp a)
         , ToCType a
         , CAssign (CExp a))
     => Array r (Z :. Int) (CExp a)
     -> Array CD Z (CExp a)
sumP = foldP (+) 0
