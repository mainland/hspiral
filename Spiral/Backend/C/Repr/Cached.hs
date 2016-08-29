{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Spiral.Backend.C.Repr.Cached
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.Repr.Cached (
    CC,
    Array(..)
  ) where

import Data.Maybe (fromMaybe)
import Language.C.Quote.C

import Spiral.Array
import Spiral.Backend.C.Array
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Types
import Spiral.Shape

-- | Type tag for a cached compiled array.
data CC

instance IsArray CC sh (CExp e) where
    -- | A /cached/ compiled array. This is an array whose contents has been
    -- computed so it can be indexed symbolically, but we still want the
    -- non-symbolic elements when we index by an integer constant.
    data Array CC sh (CExp e) = CC sh (Array D sh (Maybe (CExp e))) (CExp e)

    extent (CC sh _ _) = sh

    index (CC sh a ce) i = fromMaybe (index (C sh ce) i) (index a i)

instance Index CC (Z :. Int) (CExp Int) (CExp e) where
    (!) (CC _ a _) (CInt i) | Just ce <- a ! i =
        ce

    (!) (CC _ _ ce) ci =
        CExp [cexp|$ce[$ci]|]

instance Index CC (Z :. Int :. Int) (CExp Int, CExp Int) (CExp e) where
    (!) (CC _ a _) (CInt i, CInt j) | Just ce <- a ! (i, j) =
        ce

    (!) (CC _ _ ce) (ci, cj) =
        CExp [cexp|$ce[$ci][$cj]|]

instance ToCType e => IsCArray CC (Z :. Int) e where
    cindex (CC _ a _) (Z :. CInt i) | Just ce <- a ! i =
        ce

    cindex (CC _ _ ce) (Z :. ci) =
        CExp [cexp|$ce[$ci]|]

instance ToCType e => IsCArray CC (Z :. Int :. Int) e where
    cindex (CC _ a _) (Z :. CInt i :. CInt j) | Just ce <- a ! (i, j) =
        ce

    cindex (CC _ _ ce) (Z :. ci :. cj) =
        CExp [cexp|$ce[$ci][$cj]|]

instance CIndex CC (Z :. Int) Int (CExp e) where
    a !! i = return $ a ! i

instance CIndex CC (Z :. Int :. Int) (Int, Int) (CExp e) where
    a !! i = return $ a ! i
