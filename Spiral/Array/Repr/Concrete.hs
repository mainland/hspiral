{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Spiral.Array.Repr.Concrete
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Repr.Concrete (
    C,
    Array(..)
  ) where

import Prelude hiding ((!!), read)

import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral.Array
import Spiral.Exp
import {-# SOURCE #-} Spiral.Program.Monad

-- | Type tag for a concrete array backed by storage in a variable.
data C

instance IsArray C sh e where
    -- | A matrix whose entries are stored in a concrete location.
    data Array C sh e = C sh Var

    extent (C sh _) = sh

instance Shape sh => IArray C sh (Exp e) where
    index (C _sh v) ix = IdxE v (map intE (listOfShape ix))

instance Shape sh => SArray C sh (Exp e) where
    indexS (C _sh v) ix = IdxE v (listOfExpShape ix)

instance (Shape sh, Typed a, Num (Exp a)) => MArray C sh (Exp a) where
    read  (C _sh v) ix = return $ IdxE v (listOfExpShape ix)
    write (C _sh v) ix = assignP (IdxE v (listOfExpShape ix))

instance Shape sh => Computable C sh (Exp a) where
    computeP a b =
        forShapeP (extent b) $ \ix ->
            write a ix (indexS b ix)

instance (Shape sh, Typed a) => Pretty (Array C sh (Exp a)) where
    ppr (C sh v) = ppr tau <> ppr (listOfShape sh) <+> ppr v
      where
        tau :: Type a
        tau = typeOf (undefined :: a)
