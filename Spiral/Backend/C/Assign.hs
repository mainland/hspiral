{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      :  Spiral.Backend.C.Assign
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.Assign (
    CAssign(..),
    (.:=.)
  ) where

import Data.Complex
import Language.C.Pretty ()
import Language.C.Quote.C

import Spiral.Backend.C.Array
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Monad
import Spiral.Backend.C.Types
import Spiral.Monad (MonadCg)
import Spiral.SPL

class CAssign a where
    -- | Compile an assignment.
    cassign :: MonadCg m => a -> a -> Cg m ()

infix 4 .:=.
(.:=.) :: (CAssign a, MonadCg m) => a -> a -> Cg m ()
(.:=.) = cassign

instance CAssign (CExp Int) where
    cassign ce1 ce2 = appendStm [cstm|$ce1 = $ce2;|]

instance CAssign (CExp Double) where
    cassign ce1 ce2 = appendStm [cstm|$ce1 = $ce2;|]

instance CAssign (CExp (Complex Double)) where
    cassign ce1 ce2 = appendStm [cstm|$ce1 = $ce2;|]

instance (ToCType a, CAssign (CExp a)) => CAssign (Vector C (CExp a)) where
    cassign (C _ ce) x = compute [cexp|$ce|] x
