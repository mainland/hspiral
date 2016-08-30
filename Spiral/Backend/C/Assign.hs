{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Spiral.Monad (MonadCg)

class CAssign a b where
    -- | Compile an assignment.
    cassign :: MonadCg m => a -> b -> Cg m ()

infix 4 .:=.
(.:=.) :: (CAssign a b, MonadCg m) => a -> b -> Cg m ()
(.:=.) = cassign

instance CAssign (CExp Int) (CExp Int) where
    cassign ce1 ce2 = appendStm [cstm|$ce1 = $ce2;|]

instance CAssign (CExp Double) (CExp Double) where
    cassign ce1 ce2 = appendStm [cstm|$ce1 = $ce2;|]

instance CAssign (CExp (Complex Double)) (CExp (Complex Double)) where
    cassign ce1 ce2 = appendStm [cstm|$ce1 = $ce2;|]

instance (MCArray r1 sh a, CArray r2 sh a) => CAssign (Array r1 sh (CExp a)) (Array r2 sh (CExp a)) where
    cassign = compute
