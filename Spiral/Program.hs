{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      :  Spiral.Program
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Program (
    toProgram
  ) where

import Spiral.Array
import Spiral.Array.Program
import qualified Spiral.Array.Program as P
import Spiral.Array.Repr.Transform (fromGather)
import Spiral.Exp
import Spiral.Monad (MonadSpiral)
import Spiral.Program.Monad
import Spiral.Program.Syntax
import Spiral.SPL
import Spiral.SPL.Run

-- | Generate code for an SPL transform.
toProgram :: forall a m . (Num (Exp a), Typed a, MonadSpiral m)
          => String
          -> SPL (Exp a)
          -> m (Program a)
toProgram f a = do
    stms <- evalP $ runSPL a (fromGather x) >>= P.computeP y
    return $ Program f x y stms
   where
     x, y :: Vector C (Exp a)
     x = P.C (ix1 n) "X"
     y = P.C (ix1 m) "Y"

     m, n :: Int
     Z :. m :. n = splExtent a
