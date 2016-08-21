{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Cg
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Cg (
    CVec(..),
    MonadCg(..),

    cgSPL
  ) where

import Data.Complex

import Spiral.Cg.Monad
import Spiral.Exp
import Spiral.SPL

cgSPL :: forall m . (Num (CExp m), MonadCg m)
      => String
      -> SPL (Exp (Complex Double))
      -> m ()
cgSPL name e = cgTransform name (m, n) $ \vin vout -> go vin vout e
  where
    (m, n) = extent e

    go :: CVec m -> CVec m -> SPL (Exp (Complex Double)) -> m ()
    go vin vout mat =
        cgFor 0 m $ \ci -> do
          cout <- cgVIdx vout ci
          cgAssign cout 0
          cgFor 0 n $ \cj -> do
            cin <- cgVIdx vin cj
            cij <- cgIdx mat (ci, cj)
            cgAssign cout (cout + cin * cij)
