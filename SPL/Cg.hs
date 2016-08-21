{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  SPL.Cg
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module SPL.Cg (
    CVec(..),
    MonadCg(..),

    cgSPL
  ) where

import Data.Complex

import SPL.Cg.Monad
import SPL.Exp
import SPL.Syntax

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
