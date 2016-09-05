{-# LANGUAGE RankNTypes #-}

-- |
-- Module      :  Spiral.Util.Lift
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Util.Lift (
    Unop(..),
    Binop(..),

    LiftNum(..),

    isZero,
    isOne,
    isNegOne
  ) where
