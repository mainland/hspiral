{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Array.Operators.Reduction
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Operators.Reduction (
    foldP,

    sumP
  ) where

import Prelude hiding ((!!))

import Control.Monad (foldM)

import Spiral.Array
import Spiral.Array.Program
import Spiral.Exp

foldP :: forall a b r m .
         ( Typed a
         , Typed b
         , Num (Exp a)
         , Num (Exp b)
         , SArray r DIM1 (Exp a)
         , MonadP m
         )
      => (Exp b -> Exp a -> Exp b)
      -> Exp b
      -> Array r DIM1 (Exp a)
      -> m (Exp b)
foldP f z xs =
    shouldUnroll n >>= go
  where
    Z :. n = extent xs

    go True = do
        es <- mapM idx [0..n-1]
        foldM g z es
      where
        idx :: Int -> m (Exp a)
        idx i = cache e
          where
            e = xs !! i

        g e1 e2 = cache (f e1 e2)

    go _ = do
        temp <- tempP
        temp .:=. z
        forP 0 n $ \i ->
          temp .:=. f temp (xs !! i)
        return temp

sumP :: ( Num (Exp a)
        , Typed a
        , SArray r DIM1 (Exp a)
        , MonadP m
        )
      => Array r DIM1 (Exp a)
      -> m (Exp a)
sumP = foldP (+) 0
