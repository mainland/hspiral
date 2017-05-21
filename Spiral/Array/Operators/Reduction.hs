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

import Spiral.Array
import Spiral.Monad
import Spiral.Program.Monad
import Spiral.Exp

foldP :: forall a b r m .
         ( Typed a
         , Typed b
         , Num (Exp a)
         , Num (Exp b)
         , SArray r DIM1 (Exp a)
         , MonadSpiral m
         )
      => (Exp b -> Exp a -> Exp b)
      -> Exp b
      -> Array r DIM1 (Exp a)
      -> P m (Exp b)
foldP f z xs =
    shouldUnroll n >>= go
  where
    Z :. n = extent xs

    go True =
        return $ foldr (flip f) z [xs !! i | i <- [0..n-1]]

    go _ = do
        temp <- tempP
        temp .:=. z
        forP 0 n $ \i ->
          temp .:=. f temp (xs !! i)
        return temp

sumP :: ( Num (Exp a)
        , Typed a
        , SArray r DIM1 (Exp a)
        , MonadSpiral m
        )
      => Array r DIM1 (Exp a)
      -> P m (Exp a)
sumP = foldP (+) 0
