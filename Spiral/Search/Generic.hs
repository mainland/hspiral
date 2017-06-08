{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Search.Generic
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Search.Generic (
    runSearch,
    search
  ) where

import Data.Typeable (Typeable)
import Text.PrettyPrint.Mainland hiding ((<|>))

import Spiral.Exp
import Spiral.Monad
import Spiral.Search.Monad
import Spiral.RootOfUnity
import Spiral.SPL

type SearchFun m a = (Typeable a, Typed a, RootOfUnity (Exp a), MonadSpiral m)
                   => Int
                   -> Exp a
                   -> S m (SPL (Exp a))

-- | Run a search
runSearch :: (Typeable a, Typed a, Num (Exp a), MonadSpiral m)
          => (forall a . SearchFun m a)
          -> SPL (Exp a)
          -> m (SPL (Exp a))
runSearch f e = runS $ search f e

-- | Search for the form of a transform with the best op-count.
search :: forall a m . (Typeable a, Typed a, Num (Exp a), MonadSpiral m)
       => (forall a . SearchFun m a)
       -> SPL (Exp a)
       -> S m (SPL (Exp a))
search f = go
  where
    go :: forall a . (Typeable a, Typed a, Num (Exp a))
       => SPL (Exp a)
       -> S m (SPL (Exp a))
    go e@(F n w)
      | n <= 2    = return e
      | otherwise = f n w

    go (Kron e1 e2) =
        Kron <$> go e1 <*> go e2

    go (DSum e1 e2) =
        merge <$> go e1 <*> go e2
      where
        merge :: forall a . SPL a -> SPL a -> SPL a
        merge (Diag xs) (Diag ys) = Diag (xs <> ys)
        merge e1'       e2'       = DSum e1' e2'

    go (Prod e1 e2) =
        Prod <$> go e1 <*> go e2

    go e@E{}     = return e
    go e@Diag{}  = return e
    go e@KDiag{} = return e
    go e@Circ{}  = return e
    go e@Toep{}  = return e
    go e@I{}     = return e
    go e@Rot{}   = return e
    go e@Pi{}    = return e
    go e@F2{}    = return e
    go (Re e)    = Re <$> go e
    go (DFT n)   = go $ F n (omega n)
    go (DFT' n)  = go $ F' n (omega n)
    go (F' n w)  = go $ KDiag n (1/fromIntegral n) × F n (1/w)
