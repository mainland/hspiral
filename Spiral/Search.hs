{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Search
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Search (
    S(..),
    runS,
    observeAll,

    (<.>),

    runSearch,
    search
  ) where

import Control.Applicative ((<|>))
import Control.Monad (MonadPlus)
import Data.Typeable (Typeable)

import Spiral.Exp
import Spiral.Monad
import Spiral.SPL
import Spiral.RootOfUnity
import Spiral.Search.Monad

type SearchFun s m a = (Typeable a, Typed a) => SPL (Exp a) -> S s m (SPL (Exp a))

(<.>) :: MonadPlus m => (a -> m b) -> (a -> m b) -> a -> m b
f <.> g = \x -> f x <|> g x

-- | Run a search
runSearch :: (Typeable a, Typed a, Num (Exp a), MonadSpiral m)
          => s
          -> (forall a . SearchFun s m a)
          -> SPL (Exp a)
          -> m (SPL (Exp a))
runSearch s f e = runS (search f e) s

-- | Search rewrites of the given formula with the given rewriting function.
search :: forall s m a . (Typeable a, Typed a, Num (Exp a), MonadSpiral m)
       => (forall a . SearchFun s m a)
       -> SPL (Exp a)
       -> S s m (SPL (Exp a))
search f = go
  where
    go :: forall a . (Typeable a, Typed a, Num (Exp a))
       => SPL (Exp a)
       -> S s m (SPL (Exp a))
    go e = rewrite e <|> recurse e

    rewrite :: forall a . (Typeable a, Typed a, Num (Exp a))
            => SPL (Exp a)
            -> S s m (SPL (Exp a))
    rewrite e = do
        e' <- f e
        logRewrite
        go e'

    recurse :: forall a . (Typeable a, Typed a, Num (Exp a))
            => SPL (Exp a)
            -> S s m (SPL (Exp a))
    recurse (Kron e1 e2) =
        whenRewritten ((⊗) <$> go e1 <*> go e2) go

    recurse (DSum e1 e2) =
        whenRewritten ((⊕) <$> go e1 <*> go e2) go

    recurse (Prod e1 e2) =
        whenRewritten ((×) <$> go e1 <*> go e2) go

    recurse (Re e) =
        whenRewritten (Re <$> go e) go

    -- XXX We may want to break out the following rewrites into a separate rule.
    recurse (DFT 2) =
        go F2

    recurse (DFT n) =
        go $ F n (omega n)

    recurse (DFT' n) =
        go $ F' n (omega n)

    recurse (F 2 w) | w == omega 2 =
        go F2

    recurse (F' n w) =
        go $ KDiag n (1/fromIntegral n) × F n (1/w)

    recurse e =
        return e
