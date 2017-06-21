{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Search.Monad
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Search.Monad (
    S(..),
    runS,

    observeAll,

    logRewrite,
    whenRewritten
  ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logic (MonadLogic(..),)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.State (MonadState(..),
                            StateT(..),
                            evalStateT,
                            gets,
                            modify)
import Control.Monad.Trans (MonadTrans(..))
import Data.IORef (IORef)

import Spiral.Config
import Spiral.Monad
import Spiral.Search.SFKT
import Spiral.Util.Trace
import Spiral.Util.Uniq

newtype SState = SState { rewrites :: Int }

instance Monoid SState where
    mempty = SState 0

    x `mappend` y = SState { rewrites = rewrites x + rewrites y }

newtype S s m a = S { unS :: StateT SState (SFKT (StateT s m)) a }
  deriving (Functor, Applicative, Monad,
            Alternative, MonadPlus,
            MonadIO,
            MonadLogic,
            MonadUnique,
            MonadConfig,
            MonadTrace)

instance MonadTrans (S s) where
    lift m = S $ lift $ lift $ lift m

deriving instance MonadRef IORef m => MonadRef IORef (S s m)

instance PrimMonad m => PrimMonad (S s m) where
    type PrimState (S s m) = PrimState m
    primitive = S . primitive

instance Monad m => MonadState s (S s m) where
    get     = S $ lift get
    put s   = S $ lift $ put s
    state f = S $ lift $ state f

instance MonadSpiral m => MonadSpiral (S s m) where

runS :: forall s m a . Monad m
     => S s m a
     -> s
     -> m a
runS m s = evalStateT (runSFKT (evalStateT (unS m) mempty)) s

-- | Observe all search results.
observeAll :: forall s m a . Monad m => S s m a -> S s m [a]
observeAll m = S $ lift $ lift $ runSFKTM Nothing (evalStateT (unS m) mempty)

logRewrite :: Monad m => S s m ()
logRewrite = S $ modify $ \s -> s { rewrites = rewrites s + 1 }

whenRewritten :: Monad m => S s m a -> (a -> S s m a) -> S s m a
whenRewritten m f = do
    n  <- S $ gets rewrites
    x  <- m
    n' <- S $ gets rewrites
    if n' > n
      then f x
      else return x
