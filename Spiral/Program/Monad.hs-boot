{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Program.Monad
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Program.Monad (
    P(..),

    assignP,
    forP
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)

import Spiral.Exp
import Spiral.Monad

data PEnv

data PState

newtype P m a = P { unP :: StateT PState (ReaderT PEnv m) a}

instance Functor m => Functor (P m) where

instance Monad m => Applicative (P m) where

instance Monad m => Monad (P m) where

instance MonadIO m => MonadIO (P m) where

assignP :: forall a m . (Typed a, Num (Exp a), MonadSpiral m) => Exp a -> Exp a -> P m ()

forP :: MonadSpiral m => Int -> Int -> (Exp Int -> P m ()) -> P m ()
