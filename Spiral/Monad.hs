{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Spiral.Monad
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Monad (
    Spiral,
    runSpiral
  ) where

import Control.Monad.Exception (MonadException(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.State (MonadState(..),
                            StateT,
                            evalStateT,
                            gets)
import Data.IORef (IORef)

import Spiral.Util.Uniq

data SpiralState = SpiralState { uniq :: IORef Int }

defaultSpiralState :: MonadRef IORef m => m SpiralState
defaultSpiralState = do
    r <- newRef 0
    return SpiralState { uniq = r }

newtype Spiral a = Spiral { unSpiral :: StateT SpiralState IO a }
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadException,
              MonadState SpiralState,
              MonadRef IORef)

runSpiral :: Spiral a -> IO a
runSpiral m = defaultSpiralState >>= evalStateT (unSpiral m)

instance MonadUnique Spiral where
    newUnique = do
        r <- gets uniq
        u <- readRef r
        let u' = u + 1
        u' `seq` writeRef r u'
        return $ Uniq u
