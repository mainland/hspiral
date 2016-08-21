{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  SPL.Monad
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module SPL.Monad (
    SPLM,
    runSPLM
  ) where

import Control.Monad.Exception (MonadException(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.State (MonadState(..),
                            StateT,
                            evalStateT,
                            gets)
import Data.IORef (IORef)

import SPL.Uniq

data SPLState = SPLState { uniq :: IORef Int }

defaultSPLState :: MonadRef IORef m => m SPLState
defaultSPLState = do
    r <- newRef 0
    return SPLState { uniq = r }

newtype SPLM a = SPLM { unSPLM :: StateT SPLState IO a }
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadException,
              MonadState SPLState,
              MonadRef IORef)

runSPLM :: SPLM a -> IO a
runSPLM m = defaultSPLState >>= evalStateT (unSPLM m)

instance MonadUnique SPLM where
    newUnique = do
        r <- gets uniq
        u <- readRef r
        let u' = u + 1
        u' `seq` writeRef r u'
        return $ Uniq u
