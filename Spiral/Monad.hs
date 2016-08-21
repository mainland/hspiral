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
import Control.Monad.Reader (MonadReader(..),
                             ReaderT,
                             asks,
                             runReaderT)
import Data.IORef (IORef)

import Spiral.Config
import Spiral.Util.Uniq

data SpiralEnv = SpiralEnv
    { uniq   :: IORef Int
    , config :: Config
    }

defaultSpiralEnv :: MonadRef IORef m => m SpiralEnv
defaultSpiralEnv = do
    r <- newRef 0
    return SpiralEnv { uniq = r, config = mempty }

newtype Spiral a = Spiral { unSpiral :: ReaderT SpiralEnv IO a }
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadException,
              MonadReader SpiralEnv,
              MonadRef IORef)

runSpiral :: Spiral a -> IO a
runSpiral m = defaultSpiralEnv >>= runReaderT (unSpiral m)

instance MonadConfig Spiral where
    askConfig     = asks config
    localConfig f = local (\env -> env { config = f (config env) })

instance MonadUnique Spiral where
    newUnique = do
        r <- asks uniq
        u <- readRef r
        let u' = u + 1
        u' `seq` writeRef r u'
        return $ Uniq u
