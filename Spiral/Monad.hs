{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Monad
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Monad (
    Spiral,
    runSpiral,

    MonadCg
  ) where

import Control.Monad.Exception (MonadException(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad(..),
                                RealWorld)
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.Reader (MonadReader(..),
                             ReaderT,
                             asks,
                             runReaderT)
import Data.IORef (IORef)

import Spiral.Config
import Spiral.Trace
import Spiral.Util.Uniq

data SpiralEnv = SpiralEnv
    { uniq       :: IORef Int
    , config     :: Config
    , tracedepth :: !Int
    }

defaultSpiralEnv :: MonadRef IORef m => m SpiralEnv
defaultSpiralEnv = do
    r <- newRef 0
    return SpiralEnv { uniq = r, config = mempty, tracedepth = 0 }

newtype Spiral a = Spiral { unSpiral :: ReaderT SpiralEnv IO a }
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadException,
              MonadReader SpiralEnv,
              MonadRef IORef)

runSpiral :: Spiral a -> IO a
runSpiral m = defaultSpiralEnv >>= runReaderT (unSpiral m)

instance PrimMonad Spiral where
    type PrimState Spiral = RealWorld
    primitive = Spiral . primitive

instance MonadConfig Spiral where
    askConfig     = asks config
    localConfig f = local (\env -> env { config = f (config env) })

instance MonadTrace Spiral where
    askTraceDepth     = asks tracedepth
    localTraceDepth f = local $ \env -> env { tracedepth = f (tracedepth env) }

instance MonadUnique Spiral where
    newUnique = do
        r <- asks uniq
        u <- readRef r
        let u' = u + 1
        u' `seq` writeRef r u'
        return $ Uniq u

class ( PrimMonad m
      , PrimState m ~ RealWorld
      , MonadRef IORef m
      , MonadConfig m
      , MonadUnique m
      , MonadTrace m
      ) => MonadCg m where

instance MonadCg Spiral where
