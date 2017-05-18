-- |
-- Module      : Spiral.Globals
-- Copyright   : (c) 2016-2017 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@drexel.edu>

module Spiral.Globals (
    setUseComplexType,
    useComplexType,
    setMinimizeMults,
    minimizeMults
  ) where

import Control.Monad.Trans (MonadIO(..))
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

gUseComplexType :: IORef Bool
{-# NOINLINE gUseComplexType #-}
gUseComplexType = unsafePerformIO $ newIORef False

setUseComplexType :: MonadIO m => Bool -> m ()
setUseComplexType flag = liftIO $ writeIORef gUseComplexType flag

useComplexType :: Bool
{-# NOINLINE useComplexType #-}
useComplexType = unsafePerformIO $ readIORef gUseComplexType

gMinimizeMults :: IORef Bool
{-# NOINLINE gMinimizeMults #-}
gMinimizeMults = unsafePerformIO $ newIORef True

setMinimizeMults :: MonadIO m => Bool -> m ()
setMinimizeMults flag = liftIO $ writeIORef gMinimizeMults flag

minimizeMults :: Bool
{-# NOINLINE minimizeMults #-}
minimizeMults = unsafePerformIO $ readIORef gMinimizeMults
