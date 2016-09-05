-- |
-- Module      : Spiral.Driver.Globals
-- Copyright   : (c) 2016 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@drexel.edu>

module Spiral.Driver.Globals (
    setUseComplexType,
    useComplexType
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
