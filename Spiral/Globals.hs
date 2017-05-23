-- |
-- Module      : Spiral.Globals
-- Copyright   : (c) 2016-2017 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@drexel.edu>

module Spiral.Globals (
    setThreeMults,
    threeMults
  ) where

import Control.Monad.Trans (MonadIO(..))
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

gThreeMults :: IORef Bool
{-# NOINLINE gThreeMults #-}
gThreeMults = unsafePerformIO $ newIORef True

setThreeMults :: MonadIO m => Bool -> m ()
setThreeMults flag = liftIO $ writeIORef gThreeMults flag

threeMults :: Bool
{-# NOINLINE threeMults #-}
threeMults = unsafePerformIO $ readIORef gThreeMults
