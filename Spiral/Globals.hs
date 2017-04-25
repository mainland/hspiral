-- |
-- Module      : Spiral.Globals
-- Copyright   : (c) 2016-2017 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@drexel.edu>

module Spiral.Globals (
    setUseComplexType,
    useComplexType,
    setMinimizeAdds,
    minimizeAdds
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

gMinimizeAdds :: IORef Bool
{-# NOINLINE gMinimizeAdds #-}
gMinimizeAdds = unsafePerformIO $ newIORef False

setMinimizeAdds :: MonadIO m => Bool -> m ()
setMinimizeAdds flag = liftIO $ writeIORef gMinimizeAdds flag

minimizeAdds :: Bool
{-# NOINLINE minimizeAdds #-}
minimizeAdds = unsafePerformIO $ readIORef gMinimizeAdds
