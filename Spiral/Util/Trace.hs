{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Util.Trace
-- Copyright   :  (c) 2014-2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@cs.drexel.edu

module Spiral.Util.Trace (
    MonadTrace(..),

    traceNest,

    traceCg,
    traceSearch
  ) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Exception (ExceptionT(..), runExceptionT)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (StateT(..))
import qualified Control.Monad.State.Strict as S (StateT(..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Cont (ContT(..))
import qualified Control.Monad.Trans.Cont as Cont
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Writer (WriterT(..))
import qualified Control.Monad.Writer.Strict as S (WriterT(..))
import System.IO (hPutStrLn,
                  stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.PrettyPrint.Mainland

import Spiral.Config

class MonadConfig m => MonadTrace m where
    askTraceDepth   :: m Int
    localTraceDepth :: (Int -> Int) -> m a -> m a

instance MonadTrace m => MonadTrace (MaybeT m) where
    askTraceDepth       = lift askTraceDepth
    localTraceDepth f m = MaybeT $ localTraceDepth f (runMaybeT m)

instance MonadTrace m => MonadTrace (ContT r m) where
    askTraceDepth   = lift askTraceDepth
    localTraceDepth = Cont.liftLocal askTraceDepth localTraceDepth

instance (MonadTrace m) => MonadTrace (ExceptT e m) where
    askTraceDepth       = lift askTraceDepth
    localTraceDepth f m = ExceptT $ localTraceDepth f (runExceptT m)

instance (MonadTrace m) => MonadTrace (ExceptionT m) where
    askTraceDepth       = lift askTraceDepth
    localTraceDepth f m = ExceptionT $ localTraceDepth f (runExceptionT m)

instance MonadTrace m => MonadTrace (ReaderT r m) where
    askTraceDepth       = lift askTraceDepth
    localTraceDepth f m = ReaderT $ \r -> localTraceDepth f (runReaderT m r)

instance MonadTrace m => MonadTrace (StateT s m) where
    askTraceDepth       = lift askTraceDepth
    localTraceDepth f m = StateT $ \s -> localTraceDepth f (runStateT m s)

instance MonadTrace m => MonadTrace (S.StateT s m) where
    askTraceDepth       = lift askTraceDepth
    localTraceDepth f m = S.StateT $ \s -> localTraceDepth f (S.runStateT m s)

instance (Monoid w, MonadTrace m) => MonadTrace (WriterT w m) where
    askTraceDepth       = lift askTraceDepth
    localTraceDepth f m = WriterT $ localTraceDepth f (runWriterT m)

instance (Monoid w, MonadTrace m) => MonadTrace (S.WriterT w m) where
    askTraceDepth       = lift askTraceDepth
    localTraceDepth f m = S.WriterT $ localTraceDepth f (S.runWriterT m)

traceNest :: MonadTrace m => m a -> m a
traceNest = localTraceDepth (+1)

trace :: MonadTrace m => String -> Doc -> m ()
trace prefix doc = do
    d <- askTraceDepth
    let d'    = length prefix + 1 + 2*d
    let doc'  = spaces (2*d) <> nest d' doc
    return $!
        unsafePerformIO $
        hPutStrLn stderr (pretty 80 (text prefix <+> doc'))

traceIfSet :: MonadTrace m => TraceFlag -> String -> Doc -> m ()
traceIfSet flag prefix doc = do
    doTrace <- asksConfig (testTraceFlag flag)
    when doTrace $
        trace prefix doc

traceCg :: MonadTrace m => Doc -> m ()
traceCg = traceIfSet TraceCg "traceCg:"

traceSearch :: MonadTrace m => Doc -> m ()
traceSearch = traceIfSet TraceSearch "traceSearch:"
