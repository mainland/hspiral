{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Config
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Config (
    ModeFlag(..),
    DynFlag(..),
    TraceFlag(..),

    Config(..),
    defaultConfig,

    setMode,

    testDynFlag,
    setDynFlag,
    setDynFlags,
    unsetDynFlag,

    testTraceFlag,
    setTraceFlag,
    setTraceFlags,

    whenDynFlag,

    MonadConfig(..),
    asksConfig
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
import Data.List (foldl')
import Data.Monoid
import Data.Word (Word32)
import Data.Bits

data ModeFlag = Help
              | Compile
  deriving (Eq, Ord, Enum, Show)

data DynFlag = Quiet
             | LinePragmas
             | UseComplex
             | GenComments
  deriving (Eq, Ord, Enum, Bounded, Show)

data TraceFlag = TraceCg
  deriving (Eq, Ord, Enum, Bounded, Show)

data Config = Config
    { -- | Compiler mode
      mode :: !ModeFlag

    , -- | Dynamic flags
      dynFlags :: !(FlagSet DynFlag)

      -- | Flags for tracing
    , traceFlags  :: !(FlagSet TraceFlag)

    , -- | Verbosity level
      verbLevel :: !Int

    , -- | Maximum iterations of for loop to automatically unroll.
      maxUnroll :: !Int

    , -- | Output path
      output :: Maybe FilePath
    }
  deriving (Eq, Ord, Show)

instance Monoid Config where
    mempty = Config
        { mode       = Compile
        , dynFlags   = mempty
        , traceFlags = mempty
        , verbLevel  = 0
        , maxUnroll  = 0
        , output     = Nothing
        }

    mappend x y = Config
        { mode       = mode y
        , dynFlags   = dynFlags x <> dynFlags y
        , traceFlags = traceFlags x <> traceFlags y
        , verbLevel  = verbLevel x + verbLevel y
        , maxUnroll  = max (maxUnroll x) (maxUnroll y)
        , output     = output x <> output y
        }

defaultConfig :: Config
defaultConfig = mempty

-- | A set of flags.
newtype FlagSet a = FlagSet Word32
  deriving (Eq, Ord)

testFlag :: Enum a => FlagSet a -> a -> Bool
testFlag (FlagSet fs) f = fs `testBit` fromEnum f

setFlag :: Enum a => FlagSet a -> a -> FlagSet a
setFlag (FlagSet fs) f = FlagSet $ fs `setBit` fromEnum f

unsetFlag :: Enum a => FlagSet a -> a -> FlagSet a
unsetFlag (FlagSet fs) f = FlagSet $ fs `clearBit` fromEnum f

instance Monoid (FlagSet a) where
    mempty = FlagSet 0

    FlagSet x `mappend` FlagSet y = FlagSet (x .|. y)

instance (Enum a, Bounded a, Show a) => Show (FlagSet a) where
    show (FlagSet n) = show [f | f <- [minBound..maxBound::a],
                                 n `testBit` fromEnum f]

class Monad m => MonadConfig m where
    askConfig   :: m Config
    localConfig :: (Config -> Config) -> m a -> m a

asksConfig :: MonadConfig m => (Config -> a) -> m a
asksConfig f = fmap f askConfig

instance MonadConfig m => MonadConfig (MaybeT m) where
    askConfig       = lift askConfig
    localConfig f m = MaybeT $ localConfig f (runMaybeT m)

instance MonadConfig m => MonadConfig (ContT r m) where
    askConfig   = lift askConfig
    localConfig = Cont.liftLocal askConfig localConfig

instance (MonadConfig m) => MonadConfig (ExceptT e m) where
    askConfig       = lift askConfig
    localConfig f m = ExceptT $ localConfig f (runExceptT m)

instance (MonadConfig m) => MonadConfig (ExceptionT m) where
    askConfig       = lift askConfig
    localConfig f m = ExceptionT $ localConfig f (runExceptionT m)

instance MonadConfig m => MonadConfig (ReaderT r m) where
    askConfig       = lift askConfig
    localConfig f m = ReaderT $ \r -> localConfig f (runReaderT m r)

instance MonadConfig m => MonadConfig (StateT s m) where
    askConfig       = lift askConfig
    localConfig f m = StateT $ \s -> localConfig f (runStateT m s)

instance MonadConfig m => MonadConfig (S.StateT s m) where
    askConfig       = lift askConfig
    localConfig f m = S.StateT $ \s -> localConfig f (S.runStateT m s)

instance (Monoid w, MonadConfig m) => MonadConfig (WriterT w m) where
    askConfig       = lift askConfig
    localConfig f m = WriterT $ localConfig f (runWriterT m)

instance (Monoid w, MonadConfig m) => MonadConfig (S.WriterT w m) where
    askConfig       = lift askConfig
    localConfig f m = S.WriterT $ localConfig f (S.runWriterT m)

setMode :: ModeFlag -> Config -> Config
setMode f flags = flags { mode = f }

testDynFlag :: DynFlag -> Config -> Bool
testDynFlag f flags = dynFlags flags `testFlag` f

setDynFlag :: DynFlag -> Config -> Config
setDynFlag f flags = flags { dynFlags = setFlag (dynFlags flags) f }

setDynFlags :: [DynFlag] -> Config -> Config
setDynFlags fs flags = foldl' (flip setDynFlag) flags fs

unsetDynFlag :: DynFlag -> Config -> Config
unsetDynFlag f flags = flags { dynFlags = unsetFlag (dynFlags flags) f }

testTraceFlag :: TraceFlag -> Config -> Bool
testTraceFlag f flags = traceFlags flags `testFlag` f

setTraceFlag :: TraceFlag -> Config -> Config
setTraceFlag f flags = flags { traceFlags = setFlag (traceFlags flags) f }

setTraceFlags :: [TraceFlag] -> Config -> Config
setTraceFlags fs flags = foldl' (flip setTraceFlag) flags fs

whenDynFlag :: MonadConfig m => DynFlag -> m () -> m ()
whenDynFlag f act = do
    doit <- asksConfig (testDynFlag f)
    when doit act
