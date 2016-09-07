{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      :  Spiral.Util.Uniq
-- Copyright   :  (c) 2014-2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@cs.drexel.edu

module Spiral.Util.Uniq (
    Uniq(..),
    MonadUnique(..),

    Gensym(..)
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Monad.Error (Error, ErrorT(..))
#endif /* !MIN_VERSION_base(4,8,0) */
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Exception (ExceptionT(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (StateT(..))
import qualified Control.Monad.State.Strict as S (StateT(..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Cont (ContT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Writer (WriterT(..))
import qualified Control.Monad.Writer.Strict as S (WriterT(..))
import Data.Loc (Located,
                 Loc,
                 noLoc,
                 srclocOf)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid)
#endif /* !MIN_VERSION_base(4,8,0) */
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland

-- | A unique value.
newtype Uniq = Uniq Int
  deriving (Eq, Ord, Read, Show)

instance Pretty Uniq where
    ppr (Uniq u) = ppr u

-- | A monad that can generate unique values.
class Monad m => MonadUnique m where
    -- | Generate a new unique.
    newUnique :: m Uniq

instance MonadUnique m => MonadUnique (MaybeT m) where
    newUnique = lift newUnique

instance MonadUnique m => MonadUnique (ContT r m) where
    newUnique = lift newUnique

#if !MIN_VERSION_base(4,8,0)
instance (Error e, MonadUnique m) => MonadUnique (ErrorT e m) where
    newUnique = lift newUnique
#endif /* !MIN_VERSION_base(4,8,0) */

instance MonadUnique m => MonadUnique (ExceptT e m) where
    newUnique = lift newUnique

instance MonadUnique m => MonadUnique (ExceptionT m) where
    newUnique = lift newUnique

instance MonadUnique m => MonadUnique (ReaderT r m) where
    newUnique = lift newUnique

instance MonadUnique m => MonadUnique (StateT s m) where
    newUnique = lift newUnique

instance MonadUnique m => MonadUnique (S.StateT s m) where
    newUnique = lift newUnique

instance (Monoid w, MonadUnique m) => MonadUnique (WriterT w m) where
    newUnique = lift newUnique

instance (Monoid w, MonadUnique m) => MonadUnique (S.WriterT w m) where
    newUnique = lift newUnique

-- | A type that can be gensym'd.
class Gensym a where
    -- | Gensym a symbol using the given string as a basis.
    gensym :: MonadUnique m => String -> m a
    gensym s = gensymAt s (noLoc :: Loc)

    -- | Gensym a symbol using the given string and location as a basis.
    gensymAt :: (MonadUnique m, Located l) => String -> l -> m a
    gensymAt s _ = gensym s

    -- | Ensure the symbol is unique
    uniquify :: MonadUnique m => a -> m a

instance Gensym String where
    gensymAt s _ = do
        Uniq u <- newUnique
        return $ if u == 0 then s else s ++ show u

    uniquify s = do
        Uniq u <- newUnique
        return $ if u == 0 then s else s ++ show u

instance Gensym C.Id where
    gensymAt s l =
        C.Id <$> gensymAt s l <*> pure (srclocOf l)

    uniquify (C.Id s l) =
        C.Id <$> uniquify s <*> pure l

    uniquify cid =
        return cid
