{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Spiral.Search.SFKT
-- Copyright   :  (c) 2015-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Search.SFKT (
    SFKT(..),
    runSFKT,
    runSFKT1,
    runSFKTM
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..),
                      ap)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logic (MonadLogic(..),
                            reflect)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans (MonadTrans(..))

import Spiral.Config
import Spiral.Util.Trace
import Spiral.Util.Uniq

type SK ans a = a -> FK ans -> ans

type FK ans = ans

newtype SFKT m a = SFKT { unSFKT :: forall ans . FK (m ans) -> SK (m ans) a -> m ans }

runSFKT :: forall m a . Monad m => SFKT m a -> m a
runSFKT m =
    unSFKT m failk $ \x _fk -> return x
  where
    failk :: FK (m a)
    failk = fail "no answer"

runSFKT1 :: forall m a . Monad m => SFKT m a -> m (Maybe a)
runSFKT1 m =
    unSFKT m failk $ \x _fk -> return (Just x)
  where
    failk :: FK (m (Maybe a))
    failk = return Nothing

runSFKTM :: forall m a . Monad m => Maybe Int -> SFKT m a -> m [a]
runSFKTM Nothing m = unSFKT m failk succk
  where
    failk :: FK (m [a])
    failk = return []

    succk :: SK (m [a]) a
    succk x fk = fmap (x:) fk

runSFKTM (Just n) _ | n <= 0 = return []

runSFKTM (Just 1) m = unSFKT m failk succk
  where
    failk :: FK (m [a])
    failk = return []

    succk :: SK (m [a]) a
    succk x _fk = return [x]

runSFKTM (Just n) m = unSFKT (msplit m) failk succk
  where
    failk :: FK (m [a])
    failk = return []

    succk :: SK (m [a]) (Maybe (a, SFKT m a))
    succk Nothing        _fk = return []
    succk (Just (x, m')) _fk = fmap (x:) (runSFKTM (Just (n-1)) m')

liftLocal :: Monad m
          => m r'
          -> ((r' -> r') -> forall r . (m r -> m r))
          -> (r' -> r')
          -> SFKT m a
          -> SFKT m a
liftLocal ask local f m = SFKT $ \fk sk -> do
    r <- ask
    let fk'       = local (const r) fk
        sk' x fk' = local (const r) (sk x fk')
    local f (unSFKT m fk' sk')

instance MonadTrans SFKT where
    lift m = SFKT $ \fk sk -> m >>= \x -> sk x fk

instance Monad m => Functor (SFKT m) where
    fmap f x = x >>= return . f

instance Monad m => Applicative (SFKT m) where
    pure  = return
    (<*>) = ap

instance Monad m => Alternative (SFKT m) where
    empty = SFKT $ \fk _sk -> fk

    m1 <|> m2 = SFKT $ \fk sk -> unSFKT m1 (unSFKT m2 fk sk) sk

instance Monad m => Monad (SFKT m) where
    {-# INLINE return #-}
    return x = SFKT $ \fk sk -> sk x fk

    {-# INLINE (>>=) #-}
    m >>= f  = SFKT $ \fk sk ->
               unSFKT m     fk  $ \x fk'  ->
               unSFKT (f x) fk' $ \y fk'' ->
               sk y fk''

    {-# INLINE (>>) #-}
    m1 >> m2 = SFKT $ \fk sk ->
               unSFKT m1 fk  $ \_ fk'  ->
               unSFKT m2 fk' $ \y fk'' ->
               sk y fk''

    fail msg = SFKT $ \_fk _sk -> fail msg

instance Monad m => MonadPlus (SFKT m) where
    mzero = Control.Applicative.empty
    mplus = (Control.Applicative.<|>)

instance Monad m => MonadLogic (SFKT m) where
    msplit m = lift $ unSFKT m fk sk
      where
        fk      = return Nothing
        sk x fk = return $ Just (x, lift fk >>= reflect)

instance MonadIO m => MonadIO (SFKT m) where
    liftIO = lift . liftIO

instance MonadReader r m => MonadReader r (SFKT m) where
    ask       = lift ask
    local f m = SFKT $ \fk sk -> local f (unSFKT m fk sk)

instance MonadState s m => MonadState s (SFKT m) where
    get   = lift get
    put s = SFKT $ \fk sk -> put s >> sk () fk

instance MonadRef r m => MonadRef r (SFKT m) where
    newRef       = lift . newRef
    readRef      = lift . readRef
    writeRef r x = lift $ writeRef r x

instance MonadUnique m => MonadUnique (SFKT m) where
    newUnique   = lift newUnique
    resetUnique = SFKT $ \fk sk -> resetUnique >> sk () fk

instance MonadConfig m => MonadConfig (SFKT m) where
    askConfig   = lift askConfig
    localConfig = liftLocal askConfig localConfig

instance MonadTrace m => MonadTrace (SFKT m) where
    askTraceDepth       = lift askTraceDepth
    localTraceDepth f m = SFKT $ \fk sk -> localTraceDepth f (unSFKT m fk sk)

instance PrimMonad m => PrimMonad (SFKT m) where
    type PrimState (SFKT m) = PrimState m
    primitive = lift . primitive
