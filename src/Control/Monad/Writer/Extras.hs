{-# LANGUAGE CPP #-}
-- | Lifted 'MonadWriter'.
module Control.Monad.Writer.Extras
  ( MonadWriter1(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Control.Monad.Trans.Class
import Control.Monad.Writer

import qualified Control.Monad.Trans.Except        as E
import qualified Control.Monad.Trans.Identity      as I
import qualified Control.Monad.Trans.Maybe         as M
import qualified Control.Monad.Trans.Reader        as R
import qualified Control.Monad.Trans.RWS.Lazy      as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict    as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy    as S.Lazy
import qualified Control.Monad.Trans.State.Strict  as S.Strict
import qualified Control.Monad.Trans.Writer.Lazy   as W.Lazy
import qualified Control.Monad.Trans.Writer.Strict as W.Strict

-- | Lifted 'MonadWriter'.
class MonadTrans t => MonadWriter1 t where
  -- | Lifted 'listen'.
  listen1 :: MonadWriter w m => t m a -> t m (a, w)
  -- | Lifted 'pass'.
  pass1 :: MonadWriter w m => t m (a, w -> w) -> t m a

instance MonadWriter1 (E.ExceptT e) where
  listen1 = E.liftListen listen
  pass1 = E.liftPass pass

instance MonadWriter1 I.IdentityT where
  listen1 = I.mapIdentityT listen
  pass1 = I.mapIdentityT pass

instance MonadWriter1 M.MaybeT where
  listen1 = M.liftListen listen
  pass1 = M.liftPass pass

instance MonadWriter1 (R.ReaderT r) where
  listen1 = R.mapReaderT listen
  pass1 = R.mapReaderT pass

instance MonadWriter1 (S.Lazy.StateT s) where
  listen1 = S.Lazy.liftListen listen
  pass1 = S.Lazy.liftPass pass

instance MonadWriter1 (S.Strict.StateT s) where
  listen1 = S.Strict.liftListen listen
  pass1 = S.Strict.liftPass pass

instance Monoid w' => MonadWriter1 (RWS.Lazy.RWST r w' s) where
  listen1 m = RWS.Lazy.RWST $ \r s -> do
    ~((a, w', s'), w) <- listen (RWS.Lazy.runRWST m r s)
    return ((a, w), w', s')
  pass1 m = RWS.Lazy.RWST $ \r s -> pass $ do
    ~((a, f), w', s') <- RWS.Lazy.runRWST m r s
    return ((a, w', s'), f)

instance Monoid w' => MonadWriter1 (RWS.Strict.RWST r w' s) where
  listen1 m = RWS.Strict.RWST $ \r s -> do
    ((a, w', s'), w) <- listen (RWS.Strict.runRWST m r s)
    return ((a, w), w', s')
  pass1 m = RWS.Strict.RWST $ \r s -> pass $ do
    ((a, f), w', s') <- RWS.Strict.runRWST m r s
    return ((a, w', s'), f)

instance Monoid w' => MonadWriter1 (W.Lazy.WriterT w') where
  listen1 m = W.Lazy.WriterT $ do
    ~((a, w'), w) <- listen (W.Lazy.runWriterT m)
    return ((a, w), w')
  pass1 m = W.Lazy.WriterT $ pass $ do
    ~((a, f), w') <- W.Lazy.runWriterT m
    return ((a, w'), f)

instance Monoid w' => MonadWriter1 (W.Strict.WriterT w') where
  listen1 m = W.Strict.WriterT $ do
    ((a, w'), w) <- listen (W.Strict.runWriterT m)
    return ((a, w), w')
  pass1 m = W.Strict.WriterT $ pass $ do
    ((a, f), w') <- W.Strict.runWriterT m
    return ((a, w'), f)
