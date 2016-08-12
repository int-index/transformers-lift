{-# LANGUAGE CPP #-}
-- | Lifted 'MonadReader'.
module Control.Monad.Reader.Extras
  ( MonadReader1(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Control.Monad.Trans.Class
import Control.Monad.Reader

import qualified Control.Monad.Trans.Cont          as C
import qualified Control.Monad.Trans.Except        as E
import qualified Control.Monad.Trans.Identity      as I
import qualified Control.Monad.Trans.List          as L
import qualified Control.Monad.Trans.Maybe         as M
import qualified Control.Monad.Trans.Reader        as R
import qualified Control.Monad.Trans.RWS.Lazy      as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict    as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy    as S.Lazy
import qualified Control.Monad.Trans.State.Strict  as S.Strict
import qualified Control.Monad.Trans.Writer.Lazy   as W.Lazy
import qualified Control.Monad.Trans.Writer.Strict as W.Strict

-- | Lifted 'MonadReader'.
class MonadTrans t => MonadReader1 t where
  -- | Lifted 'local'.
  local1 :: MonadReader r m => (r -> r) -> t m a -> t m a

instance MonadReader1 (C.ContT r) where
  local1 f = C.liftLocal ask local f

instance MonadReader1 (E.ExceptT e) where
  local1 f = E.mapExceptT (local f)

instance MonadReader1 I.IdentityT where
  local1 f = I.mapIdentityT (local f)

instance MonadReader1 L.ListT where
  local1 f = L.mapListT (local f)

instance MonadReader1 M.MaybeT where
  local1 f = M.mapMaybeT (local f)

instance MonadReader1 (R.ReaderT r) where
  local1 f = R.mapReaderT (local f)

instance Monoid w => MonadReader1 (RWS.Lazy.RWST r w s) where
  local1 f = RWS.Lazy.mapRWST (local f)

instance Monoid w => MonadReader1 (RWS.Strict.RWST r w s) where
  local1 f = RWS.Strict.mapRWST (local f)

instance MonadReader1 (S.Lazy.StateT s) where
  local1 f = S.Lazy.mapStateT (local f)

instance MonadReader1 (S.Strict.StateT s) where
  local1 f = S.Strict.mapStateT (local f)

instance Monoid w => MonadReader1 (W.Lazy.WriterT w) where
  local1 f = W.Lazy.mapWriterT (local f)

instance Monoid w => MonadReader1 (W.Strict.WriterT w) where
  local1 f = W.Strict.mapWriterT (local f)
