{-# LANGUAGE CPP #-}
-- | Lifted 'MonadError'.
module Control.Monad.Except.Extras
  ( MonadError1 (..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Control.Monad.Trans.Class
import Control.Monad.Except

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

-- | Lifted 'MonadError'.
class MonadTrans t => MonadError1 t where
  -- | Lifted 'catchError'.
  catchError1 :: MonadError e m => t m a -> (e -> t m a) -> t m a

instance MonadError1 (E.ExceptT e) where
  catchError1 m h = E.ExceptT $ catchError (E.runExceptT m) (E.runExceptT . h)

instance MonadError1 I.IdentityT where
  catchError1 = I.liftCatch catchError

instance MonadError1 L.ListT where
  catchError1 = L.liftCatch catchError

instance MonadError1 M.MaybeT where
  catchError1 = M.liftCatch catchError

instance MonadError1 (R.ReaderT r) where
  catchError1 = R.liftCatch catchError

instance Monoid w => MonadError1 (RWS.Lazy.RWST r w s) where
  catchError1 = RWS.Lazy.liftCatch catchError

instance Monoid w => MonadError1 (RWS.Strict.RWST r w s) where
  catchError1 = RWS.Strict.liftCatch catchError

instance MonadError1 (S.Lazy.StateT s) where
  catchError1 = S.Lazy.liftCatch catchError

instance MonadError1 (S.Strict.StateT s) where
  catchError1 = S.Strict.liftCatch catchError

instance Monoid w => MonadError1 (W.Lazy.WriterT w) where
  catchError1 = W.Lazy.liftCatch catchError

instance Monoid w => MonadError1 (W.Strict.WriterT w) where
  catchError1 = W.Strict.liftCatch catchError
