{-# LANGUAGE CPP #-}
-- | Lifted 'MonadCont'.
module Control.Monad.Cont.Extras
  ( MonadCont1(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Control.Monad.Trans.Class
import Control.Monad.Cont

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

-- | Lifted 'MonadCont'.
class MonadTrans t => MonadCont1 t where
  -- | Lifted 'callCC'.
  callCC1 :: MonadCont m => ((a -> t m b) -> t m a) -> t m a

instance MonadCont1 (E.ExceptT e) where
  callCC1 = E.liftCallCC callCC

instance MonadCont1 I.IdentityT where
  callCC1 = I.liftCallCC callCC

instance MonadCont1 L.ListT where
  callCC1 = L.liftCallCC callCC

instance MonadCont1 M.MaybeT where
  callCC1 = M.liftCallCC callCC

instance MonadCont1 (R.ReaderT r) where
  callCC1 = R.liftCallCC callCC

instance Monoid w => MonadCont1 (W.Lazy.WriterT w) where
  callCC1 = W.Lazy.liftCallCC callCC

instance Monoid w => MonadCont1 (W.Strict.WriterT w) where
  callCC1 = W.Strict.liftCallCC callCC

instance Monoid w => MonadCont1 (RWS.Lazy.RWST r w s) where
  callCC1 = RWS.Lazy.liftCallCC callCC

instance Monoid w => MonadCont1 (RWS.Strict.RWST r w s) where
  callCC1 = RWS.Strict.liftCallCC callCC

instance MonadCont1 (S.Lazy.StateT s) where
  callCC1 = S.Lazy.liftCallCC callCC

instance MonadCont1 (S.Strict.StateT s) where
  callCC1 = S.Strict.liftCallCC callCC

