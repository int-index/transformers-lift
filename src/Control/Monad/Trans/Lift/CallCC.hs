{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Trans.Lift.CallCC
    ( LiftCallCC(..)
    , CallCC
    , module Control.Monad.Trans.Class
    ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control

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

class MonadTrans t => LiftCallCC t where
    type CallCCStT t a
    type CallCCStT t a = StT t a
    liftCallCC  :: Monad m => CallCC m (CallCCStT t a) (CallCCStT t b) -> CallCC (t m) a b
    liftCallCC' :: Monad m => CallCC m (CallCCStT t a) (CallCCStT t b) -> CallCC (t m) a b
    liftCallCC' = liftCallCC

instance LiftCallCC (E.ExceptT e) where
    liftCallCC = E.liftCallCC

instance LiftCallCC I.IdentityT where
    liftCallCC = I.liftCallCC

instance LiftCallCC L.ListT where
    liftCallCC = L.liftCallCC

instance LiftCallCC M.MaybeT where
    liftCallCC = M.liftCallCC

instance LiftCallCC (R.ReaderT r) where
    liftCallCC = R.liftCallCC

instance Monoid w => LiftCallCC (RWS.Lazy.RWST r w s) where
    liftCallCC  = RWS.Lazy.liftCallCC
    liftCallCC' = RWS.Lazy.liftCallCC'

instance Monoid w => LiftCallCC (RWS.Strict.RWST r w s) where
    liftCallCC  = RWS.Strict.liftCallCC
    liftCallCC' = RWS.Strict.liftCallCC'

instance LiftCallCC (S.Lazy.StateT s) where
    liftCallCC  = S.Lazy.liftCallCC
    liftCallCC' = S.Lazy.liftCallCC'

instance LiftCallCC (S.Strict.StateT s) where
    liftCallCC  = S.Strict.liftCallCC
    liftCallCC' = S.Strict.liftCallCC'

instance Monoid w => LiftCallCC (W.Lazy.WriterT w) where
    liftCallCC = W.Lazy.liftCallCC

instance Monoid w => LiftCallCC (W.Strict.WriterT w) where
    liftCallCC = W.Strict.liftCallCC
