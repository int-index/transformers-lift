{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Trans.Lift.Listen
    ( LiftListen(..)
    , Listen
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
import qualified Control.Monad.Trans.Maybe         as M
import qualified Control.Monad.Trans.Reader        as R
import qualified Control.Monad.Trans.RWS.Lazy      as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict    as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy    as S.Lazy
import qualified Control.Monad.Trans.State.Strict  as S.Strict
import qualified Control.Monad.Trans.Writer.Lazy   as W.Lazy
import qualified Control.Monad.Trans.Writer.Strict as W.Strict

class MonadTrans t => LiftListen t where
    type ListenStT t a
    type ListenStT t a = StT t a
    liftListen :: Monad m => Listen w m (ListenStT t a) -> Listen w (t m) a
    -- TODO: investigate how MonadTransControl can be used for default definition

instance LiftListen (E.ExceptT e) where
    liftListen = E.liftListen

instance LiftListen I.IdentityT where
    liftListen = I.mapIdentityT

instance LiftListen M.MaybeT where
    liftListen = M.liftListen

instance LiftListen (R.ReaderT r) where
    liftListen = R.mapReaderT

instance LiftListen (S.Lazy.StateT s) where
    liftListen = S.Lazy.liftListen

instance LiftListen (S.Strict.StateT s) where
    liftListen = S.Strict.liftListen

instance Monoid w' => LiftListen (RWS.Lazy.RWST r w' s) where
    liftListen = error "TODO"

instance Monoid w' => LiftListen (RWS.Strict.RWST r w' s) where
    liftListen = error "TODO"

instance Monoid w' => LiftListen (W.Lazy.WriterT w') where
    liftListen listen m = W.Lazy.WriterT $ do
        ~((a, w'), w) <- listen (W.Lazy.runWriterT m)
        return ((a, w), w')

instance Monoid w' => LiftListen (W.Strict.WriterT w') where
    liftListen listen m = W.Strict.WriterT $ do
        ((a, w'), w) <- listen (W.Strict.runWriterT m)
        return ((a, w), w')
