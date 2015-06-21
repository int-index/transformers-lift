{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
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
    default liftListen
        :: (MonadTransControl t, ListenStT t a ~ StT t a, Monad m, Monad (t m))
        => Listen w m (ListenStT t a) -> Listen w (t m) a
    liftListen listen m = do
        (st, w) <- liftWith (\run -> listen (run m))
        flip (,) w `fmap` restoreT (return st)

instance LiftListen (E.ExceptT e)
instance LiftListen I.IdentityT
instance LiftListen M.MaybeT
instance LiftListen (R.ReaderT r)
instance LiftListen (S.Lazy.StateT s)
instance LiftListen (S.Strict.StateT s)
instance Monoid w => LiftListen (RWS.Lazy.RWST r w s) where
instance Monoid w => LiftListen (RWS.Strict.RWST r w s) where
instance Monoid w => LiftListen (W.Lazy.WriterT w)
instance Monoid w => LiftListen (W.Strict.WriterT w)
