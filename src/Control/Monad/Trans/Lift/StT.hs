{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
module Control.Monad.Trans.Lift.StT where

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

type family StT (t :: (* -> *) -> (* -> *)) (a :: *) :: *
type instance StT (E.ExceptT e) a = Either e a
type instance StT I.IdentityT a = a
type instance StT L.ListT a = [a]
type instance StT M.MaybeT a = Maybe a
type instance StT (R.ReaderT r) a = a
type instance StT (RWS.Lazy.RWST r w s) a = (a, s, w)
type instance StT (RWS.Strict.RWST r w s) a = (a, s, w)
type instance StT (S.Lazy.StateT s) a = (a, s)
type instance StT (S.Strict.StateT s) a = (a, s)
type instance StT (W.Strict.WriterT w) a = (a, w)
type instance StT (W.Lazy.WriterT w) a = (a, w)
