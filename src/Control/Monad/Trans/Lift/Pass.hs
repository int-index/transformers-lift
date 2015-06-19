{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Trans.Lift.Pass
    ( LiftPass(..)
    , Pass
    , module Control.Monad.Trans.Class
    ) where

import Data.Monoid

import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control

import qualified Control.Monad.Trans.Except        as E
import qualified Control.Monad.Trans.Identity      as I
import qualified Control.Monad.Trans.Maybe         as M
import qualified Control.Monad.Trans.Reader        as R
import qualified Control.Monad.Trans.Cont          as C
import qualified Control.Monad.Trans.RWS.Lazy      as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict    as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy    as S.Lazy
import qualified Control.Monad.Trans.State.Strict  as S.Strict
import qualified Control.Monad.Trans.Writer.Lazy   as W.Lazy
import qualified Control.Monad.Trans.Writer.Strict as W.Strict

class MonadTrans t => LiftPass t where
    type PassStT t a
    type PassStT t a = StT t a
    liftPass :: Monad m => Pass w m (PassStT t a) -> Pass w (t m) a
    -- TODO: investigate how MonadTransControl can be used for default definition

instance LiftPass (E.ExceptT e) where
    liftPass = E.liftPass

instance LiftPass I.IdentityT where
    liftPass = I.mapIdentityT

instance LiftPass M.MaybeT where
    liftPass = M.liftPass

instance LiftPass (R.ReaderT r) where
    liftPass = R.mapReaderT

instance LiftPass (S.Lazy.StateT s) where
    liftPass = S.Lazy.liftPass

instance LiftPass (S.Strict.StateT s) where
    liftPass = S.Strict.liftPass

instance Monoid w' => LiftPass (W.Lazy.WriterT w') where
    liftPass pass m = W.Lazy.WriterT $ pass $ do
        ~((a, f), w) <- W.Lazy.runWriterT m
        return ((a, w), f)

instance Monoid w' => LiftPass (W.Strict.WriterT w') where
    liftPass pass m = W.Strict.WriterT $ pass $ do
        ((a, f), w) <- W.Strict.runWriterT m
        return ((a, w), f)
