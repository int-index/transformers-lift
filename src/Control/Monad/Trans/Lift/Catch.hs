{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Control.Monad.Trans.Lift.Catch
    ( LiftCatch(..)
    , Catch
    , module Control.Monad.Trans.Class
    ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control

import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy      as Lazy
import qualified Control.Monad.Trans.RWS.Strict    as Strict
import qualified Control.Monad.Trans.State.Lazy    as Lazy
import qualified Control.Monad.Trans.State.Strict  as Strict
import qualified Control.Monad.Trans.Writer.Lazy   as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

class MonadTrans t => LiftCatch t where
    type CatchStT t a
    type CatchStT t a = StT t a
    liftCatch :: Monad m => Catch e m (CatchStT t a) -> Catch e (t m) a
    default liftCatch
        :: (MonadTransControl t, CatchStT t a ~ StT t a, Monad m, Monad (t m))
        => Catch e m (CatchStT t a) -> Catch e (t m) a
    liftCatch catch m h = do
        liftWith (\run -> catch (run m) (run . h)) >>= restoreT . return

instance LiftCatch (ExceptT e)
instance LiftCatch IdentityT
instance LiftCatch ListT
instance LiftCatch MaybeT
instance LiftCatch (ReaderT r)
instance Monoid w => LiftCatch (Lazy.RWST r w s)
instance Monoid w => LiftCatch (Strict.RWST r w s)
instance LiftCatch (Lazy.StateT s)
instance LiftCatch (Strict.StateT s)
instance Monoid w => LiftCatch (Lazy.WriterT w)
instance Monoid w => LiftCatch (Strict.WriterT w)
