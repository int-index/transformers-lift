{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
-- | Lifting the 'local' operation.
module Control.Monad.Trans.Lift.Local
    ( LiftLocal(..)
    , Local
    , defaultLiftLocal
    , module Control.Monad.Trans.Class
    ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Control.Monad.Trans.Class

import qualified Control.Monad.Trans.Cont          as C
import qualified Control.Monad.Trans.Except        as E
import qualified Control.Monad.Trans.Identity      as I
import qualified Control.Monad.Trans.List          as L
import qualified Control.Monad.Trans.Maybe         as M
import qualified Control.Monad.Trans.Reader        as R
import qualified Control.Monad.Trans.RWS.Lazy      as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict    as RWS.Strict
import qualified Control.Monad.Trans.RWS.CPS       as RWS.CPS
import qualified Control.Monad.Trans.State.Lazy    as S.Lazy
import qualified Control.Monad.Trans.State.Strict  as S.Strict
import qualified Control.Monad.Trans.Writer.Lazy   as W.Lazy
import qualified Control.Monad.Trans.Writer.Strict as W.Strict
import qualified Control.Monad.Trans.Writer.CPS    as W.CPS
import qualified Control.Monad.Trans.Accum         as Acc

-- | Signature of the @local@ operation,
-- introduced in "Control.Monad.Trans.Reader".
type Local r m a = (r -> r) -> m a -> m a

-- | The class of monad transformers capable of lifting 'local'.
class MonadTrans t => LiftLocal t where
    -- | Lift the 'local' operation.
    liftLocal :: Monad m => m r -> (forall a . Local r    m  a)
                                -> (forall a . Local r (t m) a)

-- | Default definition for the 'liftLocal' method.
defaultLiftLocal
    :: (Monad m, LiftLocal n)
    => (forall x . n m x -> t m x)
    -- ^ Monad constructor
    -> (forall o x . t o x -> n o x)
    -- ^ Monad deconstructor
    -> m r
    -> (forall a . Local r    m  a)
    -> (forall a . Local r (t m) a)
defaultLiftLocal t unT a l f = t . liftLocal a l f . unT

instance LiftLocal (C.ContT r) where
    liftLocal a l f = C.liftLocal a l f
    {-# INLINE liftLocal #-}

instance LiftLocal (E.ExceptT e) where
    liftLocal _ l f = E.mapExceptT (l f)
    {-# INLINE liftLocal #-}

instance LiftLocal I.IdentityT where
    liftLocal _ l f = I.mapIdentityT (l f)
    {-# INLINE liftLocal #-}

instance LiftLocal L.ListT where
    liftLocal _ l f = L.mapListT (l f)
    {-# INLINE liftLocal #-}

instance LiftLocal M.MaybeT where
    liftLocal _ l f = M.mapMaybeT (l f)
    {-# INLINE liftLocal #-}

instance LiftLocal (R.ReaderT r) where
    liftLocal _ l f = R.mapReaderT (l f)
    {-# INLINE liftLocal #-}

instance Monoid w => LiftLocal (RWS.Lazy.RWST r w s) where
    liftLocal _ l f = RWS.Lazy.mapRWST (l f)
    {-# INLINE liftLocal #-}

instance Monoid w => LiftLocal (RWS.Strict.RWST r w s) where
    liftLocal _ l f = RWS.Strict.mapRWST (l f)
    {-# INLINE liftLocal #-}

instance Monoid w => LiftLocal (RWS.CPS.RWST r w s) where
    liftLocal _ l f = RWS.CPS.mapRWST (l f)
    {-# INLINE liftLocal #-}

instance LiftLocal (S.Lazy.StateT s) where
    liftLocal _ l f = S.Lazy.mapStateT (l f)
    {-# INLINE liftLocal #-}

instance LiftLocal (S.Strict.StateT s) where
    liftLocal _ l f = S.Strict.mapStateT (l f)
    {-# INLINE liftLocal #-}

instance Monoid w => LiftLocal (W.Lazy.WriterT w) where
    liftLocal _ l f = W.Lazy.mapWriterT (l f)
    {-# INLINE liftLocal #-}

instance Monoid w => LiftLocal (W.Strict.WriterT w) where
    liftLocal _ l f = W.Strict.mapWriterT (l f)
    {-# INLINE liftLocal #-}

instance Monoid w => LiftLocal (W.CPS.WriterT w) where
    liftLocal _ l f = W.CPS.mapWriterT (l f)
    {-# INLINE liftLocal #-}

instance Monoid w => LiftLocal (Acc.AccumT w) where
    liftLocal _ l f = Acc.mapAccumT (l f)
    {-# INLINE liftLocal #-}
