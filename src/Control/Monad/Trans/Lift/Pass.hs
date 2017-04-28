{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
-- | Lifting the 'pass' operation.
module Control.Monad.Trans.Lift.Pass
    ( LiftPass(..)
    , Pass
    , defaultLiftPass
    , module Control.Monad.Trans.Class
    ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Control.Monad.Signatures
import Control.Monad.Trans.Class

import qualified Control.Monad.Trans.Except              as E
import qualified Control.Monad.Trans.Identity            as I
import qualified Control.Monad.Trans.Maybe               as M
import qualified Control.Monad.Trans.Reader              as R
import qualified Control.Monad.Trans.RWS.Lazy            as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict          as RWS.Strict
import qualified Control.Monad.Trans.RWS.CPS.Internal    as RWS.CPS
import qualified Control.Monad.Trans.State.Lazy          as S.Lazy
import qualified Control.Monad.Trans.State.Strict        as S.Strict
import qualified Control.Monad.Trans.Writer.Lazy         as W.Lazy
import qualified Control.Monad.Trans.Writer.Strict       as W.Strict
import qualified Control.Monad.Trans.Writer.CPS.Internal as W.CPS
import qualified Control.Monad.Trans.Accum               as Acc

import Control.Monad.Trans.Lift.StT

-- | The class of monad transformers capable of lifting 'pass'.
class MonadTrans t => LiftPass t where
    -- | Lift the 'pass' operation.
    -- Should satisfy the uniformity property
    --
    -- * @'lift' . 'liftPass' = 'liftPass' . 'lift'@
    --
    liftPass :: Monad m => Pass w m (StT t a) -> Pass w (t m) a

-- | Default definition for the 'liftPass' method.
defaultLiftPass
    :: (Monad m, LiftPass n)
    => (forall x . n m x -> t m x)
    -- ^ Monad constructor
    -> (forall o x . t o x -> n o x)
    -- ^ Monad deconstructor
    -> Pass w m (StT n a)
    -> Pass w (t m) a
defaultLiftPass t unT pass m = t $ liftPass pass (unT m)

instance LiftPass (E.ExceptT e) where
    liftPass = E.liftPass
    {-# INLINE liftPass #-}

instance LiftPass I.IdentityT where
    liftPass = I.mapIdentityT
    {-# INLINE liftPass #-}

instance LiftPass M.MaybeT where
    liftPass = M.liftPass
    {-# INLINE liftPass #-}

instance LiftPass (R.ReaderT r) where
    liftPass = R.mapReaderT
    {-# INLINE liftPass #-}

instance LiftPass (S.Lazy.StateT s) where
    liftPass = S.Lazy.liftPass
    {-# INLINE liftPass #-}

instance LiftPass (S.Strict.StateT s) where
    liftPass = S.Strict.liftPass
    {-# INLINE liftPass #-}

instance Monoid w' => LiftPass (RWS.Lazy.RWST r w' s) where
    liftPass pass m = RWS.Lazy.RWST $ \r s -> pass $ do
        ~((a, f), w', s') <- RWS.Lazy.runRWST m r s
        return ((a, w', s'), f)
    {-# INLINE liftPass #-}

instance Monoid w' => LiftPass (RWS.Strict.RWST r w' s) where
    liftPass pass m = RWS.Strict.RWST $ \r s -> pass $ do
        ((a, f), w', s') <- RWS.Strict.runRWST m r s
        return ((a, w', s'), f)
    {-# INLINE liftPass #-}

instance Monoid w' => LiftPass (RWS.CPS.RWST r w' s) where
    liftPass pass m = RWS.CPS.RWST $ \r w_ s -> pass $ do
        ((a, f), w', s') <- RWS.CPS.unRWST m r w_ s
        return ((a, w', s'), f)
    {-# INLINE liftPass #-}

instance Monoid w' => LiftPass (W.Lazy.WriterT w') where
    liftPass pass m = W.Lazy.WriterT $ pass $ do
        ~((a, f), w') <- W.Lazy.runWriterT m
        return ((a, w'), f)
    {-# INLINE liftPass #-}

instance Monoid w' => LiftPass (W.Strict.WriterT w') where
    liftPass pass m = W.Strict.WriterT $ pass $ do
        ((a, f), w') <- W.Strict.runWriterT m
        return ((a, w'), f)
    {-# INLINE liftPass #-}

instance Monoid w' => LiftPass (W.CPS.WriterT w') where
    liftPass pass m = W.CPS.WriterT $ \w_ -> pass $ do
        ((a, f), w') <- W.CPS.unWriterT m w_
        return ((a, w'), f)
    {-# INLINE liftPass #-}

instance Monoid w' => LiftPass (Acc.AccumT w') where
    liftPass = Acc.liftPass
    {-# INLINE liftPass #-}
