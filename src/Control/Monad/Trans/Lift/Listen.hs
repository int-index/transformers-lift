{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
-- | Lifting the @listen@ operation.
module Control.Monad.Trans.Lift.Listen
    ( LiftListen(..)
    , Listen
    , defaultLiftListen
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

#if MIN_VERSION_transformers(0,5,3)
import qualified Control.Monad.Trans.Accum               as Acc
#endif

import Control.Monad.Trans.Lift.StT

-- | The class of monad transformers capable of lifting 'listen'.
class MonadTrans t => LiftListen t where
    -- | Lift the @listen@ operation.
    -- Should satisfy the uniformity property
    --
    -- * @'lift' . 'liftListen' = 'liftListen' . 'lift'@
    --
    liftListen :: Monad m => Listen w m (StT t a) -> Listen w (t m) a

-- | Default definition for the `liftListen` method.
defaultLiftListen
    :: (Monad m, LiftListen n)
    => (forall x . n m x -> t m x)
    -- ^ Monad constructor
    -> (forall o x . t o x -> n o x)
    -- ^ Monad deconstructor
    -> Listen w m (StT n a) -> Listen w (t m) a
defaultLiftListen t unT listen m = t $ liftListen listen (unT m)

instance LiftListen (E.ExceptT e) where
    liftListen = E.liftListen
    {-# INLINE liftListen #-}

instance LiftListen I.IdentityT where
    liftListen = I.mapIdentityT
    {-# INLINE liftListen #-}

instance LiftListen M.MaybeT where
    liftListen = M.liftListen
    {-# INLINE liftListen #-}

instance LiftListen (R.ReaderT r) where
    liftListen = R.mapReaderT
    {-# INLINE liftListen #-}

instance LiftListen (S.Lazy.StateT s) where
    liftListen = S.Lazy.liftListen
    {-# INLINE liftListen #-}

instance LiftListen (S.Strict.StateT s) where
    liftListen = S.Strict.liftListen
    {-# INLINE liftListen #-}

instance Monoid w' => LiftListen (RWS.Lazy.RWST r w' s) where
    liftListen listen m = RWS.Lazy.RWST $ \r s -> do
        ~((a, w', s'), w) <- listen (RWS.Lazy.runRWST m r s)
        return ((a, w), w', s')
    {-# INLINE liftListen #-}

instance Monoid w' => LiftListen (RWS.Strict.RWST r w' s) where
    liftListen listen m = RWS.Strict.RWST $ \r s -> do
        ((a, w', s'), w) <- listen (RWS.Strict.runRWST m r s)
        return ((a, w), w', s')
    {-# INLINE liftListen #-}

instance Monoid w' => LiftListen (RWS.CPS.RWST r w' s) where
    liftListen listen m = RWS.CPS.RWST $ \r w_ s -> do
        ((a, w', s'), w) <- listen (RWS.CPS.unRWST m r w_ s)
        return ((a, w), w', s')
    {-# INLINE liftListen #-}

instance Monoid w' => LiftListen (W.Lazy.WriterT w') where
    liftListen listen m = W.Lazy.WriterT $ do
        ~((a, w'), w) <- listen (W.Lazy.runWriterT m)
        return ((a, w), w')
    {-# INLINE liftListen #-}

instance Monoid w' => LiftListen (W.Strict.WriterT w') where
    liftListen listen m = W.Strict.WriterT $ do
        ((a, w'), w) <- listen (W.Strict.runWriterT m)
        return ((a, w), w')
    {-# INLINE liftListen #-}

instance Monoid w' => LiftListen (W.CPS.WriterT w') where
    liftListen listen m = W.CPS.WriterT $ \w_ -> do
        ((a, w'), w) <- listen (W.CPS.unWriterT m w_)
        return ((a, w), w')
    {-# INLINE liftListen #-}

#if MIN_VERSION_transformers(0,5,3)
instance Monoid w' => LiftListen (Acc.AccumT w') where
    liftListen = Acc.liftListen
    {-# INLINE liftListen #-}
#endif
