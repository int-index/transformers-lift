{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
-- | Lifting the @catch@ operation.
module Control.Monad.Trans.Lift.Catch
    ( LiftCatch(..)
    , Catch
    , defaultLiftCatch
    , module Control.Monad.Trans.Class
    ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Control.Monad.Signatures
import Control.Monad.Trans.Class

import qualified Control.Monad.Trans.Except        as E
import qualified Control.Monad.Trans.Identity      as I
import qualified Control.Monad.Trans.List          as L
import qualified Control.Monad.Trans.Maybe         as M
import qualified Control.Monad.Trans.Reader        as R
import qualified Control.Monad.Trans.RWS.Lazy      as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict    as RWS.Strict
import qualified "writer-cps-transformers" Control.Monad.Trans.RWS.CPS as RWS.CPS
import qualified Control.Monad.Trans.State.Lazy    as S.Lazy
import qualified Control.Monad.Trans.State.Strict  as S.Strict
import qualified Control.Monad.Trans.Writer.Lazy   as W.Lazy
import qualified Control.Monad.Trans.Writer.Strict as W.Strict
import qualified "writer-cps-transformers" Control.Monad.Trans.Writer.CPS as W.CPS

#if MIN_VERSION_transformers(0,5,3)
import qualified Control.Monad.Trans.Accum         as Acc
#endif

import Control.Monad.Trans.Lift.StT

-- | The class of monad transformers capable of lifting 'catch'.
class MonadTrans t => LiftCatch t where
    -- | Lift the @catch@ operation.
    -- Should satisfy the uniformity property
    --
    -- * @'lift' (cf m f) = 'liftCatch' ('lift' . cf) ('lift' f)@
    --
    liftCatch :: Monad m => Catch e m (StT t a) -> Catch e (t m) a

-- | Default definition for the 'liftCatch' method.
defaultLiftCatch
    :: (Monad m, LiftCatch n)
    => (forall x . n m x -> t m x)
    -- ^ Monad constructor
    -> (forall o x . t o x -> n o x)
    -- ^ Monad deconstructor
    -> Catch e m (StT n a) -> Catch e (t m) a
defaultLiftCatch t unT f m h = t $ liftCatch f (unT m) (unT . h)

instance LiftCatch (E.ExceptT e) where
    liftCatch f m h = E.ExceptT $ f (E.runExceptT m) (E.runExceptT . h)
    {-# INLINE liftCatch #-}

instance LiftCatch I.IdentityT where
    liftCatch = I.liftCatch
    {-# INLINE liftCatch #-}

instance LiftCatch L.ListT where
    liftCatch = L.liftCatch
    {-# INLINE liftCatch #-}

instance LiftCatch M.MaybeT where
    liftCatch = M.liftCatch
    {-# INLINE liftCatch #-}

instance LiftCatch (R.ReaderT r) where
    liftCatch = R.liftCatch
    {-# INLINE liftCatch #-}

instance Monoid w => LiftCatch (RWS.Lazy.RWST r w s) where
    liftCatch = RWS.Lazy.liftCatch
    {-# INLINE liftCatch #-}

instance Monoid w => LiftCatch (RWS.Strict.RWST r w s) where
    liftCatch = RWS.Strict.liftCatch
    {-# INLINE liftCatch #-}

instance Monoid w => LiftCatch (RWS.CPS.RWST r w s) where
    liftCatch = RWS.CPS.liftCatch
    {-# INLINE liftCatch #-}

instance LiftCatch (S.Lazy.StateT s) where
    liftCatch = S.Lazy.liftCatch
    {-# INLINE liftCatch #-}

instance LiftCatch (S.Strict.StateT s) where
    liftCatch = S.Strict.liftCatch
    {-# INLINE liftCatch #-}

instance Monoid w => LiftCatch (W.Lazy.WriterT w) where
    liftCatch = W.Lazy.liftCatch
    {-# INLINE liftCatch #-}

instance Monoid w => LiftCatch (W.Strict.WriterT w) where
    liftCatch = W.Strict.liftCatch
    {-# INLINE liftCatch #-}

instance Monoid w => LiftCatch (W.CPS.WriterT w) where
    liftCatch = W.CPS.liftCatch
    {-# INLINE liftCatch #-}

#if MIN_VERSION_transformers(0,5,3)
instance Monoid w => LiftCatch (Acc.AccumT w) where
    liftCatch = Acc.liftCatch
    {-# INLINE liftCatch #-}
#endif
