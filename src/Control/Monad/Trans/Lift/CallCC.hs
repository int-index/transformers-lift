{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
-- | Lifting the @callCC@ operation.
module Control.Monad.Trans.Lift.CallCC
    ( LiftCallCC(..)
    , CallCC
    , defaultLiftCallCC
    , defaultLiftCallCC'
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
import qualified Control.Monad.Trans.State.Lazy    as S.Lazy
import qualified Control.Monad.Trans.State.Strict  as S.Strict
import qualified Control.Monad.Trans.Writer.Lazy   as W.Lazy
import qualified Control.Monad.Trans.Writer.Strict as W.Strict

import Control.Monad.Trans.Lift.StT

-- | The class of monad transformers capable of lifting 'callCC'.
class MonadTrans t => LiftCallCC t where
    -- | Lift the @callCC@ operation.
    -- Should satisfy the uniformity property
    --
    -- * @'lift' (f k) = f' ('lift' . k) => 'lift' (cf f) = 'liftCallCC' cf f'@
    --
    liftCallCC :: Monad m => CallCC m (StT t a) (StT t b) -> CallCC (t m) a b

    -- | Lift the @callCC@ operation.
    -- This is an alternative version of 'liftCallCC' included for historical
    -- reasons. It has a different lifting behavior for the @StateT@ and @RWST@
    -- monad transformers. Matches what @mtl@ does but doesn't satisfy the
    -- uniformity property.
    liftCallCC' :: Monad m => CallCC m (StT t a) (StT t b) -> CallCC (t m) a b
    liftCallCC' = liftCallCC

-- | Default definition for the 'liftCallCC' method.
defaultLiftCallCC
    :: (Monad m, LiftCallCC n)
    => (forall x . n m x -> t m x)
    -- ^ Monad constructor
    -> (forall o x . t o x -> n o x)
    -- ^ Monad deconstructor
    -> CallCC m (StT n a) (StT n b)
    -> CallCC (t m) a b
defaultLiftCallCC t unT callCC f
    = t $ liftCallCC callCC (\g -> (unT . f) (t . g))

-- | Default definition for the 'liftCallCC'' method.
defaultLiftCallCC'
    :: (Monad m, LiftCallCC n)
    => (forall x . n m x -> t m x)
    -- ^ Monad constructor
    -> (forall o x . t o x -> n o x)
    -- ^ Monad deconstructor
    -> CallCC m (StT n a) (StT n b)
    -> CallCC (t m) a b
defaultLiftCallCC' t unT callCC f
    = t $ liftCallCC' callCC (\g -> (unT . f) (t . g))

instance LiftCallCC (E.ExceptT e) where
    liftCallCC  = E.liftCallCC

instance LiftCallCC I.IdentityT where
    liftCallCC  = I.liftCallCC

instance LiftCallCC L.ListT where
    liftCallCC  = L.liftCallCC

instance LiftCallCC M.MaybeT where
    liftCallCC  = M.liftCallCC

instance LiftCallCC (R.ReaderT r) where
    liftCallCC  = R.liftCallCC

instance Monoid w => LiftCallCC (W.Lazy.WriterT w) where
    liftCallCC  = W.Lazy.liftCallCC

instance Monoid w => LiftCallCC (W.Strict.WriterT w) where
    liftCallCC  = W.Strict.liftCallCC

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

