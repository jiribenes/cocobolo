{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-} -- used for (~) constraint only

-- | This module is copied from Lily: https://github.com/jiribenes/lily
module Control.Monad.Fresh
    ( FreshState
    , initialFreshState
    , MonadFresh
    , fresh
    , setFresh
    , getFresh
    , FreshT
    , evalFreshT
    , runFreshT
    , Fresh
    , evalFresh
    , runFresh
    ) where

import           Control.Applicative
import           Control.Lens
import qualified Control.Lens.NonEmpty         as NE
import           Control.Monad.Except
import           Control.Monad.RWS
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy      as LazyS
import qualified Control.Monad.State.Strict    as StrictS
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.Kind                      ( Type )
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T

newtype FreshState n = FreshState { unFresh :: NE.NonEmpty n }

-- makes a lens `_fresh` for getting the contents of `FreshState`
makeLensesFor [("unFresh", "_fresh")] ''FreshState

-- | An initial 'FreshState' of @a@s.
-- Takes a @$prefix@ as a 'Text' and a function from 'Text' to your type @a@
-- and produces an infinite stream of @a@s of the form @$prefix$N@ where @$N@ is from 0 onwards.
initialFreshState :: T.Text -> (T.Text -> a) -> FreshState a
initialFreshState prefix f = FreshState
    { unFresh = f . (prefix <>) . T.pack . show @Int <$> NE.iterate (+ 1) 0
    }

-- | Monad transformer for 'MonadFresh'
newtype FreshT n (m :: Type -> Type) a = FreshT { unFreshT :: StrictS.StateT (FreshState n) m a }
    deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadPlus, Alternative, MonadReader r, MonadState (FreshState n), MonadWriter w, MonadIO)

evalFreshT :: Monad m => FreshT n m a -> FreshState n -> m a
evalFreshT = StrictS.evalStateT . unFreshT
{-# INLINE evalFreshT #-}

runFreshT :: FreshT n m a -> FreshState n -> m (a, FreshState n)
runFreshT = StrictS.runStateT . unFreshT
{-# INLINE runFreshT #-}

-- | Monad for getting a supply of fresh identifiers `n`
-- | Should support `get-set`, `set-set` and `set-get` laws like lenses (!)
class (Monad m) => MonadFresh n m where
  -- | Get a fresh identifier
  fresh :: m n

  -- | Set a new supply
  setFresh :: FreshState n -> m ()

  -- | Get the current supply
  getFresh :: m (FreshState n)

  -- technique from https://lexi-lambda.github.io/blog/2017/04/28/lifts-for-free-making-mtl-typeclasses-derivable/ 
  -- | `fresh` for automatically deriving transformers
  default fresh :: (MonadTrans t, MonadFresh n1 m1, m ~ t m1, n ~ n1) => m n
  fresh = lift fresh
  {-# INLINE fresh #-}

  -- | `setFresh` for automatically deriving transformers
  default setFresh :: (MonadTrans t, MonadFresh n1 m1, m ~ t m1, n ~ n1) => FreshState n -> m ()
  setFresh = lift . setFresh
  {-# INLINE setFresh #-}

  -- | `getFresh` for automatically deriving transformers
  default getFresh :: (MonadTrans t, MonadFresh n1 m1, m ~ t m1, n ~ n1) => m (FreshState n)
  getFresh = lift getFresh
  {-# INLINE getFresh #-}

-- the actual implementation
instance Monad m => MonadFresh n (FreshT n m) where
    fresh = FreshT $ do
        _ <- _fresh <%= NE.fromList . NE.tail
        use (_fresh . NE._head)
    {-# INLINE fresh #-}

    setFresh = FreshT . put
    {-# INLINE setFresh #-}

    getFresh = FreshT get
    {-# INLINE getFresh #-}

-- automatically derived transformer boilerplate
instance MonadFresh n m => MonadFresh n (MaybeT m)
instance MonadFresh n m => MonadFresh n (StrictS.StateT s m)
instance MonadFresh n m => MonadFresh n (LazyS.StateT s m)
instance MonadFresh n m => MonadFresh n (ReaderT r m)
instance (Monoid w, MonadFresh n m) => MonadFresh n (WriterT w m)
instance (Monoid w, MonadFresh n m) => MonadFresh n (RWST r w s m)

instance MonadError e m => MonadError e (FreshT n m) where
    throwError = lift . throwError
    {-# INLINE throwError #-}

  -- this function has been fully written by hole-driven programming
    catchError m h = FreshT $ catchError (unFreshT m) (unFreshT . h)
    {-# INLINE catchError #-}

-- | Type synonym for using Fresh only 
type Fresh n = FreshT n Identity

-- | `evalFreshT` specialized for (m ~ Identity)
evalFresh :: Fresh n a -> FreshState n -> a
evalFresh n = runIdentity . evalFreshT n
{-# INLINE evalFresh #-}

-- | `runFreshT` specialized for (m ~ Identity)
runFresh :: Fresh n a -> FreshState n -> (a, FreshState n)
runFresh n = runIdentity . runFreshT n
{-# INLINE runFresh #-}
