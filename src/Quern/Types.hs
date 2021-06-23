{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleInstances, MultiParamTypeClasses #-}
{-# Language RankNTypes #-}

module Quern.Types
  (
  -- * The basic type wrapping @ReaderT env IO@, and simple operations within it
    Quern(..)
  , runQuern
  , quernRun
  , mutPut
  , mutGet
  , mutModify
  , immutably
  , ident
  -- * MTL re-exports
  , MonadReader
  , ask
  , local
  , reader
  , MonadIO
  , liftIO
  , MonadState
  , get
  , put
  -- * Lens re-exports
  , Magnify
  , Magnified
  , magnify
  , LensLike'
  , Lens'
  , Lens
  , lens
  ) where

-- The ReaderT env IO pattern

import Control.Lens (magnify, Magnify, LensLike', Lens', Lens, lens, Magnified, (^.))
import Control.Lens.Internal.Zoom (Effect)
--import Lens.Micro.Platform
--import Lens.Micro.Mtl.Internal
import Control.Monad.IO.Class
import Control.Monad.Reader
--import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.State
import Data.IORef

-- | A simple environment-carrying computation floating on `IO`
newtype Quern env a = Quern { unQuern :: ReaderT env IO a }
  deriving (Functor, Applicative, Monad, MonadReader env, MonadIO)

-- | Execute a @Quern@ computation
runQuern :: Quern env a -> env -> IO a
runQuern (Quern k) = runReaderT k

-- | Flipped version of `runQuern`
quernRun :: env -> Quern env a -> IO a
quernRun = flip runQuern

type instance Magnified (Quern env) = Effect IO

-- | An instance for `Magnify` allows execution of a Quern computation
-- using only a subset of the environment:
--
-- >>> quernRun ("README.md", False) $ magnify _1 (ask >>= liftIO . readFile)
instance Magnify (Quern b) (Quern a) b a where
  magnify l (Quern r) = Quern $ magnify l r


--  A partial StateT-like interface for refs in environments

-- | Puts a value into an `IORef` carried by the environment
mutPut :: Lens' env (IORef a) -> a -> Quern env ()
mutPut l a = ask >>= \env -> liftIO (writeIORef (env ^. l) a)

-- | Gets a value from an `IORef` carried by the environment
mutGet :: Lens' env (IORef a) -> Quern env a
mutGet l = ask >>= \env -> liftIO (readIORef (env ^. l))

-- | Strictly mutates the contents of an `IORef` carried by the environment
mutModify :: Lens' env (IORef a) -> (a -> a) -> Quern env ()
mutModify l f = ask >>= \env -> liftIO (modifyIORef' (env ^. l) f)

-- | A very simple environment containing only an `IORef` can implement `MonadState` directly
--
-- Combines pleasantly with `magnify` to offer a locally stateful scope
instance MonadState a (Quern (IORef a)) where
  get = mutGet id
  put = mutPut id

-- | Run a `MonadReader`-like `Quern` computation using some environment
-- stored within an `IORef`
immutably :: Lens' s (IORef env) -> Quern env a -> Quern s a
immutably l inner = mutGet l >>= \e -> liftIO (quernRun e inner)

-- | Surely this trivial lens exists elsewhere, but here it is again
ident :: Lens' a a
ident = lens id const
