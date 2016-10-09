{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module VirtualHom.Internal.Handler where

import Control.Lens
import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Monoid

data HandlerF s a = 
  UpdateState s a
  | RunIO (IO a)
  deriving Functor

-- | Conceptually, a `Handler s a` is a background thread that can make
-- updates to a state `s`. `s` is usually an arrow `a -> a` so the updates
-- are actually modifications. 
--
-- Handlers will run inside `forkIO`, so as soon as they reach a blocking IO
-- action (eg. waiting for an XmlHttpRequest or a timer), the GHCJS runtime
-- will suspend them and return control to the main thread. This means that 
-- you can use blocking IO inside a `Handler`, and don't have to deal with
-- nested callbacks.  
newtype Handler s a = Handler { getHandler :: Free (HandlerF s) a }
  deriving (Functor, Applicative, Monad)

instance Bifunctor Handler where
  first = mapUpdates
  second = fmap

instance MonadIO (Handler s) where
  liftIO = Handler . Free . RunIO . fmap return

-- | Update the state
update :: s -> Handler s ()
update s = Handler $ Free $ UpdateState s (Pure ())

-- | Run a handler in the IO monad, given an interpreter for 
-- state updates
runHandler :: (s -> IO ()) -> Handler s a -> IO a
runHandler i (Handler f) = iterM go f where
  go h = case h of
    UpdateState e f' -> (i e) >> f'
    RunIO io -> join io

-- | Transform the updates in a handler
mapUpdates :: (s -> t) -> Handler s a -> Handler t a
mapUpdates f = Handler . hoistFree transf . getHandler where
  transf ff = case ff of
    RunIO a -> RunIO a
    UpdateState e cont -> UpdateState (f e) cont
