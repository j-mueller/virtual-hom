{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module VirtualHom.Components where

import Control.Lens hiding (children)

import VirtualHom.Internal.Element
import VirtualHom.Internal.FFI (render)
import VirtualHom.Internal.Handler 
import VirtualHom.Internal.Rendering (RenderingOptions, actionHandler, prepare)
import VirtualHom.Html (div)

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import Data.Monoid
import Prelude hiding (div)

-- Components.
-- Components can have an internal state and an external state, just like 
-- React components have state and props.
-- 

-- | Component with external state `p` (for Props)
newtype Component a = Component { _getComponent :: a -> [Elem (Handler (a -> (a, Component a)) ()) ()] }

makeLenses ''Component

instance Monoid (Component a) where
  mempty = Component $ const mempty
  mappend l r = component (l, r) cmb where 
    cmb = (<>) <$> subComponent _2 (state._1) <*> subComponent _2 (state._2)

-- | Create a component with internal state `s` and external state `p`
component :: s -> ((s, p) -> [Elem (Handler ((s, p) -> (s, p)) ()) ()]) -> Component p
component initialState f = Component $ \p -> fmap (mapCallbacks (mapUpdates mp)) $ f (initialState, p) where
  mp cb p = fmap (\(s', p') -> (p', component s' f)) cb (initialState, p)

-- | Create a component with external state `p`
component' :: (p -> [Elem (Handler (p -> p) ()) ()]) -> Component p
component' f = Component $ \p -> fmap (mapCallbacks (mapUpdates mp)) $ f p where
  mp cb p = fmap (\p' -> (p', component' f)) cb p

-- | Change the type of a component using a lens.
specialise :: Lens' a b -> Component b -> Component a
specialise l c = Component rnd' where
  rnd' a = fmap (mapCallbacks $ mapUpdates inner) $ view getComponent c $ view l a
  inner cb a = fmap (\(b, compB) -> (a & l .~ b, specialise l compB)) cb $ view l a

-- | Use a sub-component that is part of the state and modifies the state
subComponent :: Lens' a b -> Lens' a (Component b) -> a -> [Elem (Handler (a -> a) ()) ()]
subComponent bLens compLens a = fmap (mapCallbacks $ mapUpdates inner) elms' where
  (Component rnd) = a^.compLens
  elms' = rnd (a^.bLens) 
  inner cb a = fmap (\(b, compB) -> (a & bLens .~ b & compLens .~ compB)) cb (a^.bLens)

-- | Use a sub-component that is part of the state
subComponent' :: Lens' a (Component ()) -> a -> [Elem (Handler (a -> a) ()) ()]
subComponent' = subComponent united

-- | Show a component only if a prism gets a value
on :: Prism' a b -> Component b -> Component a
on p v = Component $ \a -> withPrism p $ \_ from ->
  either (const []) (\b -> fmap (mapCallbacks (mapUpdates $ inner b)) $ view getComponent v $ b) $ from a where
  inner b cb a = fmap (\(b', compB') -> (a & p .~ b', on p compB')) cb b

-- Render a `Component p` , given an initial state `p`
renderComponent' ::
  RenderingOptions ->
  Component a ->
  TQueue (a -> (a, Component a)) ->
  a ->
  IO ()
renderComponent' opts comp queue props = do
  -- render the current view
  let (actions, opts') = prepare opts $ div & children .~ (view getComponent comp $ props)
  let ioActions = fmap (fmap (\a -> do { forkIO a; return () })) $ fmap (fmap $ runHandler $ atomically . writeTQueue queue) actions
  let callback = opts'^.actionHandler
  _ <- render callback ioActions
  (newProps, nextComponent) <- (atomically $ readTQueue queue) >>= (\f -> return $ f props)
  renderComponent' opts' nextComponent queue newProps

renderComponent ::
  RenderingOptions ->
  Component a ->
  a ->
  IO ()
renderComponent opts comp props = do
  q <- newTQueueIO
  renderComponent' opts comp q props

-- | Lens for the props of a component
props :: Lens' (a, b) b
props = _2

-- | Lens for the state of a component
state :: Lens' (a, b) a
state = _1