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
import VirtualHom.Internal.Rendering (RenderingOptions, actionHandler, prepare)
import VirtualHom.Html (div)

import Control.Applicative
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import Data.Monoid
import Prelude hiding (div)

-- Components.
-- Components can have an internal state and an external state, just like 
-- React components have state and props.
-- 

-- | Component with external state `p` (for Props)
newtype Component m a = Component { _getComponent :: a -> [Elem (a -> m (a, Component m a)) ()] }

makeLenses ''Component

instance Monoid (Component m a) where
  mempty = Component $ const mempty
  mappend l r = Component $ (<>) <$> (view getComponent l) <*> (view getComponent r)

-- | Create a component with internal state `s` and external state `p`
component :: Functor m => s -> ((s, p) -> [Elem ((s, p) -> m (s, p)) ()]) -> Component m p
component initialState f = Component $ \p -> fmap (mapCallbacks transf) $ f (initialState, p) where
  transf cb p = fmap (\(s', p') -> (p', component s' f)) $ cb (initialState, p) 

-- | Change the type of a component using a lens.
specialise :: Functor f => Lens' a b -> Component f b -> Component f a
specialise l c = Component rnd' where
  rnd' a = fmap (mapCallbacks inner) $ view getComponent c $ view l a
  inner cb a = fmap (\(b, compB) -> (a & l .~ b, specialise l compB)) $ cb $ view l a

-- | Use a sub-component that is part of the state and modifies the state
subComponent :: Functor f => Lens' a b -> Lens' a (Component f b) -> a -> [Elem (a -> f a) ()]
subComponent bLens compLens a = fmap (mapCallbacks inner) elms' where
  (Component rnd) = a^.compLens
  elms' = rnd (a^.bLens) 
  inner cb a = fmap (\(b, compB) -> (a & bLens .~ b & compLens .~ compB)) $ cb (a^.bLens)

-- | Use a sub-component that is part of the state
subComponent' :: Functor f => Lens' a (Component f ()) -> a -> [Elem (a -> f a) ()]
subComponent' = subComponent united

-- | Show a component only if a prism gets a value
on :: Functor f => Prism' a b -> Component f b -> Component f a
on p v = Component $ \a -> withPrism p $ \_ from ->
  either (const []) (\b -> fmap (mapCallbacks (inner b)) $ view getComponent v $ b) $ from a where
  inner b cb a = fmap (\(b', compB') -> (a & p .~ b', on p compB')) $ cb b

-- Render a `Component p m` , given an initial state `p`
renderComponent' :: Functor m =>
  RenderingOptions ->
  Component m a ->
  (forall p. m p -> IO p) ->
  TQueue (a -> m (a, Component m a)) ->
  a ->
  IO ()
renderComponent' opts comp interp queue props = do
  -- render the current view
  let (actions, opts') = prepare opts $ div & children .~ (view getComponent comp $ props)
  let ioActions = fmap (fmap $ atomically . writeTQueue queue) actions
  let callback = opts'^.actionHandler
  _ <- render callback ioActions
  (newProps, nextComponent) <- (atomically $ readTQueue queue) >>= (\f -> interp $ f props)
  renderComponent' opts' nextComponent interp queue newProps

renderComponent :: Functor m =>
  RenderingOptions ->
  Component m a ->
  (forall p. m p -> IO p) ->
  a ->
  IO ()
renderComponent opts comp interp props = do
  q <- newTQueueIO
  renderComponent' opts comp interp q props