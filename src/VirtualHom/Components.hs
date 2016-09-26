{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module VirtualHom.Components where

import Control.Lens hiding (children)
import Control.Monad.Cont

import VirtualHom.Internal.Element
import VirtualHom.Internal.FFI (render)
import VirtualHom.Internal.Rendering (RenderingOptions, actionHandler, prepare)
import VirtualHom.Html (div)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import Prelude hiding (div)

-- Components.
-- Components can have an internal state and an external state, just like 
-- React components have state and props.
-- 

-- | Component with external state `p` (for Props)
newtype Component m a = Component { _getComponent :: a -> [Elem (a -> m (a, Component m a)) ()] }

makeLenses ''Component

-- | Create a component with internal state `s` and external state `p`
component :: Functor m => s -> (s -> p -> [Elem ((s, p) -> m (s, p)) ()]) -> Component m p
component initialState f = Component $ \p -> fmap (mapCallbacks transf) $ f initialState p where
  transf cb p = fmap (\(s', p') -> (p', component s' f)) $ cb (initialState, p) 

type SubComponent m p = p -> [Elem (p -> m (p, Component m p)) ()]

times :: Functor m => 
  Component m p -> 
  Component m p -> 
  (SubComponent m p -> SubComponent m p -> SubComponent m p) -> 
  Component m p
times lft rght f = Component $ f rndLft rndRght where
  mapResult f = fmap (fmap (mapCallbacks (fmap (fmap (fmap f)))))
  rndLft  = mapResult (\c' -> times c' rght f) $ view getComponent lft
  rndRght = mapResult (\c' -> times lft c'  f) $ view getComponent rght

times3 :: Functor m =>
  Component m p ->
  Component m p ->
  Component m p ->
  (SubComponent m p -> SubComponent m p -> SubComponent m p -> SubComponent m p) ->
  Component m p
times3 lft m rght f = Component $ f rndLft rndM rndRght where
  mapResult f = fmap (fmap (mapCallbacks (fmap (fmap (fmap f)))))
  -- TODO: Replace with tuples and lenses ?
  -- TODO: No more newtype for component
  rndLft  = mapResult (\c' -> times3 c'  m  rght f) $ view getComponent lft
  rndM    = mapResult (\c' -> times3 lft c' rght f) $ view getComponent m
  rndRght = mapResult (\c' -> times3 lft m  c'   f) $ view getComponent rght

times4 :: Functor m =>
  Component m p ->
  Component m p ->
  Component m p ->
  Component m p ->
  (SubComponent m p -> SubComponent m p -> SubComponent m p -> SubComponent m p -> SubComponent m p) ->
  Component m p
times4 c1 c2 c3 c4 f = Component $ f rnd1 rnd2 rnd3 rnd4 where
  mapResult f = fmap (fmap (mapCallbacks (fmap (fmap (fmap f)))))
  rnd1 = mapResult (\c1' -> times4 c1' c2 c3 c4 f) $ view getComponent c1
  rnd2 = mapResult (\c2' -> times4 c1 c2' c3 c4 f) $ view getComponent c2
  rnd3 = mapResult (\c3' -> times4 c1 c2 c3' c4 f) $ view getComponent c3
  rnd4 = mapResult (\c4' -> times4 c1 c2 c3 c4' f) $ view getComponent c4

-- | Integrate a component of only part of an application state
specialise :: Functor f => Lens' a b -> Component f b -> Component f a
specialise l c = Component rnd' where
  rnd' a = fmap (mapCallbacks inner) $ view getComponent c $ view l a
  inner cb a = fmap (\(b, compB) -> (a & l .~ b, specialise l compB)) $ cb $ view l a

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