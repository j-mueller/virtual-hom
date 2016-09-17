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

generalise :: Functor m => Component m p -> Lens' q p -> Component m q
generalise comp lns = Component rnd' where
  rnd' q = fmap (mapCallbacks transf) $ (view getComponent comp) (q^.lns)
  transf cb q = fmap (\(p', comp') -> (q & lns .~ p', generalise comp' lns)) $ cb $ view lns q

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
  rndLft  = mapResult (\c' -> times3 c'  m  rght f) $ view getComponent lft
  rndM    = mapResult (\c' -> times3 lft c' rght f) $ view getComponent m
  rndRght = mapResult (\c' -> times3 lft m  c'   f) $ view getComponent rght



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