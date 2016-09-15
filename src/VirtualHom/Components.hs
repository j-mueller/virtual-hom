{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
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

within :: Functor m => Component m p -> ((p -> [Elem (p -> m (p, Component m p)) ()]) -> Component m p) -> Component m p
within comp f = f rnd where
  rnd = fmap (fmap (mapCallbacks transf)) $ view getComponent comp
  transf cb = fmap (fmap (\(p', c') -> (p', within c' f))) cb

-- | Render a `Component p m` , given an initial state `p`
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
  let callback = opts^.actionHandler
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