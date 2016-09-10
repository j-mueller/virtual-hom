{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module VirtualHom.Components where

import Control.Lens hiding (children)
import Control.Monad.Cont

import VirtualHom.Html
import VirtualHom.Internal.Element
import VirtualHom.View

import Prelude hiding (div)

-- type Layout m a = Cont (Elem (a -> m a) ()) ()

-- Components.
--
-- There are two distinct phases in the execution of a webapp:
-- Rendering, and reacting to callbacks.
-- Rendering is realised as a function `State -> Html`.
-- Callbacks (for an event `E`) are realised as functions `E -> Endo State`.
--
-- Both phases have effects:
-- Rendering reads from the state and creates event handlers.
-- Callbacks modify the state.
--
-- Components can have an internal state and an external state, just like 
-- React components have state and props.
-- 

-- | Component with state `s` and inner state `p`.
data Component s p m = Component {
  _state :: s,
  _props :: p,
  _render :: s -> p -> Elem ((s, p) -> m (s, p)) ()
  -- _eventHandlers :: () -- global event handlers for this component, TBD
} 

makeLenses ''Component

myComp = component "" $ \props state ->
  div & children .~ []
    -- _ --zoomL innerProps otherComponent
    -- ]

component :: s -> (s -> p -> Elem ((s, p) -> m (s, p)) ()) -> p -> Component s p m
component initial f p = Component initial p f

renderComp :: Monad m => Component s p m -> [Elem ((Component s p m) -> m (Component s p m)) ()]
renderComp comp = [mapCallbacks transf $ r s p] where
  s = comp^.state
  r = comp^.render
  p = comp^.props
  -- transf :: ((s, p) -> m (s, p)) -> (Component s p m) -> m (Component s p m) ()
  -- f is the 
  transf f cmp = fmap (\(s, p) -> cmp & state .~ s & props .~ p) (f (cmp^.state, cmp^.props))  

-- zoomL :: Lens' p q -> Component s' q -> Layout m ()
-- zoomL = undefined 