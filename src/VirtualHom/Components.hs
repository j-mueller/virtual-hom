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

-- | Component with state `p` (for Props)
data Component p m = Component {
  _props :: p,
  _render :: p -> Elem (Component p m -> m (Component p m)) ()
  -- _eventHandlers :: () -- global event handlers for this component, TBD
} 

makeLenses ''Component

myComp :: Monad m => String -> Component String m 
myComp = component "" $ \state props ->
  div & children .~ []
    -- _ --zoomL innerProps otherComponent
    -- ]

-- | Create a component with props `p` and inner state `s`
component :: Functor m => s -> (s -> p -> Elem ((s, p) -> m (s, p)) ()) -> p -> Component p m
component initialState f prps = Component prps $ inner initialState where
  inner s p = mapCallbacks (transf s) $ f s p
  transf s cb comp = fmap (\(s', p') -> component s' f p') $ cb (s, comp^.props) 

renderComp :: Monad m => Component p m -> [Elem ((Component p m) -> m (Component p m)) ()]
renderComp comp = [r p] where
  r = comp^.render
  p = comp^.props
  -- transf f cmp = fmap (\p -> cmp & props .~ p) (f (cmp^.props))  

