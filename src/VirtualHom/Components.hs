{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module VirtualHom.Components where

import Control.Lens hiding (children)
import Control.Monad.Cont

import VirtualHom.Html
import VirtualHom.Internal.Element
import VirtualHom.Internal.Rendering (RenderingOptions)
import VirtualHom.View (renderUI)

import Prelude hiding (div)

-- Components.
-- Components can have an internal state and an external state, just like 
-- React components have state and props.
-- 

-- | Component with external state `p` (for Props)
newtype Component p m = Component { _render :: p -> Elem (p -> m (p, Component p m)) () }

makeLenses ''Component

data InitialisedComponent p m = InitialisedComponent {
  _props :: p,
  _comp :: Component p m
}

makeLenses ''InitialisedComponent

-- | Render a component inside another component
within :: Functor m => Component p m -> ((p -> Elem (p -> m (p, Component p m)) ()) -> Component p m) -> Component p m
within subC f = f $ fmap transf r' where
  r' = subC^.render
  transf elm = mapCallbacks (\cb' p -> fmap (\(p', comp') -> (p', within comp' f)) $ cb' p) elm

-- | Generalise a component using a lens
generalise :: Functor m => Lens' p q -> Component q m -> Component p m
generalise lns comp = Component render' where
  render' p = mapCallbacks transf $ (view render comp) (p^.lns)
  transf cb p = fmap (\(q', comp') -> (p & lns .~ q', generalise lns comp')) $ cb (p^.lns)

-- | View that shows an initialised component
compView :: Functor m => InitialisedComponent p m -> [Elem ((InitialisedComponent p m) -> m (InitialisedComponent p m)) ()]
compView ic = [mapCallbacks transf $ r initialProps] where
  initialProps = ic^.props
  r = ic^.comp.render
  transf f = fmap (uncurry InitialisedComponent) . f . view props

-- | Render a `Component p m` , given an initial state `p`
renderComponent :: Functor m =>
  RenderingOptions ->
  Component p m ->
  (forall a. m a -> IO a) ->
  p ->
  IO ()
renderComponent opts comp interp p = renderUI opts compView interp c' where  
  c' = InitialisedComponent p comp