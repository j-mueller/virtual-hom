{-# LANGUAGE RankNTypes #-}
module VirtualHom.View where

import VirtualHom.Internal.Element
import VirtualHom.Internal.Rendering
import VirtualHom.Internal.FFI
import VirtualHom.Html (div)

import Control.Lens hiding (children)
import Data.Bifunctor
import Prelude hiding (div)

-- | A `View m a` is a function from `a`, the application state, to virtual
-- `Elem`s (ie `Elem`s whose type of ID is `()`) with callbacks whose effect is
-- `m`
type View m a = a -> [Elem (a -> m a) ()]

-- | Render a `View m a` by providing an interpreter for effects in `m` and an
-- initial state `a`.
--
-- When the state changes (via one of the callbacks), the view will be updated
-- incrementally, based on the diff between the old and the new `Elem`s.
renderUI ::
  RenderingOptions -> -- Some settings for rendering
  View m a -> -- The view
  (m a -> IO a) -> -- Interpreter for effects
  a -> -- Initial application state
  IO ()
renderUI opts view interp state = render ioActions where
  -- prepare the actions that will turn the old DOM into the new DOM, along with
  -- an updated version of the `RenderingOptions`
  (actions, opts') = prepare opts $ div & children .~ view state
  -- a callback for updating the UI when the app. state has changed
  -- updateUI :: a -> IO ()
  updateUI = renderUI opts' view interp
  -- list of actions, this time with all callbacks transformed to `IO ()`` so
  -- that the event handlers can be hooked up properly.
  ioActions = fmap (fmap $ \f -> interp (f state) >>= updateUI) actions

-- | Integrate a view of only part of an application state
specialise :: Monad m => Lens' a b -> View m b -> View m a
specialise l v = fmap (first $ mapMOf l) . v . view l

-- | Show a view only if a prism gets a value`
on :: Monad m => Prism' a b -> View m b -> View m a
on p v a = withPrism p $ \_ from ->
  either (const []) (fmap (first $ mapMOf p) . v) $ from a
