{-# LANGUAGE ScopedTypeVariables #-}
module VirtualHom.Rendering(
  RenderingOptions,
  renderingOptions,
  targetDivId,
  onElementChanged
  ) where

import Control.Lens
import           Data.Text (Text)

import VirtualHom.Internal.Element (RenderingAction, elementId)
import VirtualHom.Internal.Rendering

-- | Callback when a new DOM element is created
onElementChanged :: (Text -> IO ()) -> RenderingOptions -> RenderingOptions
onElementChanged cb opts = opts{ _actionHandler = newHandler } where
  newHandler :: forall c. RenderingAction c -> IO ()
  newHandler = cb . view elementId -- TODO: Currently "view elementId" results in "" when called with NoAction - this case should be handled separately