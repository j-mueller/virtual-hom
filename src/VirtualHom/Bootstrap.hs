{-# LANGUAGE OverloadedStrings #-}
module VirtualHom.Bootstrap(
  -- * Containers
  container,
  row,
  -- * Buttons
  btnDefault
) where

import           Control.Lens
import           Prelude hiding (div)

import VirtualHom.Internal.Element
import VirtualHom.Html

container :: Elem m ()
container = div & attributes . at "class" ?~ "container"

row :: Elem m ()
row = div & attributes . at "class" ?~ "row"

btnDefault :: Elem m ()
btnDefault = button & attributes . at "class" ?~ "btn btn-default"
