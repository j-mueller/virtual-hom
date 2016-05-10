{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens hiding (children, transform)
import Data.Functor.Identity(Identity, runIdentity)
import Data.Monoid
import qualified Data.Text as T
import Prelude hiding (div)

import VirtualHom.Element
import VirtualHom.Html hiding (content, main)
import VirtualHom.Rendering(renderingOptions)
import VirtualHom.Bootstrap(container, row, btnDefault)
import VirtualHom.View(View, renderUI)

theUI :: View Identity Int
theUI i = [container & children .~ [
    row & children .~ [
      h1 "Hello, world",
      p & content .~ "I am a paragraph!",
      p & content .~ ("I have been clicked " <> (T.pack $ show i) <> " times"),
      if (i <= 5)
      then btnDefault &
        content .~ "Submit" &
        callbacks . click ?~ const (return . succ)
      else div & content .~ "DONE"]
    ]]

main :: IO ()
main = do
  let options = renderingOptions "virtual-hom"
  let interp = return . runIdentity
  renderUI options theUI interp 0
