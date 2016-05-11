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

theUI :: View Identity (Int,T.Text)
theUI (i,t) = [container & children .~ [
    row & children .~ [
      h1 "Hello, world",
      p & content .~ "I am a paragraph!",
      p & content .~ ("I have been clicked " <> (T.pack $ show i) <> " times"),
      if (i <= 5)
      then btnDefault &
        content .~ "Submit" &
        callbacks . click ?~ (\_ (a, b) -> return (succ a, t))
      else div & content .~ "DONE"],
      row & children .~ [
          VirtualHom.Html.select & 
            children .~ [
              option & content .~ "Option 1" & attributes . at "value" .~ Just "1",
              option & content .~ "Option 2" & attributes . at "value" .~ Just "2"
              ] &
            callbacks . change ?~ (\e (a, _) -> return (a, e^.value)),
          div & content .~ "Selected option: " <> t
      ]
    ]]

main :: IO ()
main = do
  let options = renderingOptions "virtual-hom"
  let interp = return . runIdentity
  renderUI options theUI interp (0,"1")
