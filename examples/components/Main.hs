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
import VirtualHom.Components

counterComp = component 0 $ \state () -> 
    row & children .~ [
      p & content .~ ("This button has been clicked " <> (T.pack $ show state) <> " times"),
      btnDefault 
        & content .~ "Click" 
        & callbacks . click ?~ (\_ (a, ()) -> return (succ a, ()))
    ] 

theUI = counterComp `within` _2 $ \counter1 ->
        counterComp `within` _2 $ \counter2 ->
        component () $ \() () ->
          counter1 ()

main :: IO ()
main = do
  let options = renderingOptions "virtual-hom"
  let interp = return . runIdentity
  renderComponent options theUI interp ()
