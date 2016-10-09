{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens hiding (children, transform)
import Data.Functor.Identity(Identity, runIdentity)
import Data.Monoid
import qualified Data.Text as T
import Prelude hiding (div)

import VirtualHom.Element
import VirtualHom.Internal.Handler
import VirtualHom.Html hiding (content, main)
import VirtualHom.Rendering(renderingOptions)
import VirtualHom.Bootstrap(container, row, btnDefault)
import VirtualHom.Components

counterComp :: Component ()
counterComp = component 0 $ \(state, _) -> 
    [row & children .~ [
      p & content .~ ("This button has been clicked " <> (T.pack $ show state) <> " times"),
      btnDefault 
        & content .~ "Click" 
        & callbacks . click ?~ (\_ -> update $ over _1 succ)
    ]]

data CompState = CompState {
  _counter1 :: Component (),
  _counter2 :: Component ()
}

makeLenses ''CompState

initialState = CompState counterComp counterComp

theUI = component initialState $ \tpl -> [container & children .~ (inner tpl)] where
  inner = (++) <$> subComponent' (_1.counter1) <*> subComponent' (_1.counter2)

main :: IO ()
main = do
  let options = renderingOptions "virtual-hom"
  renderComponent options theUI ()
      
