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
import VirtualHom.Html hiding (content, main)
import VirtualHom.Rendering(renderingOptions)
import VirtualHom.Bootstrap(container, row, btnDefault)
import VirtualHom.Components

counterComp :: Monad m => Component m ()
counterComp = component 0 $ \(state, _) -> 
    [row & children .~ [
      p & content .~ ("This button has been clicked " <> (T.pack $ show state) <> " times"),
      btnDefault 
        & content .~ "Click" 
        & callbacks . click ?~ (\_ (s, p) -> return (succ s, p))
    ]]

data CompState m = CompState {
  _counter1 :: Component m (),
  _counter2 :: Component m ()
}

makeLenses ''CompState

initialState = CompState counterComp counterComp

theUI = component initialState $ \tpl -> [container & children .~ (inner tpl)] where
  inner = (++) <$> subComponent' (_1.counter1) <*> subComponent' (_1.counter2)

main :: IO ()
main = do
  let options = renderingOptions "virtual-hom"
  let interp = return . runIdentity
  renderComponent options theUI interp ()
      
