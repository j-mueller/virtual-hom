{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens hiding (children, transform)
import Data.Functor.Identity(Identity, runIdentity)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (div)

import VirtualHom.Element
import VirtualHom.Html hiding (content, main)
import VirtualHom.Rendering(renderingOptions)
import VirtualHom.Bootstrap(container, row, btnDefault)
import VirtualHom.View(View, renderUI, specialise, on)

data AppState = Loading | Error | Done

makePrisms ''AppState

data Application = Application{
  _notifications :: [Text],
  _state :: AppState
  }

initialState :: Application
initialState = Application ["Ready"] Loading

makeLenses ''Application

loadingView :: View Identity ()
loadingView _ = [p & content .~ "Loading"]

errorView :: View Identity ()
errorView _ = [p & content .~ "Error"]

doneView :: View Identity ()
doneView _ = [p & content .~ "Done"]

-- much composition
stateView :: View Identity AppState
stateView =
  (on _Loading loadingView) <>
  (on _Error   errorView)   <>
  (on _Done    doneView)

notify :: View Identity [Text]
notify ns = [ul & children .~ fmap (\t -> li & content .~ t) ns]

theUI :: View Identity Application
theUI = (specialise notifications notify) <> (specialise state stateView)

main :: IO ()
main = do
  let options = renderingOptions "virtual-hom"
  let interp = return . runIdentity
  renderUI options theUI interp initialState
