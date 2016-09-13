{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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

counterComp :: Monad m => Component () m
counterComp = Component $ go 0 where
  go state () =  
    row & children .~ [
      p & content .~ ("This button has been clicked " <> (T.pack $ show state) <> " times"),
      btnDefault 
        & content .~ "Click" 
        & callbacks . click ?~ (\_ p -> return (p, Component $ go $ succ state))
    ] 

theUI :: Monad m => Component () m
theUI = within counterComp $ \counter1 ->
        within counterComp $ \counter2 ->
          Component $ \_ ->
            container & children .~ [
              counter1 (),
              counter2 ()
            ]

main :: IO ()
main = do
  let options = renderingOptions "virtual-hom"
  let interp = return . runIdentity
  renderComponent options theUI interp ()
