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

counterComp :: Monad m => Component m ()
counterComp = component 0 $ \state _ -> 
    [row & children .~ [
      p & content .~ ("This button has been clicked " <> (T.pack $ show state) <> " times"),
      btnDefault 
        & content .~ "Click" 
        & callbacks . click ?~ (\_ (s, p) -> return (succ s, p))
    ]]

theUI = times3 counterComp counterComp counterComp $ \counter1 counter2 counter3 () -> 
          [container & children .~ 
               (counter1 () ++
               counter2 () ++ 
              counter3 ())
             ]

main :: IO ()
main = do
  let options = renderingOptions "virtual-hom"
  let interp = return . runIdentity
  renderComponent options theUI interp ()
      
