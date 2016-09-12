{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens hiding (children, transform)
import Data.Either (isRight)
import Data.Functor.Identity(Identity, runIdentity)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (div)

import VirtualHom.Element hiding (input)
import qualified VirtualHom.Element as E
import VirtualHom.Html hiding (address, content, main)
import VirtualHom.Rendering(renderingOptions)
import VirtualHom.Bootstrap(container, row, btnDefault)
import VirtualHom.View(View, renderUI)

type FormValue = Either String

data Address f = Address{
  _street :: f String,
  _postcode :: f String,
  _town :: f String
  }

makeLenses ''Address

data Person f = Person{
  _firstName :: f String,
  _lastName :: f String,
  _address :: Address f
  }

makeLenses ''Person

class Commute d where
  commute :: (Traversable f, Applicative f, Applicative g) => f (d g) -> g (d f)

instance Commute Address  where
  commute fa = Address <$> s <*> p <*> t where
    s = sequenceA $ fmap (view street) fa
    p = sequenceA $ fmap (view postcode) fa
    t = sequenceA $ fmap (view town) fa

instance Commute Person where
  commute fp = Person <$> f <*> l <*> a where
    f = sequenceA $ fmap (view firstName) fp
    l = sequenceA $ fmap (view lastName) fp
    a = commute $ fmap (view address) fp

type Form m a = View m (FormValue a)

-- | A text input field that produces a value
validatingTextInput :: (Text -> FormValue a) -> Text -> Form IO a
validatingTextInput f l a = [result] where
  errorMarker = either (const "has-error") (const "") a
  result = div
    & attributes . at "class" ?~ ("form-group " <> errorMarker)
    & children .~ [
      label
        & attributes . at "class" ?~ "col-sm-2 control-label"
        & content .~ l,
      div
        & attributes . at "class" ?~ "col-sm-10"
        & children .~ [
        input
          & attributes . at "class" ?~ "form-control"
          & attributes . at "type"  ?~ "text"
          & callbacks . E.change ?~ (\e s -> (putStrLn $ show e) >> return s)
        ]
    ]

theUI :: Form IO Text
theUI t = [result] where
  validate t = if (T.null t) then Left "Must not be empty" else Right t
  result = container & children .~ [
      row & children .~ [h1 "Validating inputs"] ++ (validatingTextInput validate "First name" t) ++ (validatingTextInput validate "Last name" t)
    ]

main :: IO ()
main = do
  let options = renderingOptions "virtual-hom"
  let interp = id
  renderUI options theUI interp (Left "Must not be empty")
