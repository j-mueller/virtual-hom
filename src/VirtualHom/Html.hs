{-# LANGUAGE OverloadedStrings #-}
{- Constructors for HTML elements -}
module VirtualHom.Html where

import VirtualHom.Internal.Element

import Control.Lens
import Data.Text (Text)

elmWithContent :: Text -> Text -> Elem cb ()
elmWithContent t c = elm t & content .~ c

div :: Elem cb ()
div = elm "div"

h1 :: Text -> Elem cb ()
h1 = elmWithContent "h1"

h2 :: Text -> Elem cb ()
h2 = elmWithContent "h2"

p :: Text -> Elem cb ()
p = elmWithContent "p"

strong :: Text -> Elem cb ()
strong = elmWithContent "strong"

button :: Elem cb ()
button = elm "button"
