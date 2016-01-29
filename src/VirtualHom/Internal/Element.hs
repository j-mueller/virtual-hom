-- | HTML elements
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module VirtualHom.Internal.Element where

import           Control.Applicative
import           Control.Lens hiding (children)
import           Control.Monad.Cont
import           Data.Bifunctor
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

-- Event data, see http://api.jquery.com/category/events/event-object/ for other data that we could get depending on event type

data GenericEventData = GenericEventData{ _timestamp :: Int, _pageX :: Int, _pageY :: Int }
  deriving (Eq, Ord, Show)
makeLenses ''GenericEventData

data ValueChangedData = ValueChangedData{ _genericData :: GenericEventData, _value :: Text }
  deriving (Eq, Ord, Show)
makeLenses ''ValueChangedData

-- | Collection of callbacks of an element
data Callbacks cb = Callbacks{
  _blur :: Maybe (GenericEventData -> cb),
  _click :: Maybe (GenericEventData -> cb),
  _change :: Maybe (ValueChangedData -> cb),
  _contextmenu :: Maybe (GenericEventData -> cb),
  _dblclick :: Maybe (GenericEventData -> cb),
  _error :: Maybe (GenericEventData -> cb),
  _focus :: Maybe (GenericEventData -> cb),
  _focusin :: Maybe (GenericEventData -> cb),
  _focusout :: Maybe (GenericEventData -> cb),
  _hover :: Maybe (GenericEventData -> cb),
  _keydown :: Maybe (GenericEventData -> cb),
  _keypress :: Maybe (GenericEventData -> cb),
  _keyup :: Maybe (GenericEventData -> cb),
  _load :: Maybe (GenericEventData -> cb),
  _mousedown :: Maybe (GenericEventData -> cb),
  _mouseenter :: Maybe (GenericEventData -> cb),
  _mouseleave :: Maybe (GenericEventData -> cb),
  _mousemove :: Maybe (GenericEventData -> cb),
  _mouseout :: Maybe (GenericEventData -> cb),
  _mouseover :: Maybe (GenericEventData -> cb),
  _mouseup :: Maybe (GenericEventData -> cb),
  _ready :: Maybe (GenericEventData -> cb),
  _resize :: Maybe (GenericEventData -> cb),
  _scroll :: Maybe (GenericEventData -> cb),
  _select :: Maybe (GenericEventData -> cb),
  _submit :: Maybe (GenericEventData -> cb)
  }
  deriving (Functor)

makeLenses ''Callbacks

emptyCb :: Callbacks cb
emptyCb = Callbacks
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing

data Elem cb a = Elem{
  _elementType :: !Text,
  _attributes  :: !(Map Text Text),
  _content     :: !Text,
  _children    :: [Elem cb a],
  _elemID      :: !a,
  _callbacks   :: !(Callbacks cb)
}
  deriving (Functor, Foldable, Traversable)

makeLenses ''Elem

instance Bifunctor Elem where
  bimap f g = mapCallbacks f . fmap g

-- | Transform the callbacks in an Elem
mapCallbacks :: (cb -> cc) -> Elem cb a -> Elem cc a
mapCallbacks f elm = elm{
  _children  = fmap (mapCallbacks f) $ elm^.children,
  _callbacks = fmap f $ elm^.callbacks
  }

type ElementID = Text
type ElementType = Text
type VirtualElem = Elem () ElementID -- an element whose callbacks are of type ()

-- | Create an element with the specified type
elm :: Text -> Elem cb ()
elm t = Elem t mempty mempty [] () emptyCb

-- | Where to insert an element - before another elem, or as (last) child of an
-- elem
data InsertWhere = InsertBefore Text | InsertAsChildOf Text | InsertAfter Text
  deriving Show

data RenderingAction c =
    DeleteElement{ _id :: ElementID }
  | NewElement{ _insertWhere :: InsertWhere, _elemType :: Text, _id :: ElementID}
  | SetTextContent{ _id :: ElementID, _text :: Text }
  | RemoveAttribute{ _id :: ElementID, _attribute :: Text }
  | SetAttribute{ _id :: ElementID, _attribute :: Text, _attrValue :: Text }
  | SetGenericEventCallback{ _id :: ElementID, _callbackName :: Text, _genericEventCallback :: GenericEventData -> c }
  | SetValueCallback{ _id :: ElementID, _callbackName :: Text, _valueChangedCallback :: ValueChangedData -> c }
  | RemoveCallback{ _id :: ElementID, _callbackName :: Text }
  | NoAction
  deriving (Functor)
