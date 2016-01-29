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

-- | Collection of callbacks of an element
data Callbacks cb = Callbacks{
  _blur :: Maybe cb,
  _click :: Maybe cb,
  _change :: Maybe cb,
  _contextmenu :: Maybe cb,
  _dblclick :: Maybe cb,
  _error :: Maybe cb,
  _focus :: Maybe cb,
  _focusin :: Maybe cb,
  _focusout :: Maybe cb,
  _hover :: Maybe cb,
  _keydown :: Maybe cb,
  _keypress :: Maybe cb,
  _keyup :: Maybe cb,
  _load :: Maybe cb,
  _mousedown :: Maybe cb,
  _mouseenter :: Maybe cb,
  _mouseleave :: Maybe cb,
  _mousemove :: Maybe cb,
  _mouseout :: Maybe cb,
  _mouseover :: Maybe cb,
  _mouseup :: Maybe cb,
  _ready :: Maybe cb,
  _resize :: Maybe cb,
  _scroll :: Maybe cb,
  _select :: Maybe cb,
  _submit :: Maybe cb
  }
  deriving (Functor, Foldable, Traversable)

makeLenses ''Callbacks

emptyCb :: Callbacks cb
emptyCb = Callbacks Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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

data RenderingAction c e =
    DeleteElement{ _id :: ElementID }
  | NewElement{ _insertWhere :: InsertWhere, _elemDef :: e }
  | SetTextContent{ _id :: ElementID, _text :: Text }
  | RemoveAttribute{ _id :: ElementID, _attribute :: Text }
  | SetAttribute{ _id :: ElementID, _attribute :: Text, _attrValue :: Text }
  | SetCallback{ _id :: ElementID, _callbackName :: Text, _callback :: c }
  | RemoveCallback{ _id :: ElementID, _callbackName :: Text }
  | NoAction
  deriving (Functor, Foldable, Traversable)

mapCbs :: (c -> d) -> RenderingAction c (Elem c a) -> RenderingAction d (Elem d a)
mapCbs f = mapCb . fmap (mapCallbacks f) where
  mapCb (DeleteElement i)     = DeleteElement i
  mapCb (NewElement i d)      = NewElement i d
  mapCb (SetTextContent i t)  = SetTextContent i t
  mapCb (RemoveAttribute i a) = RemoveAttribute i a
  mapCb (SetAttribute i a v)  = SetAttribute i a v
  mapCb (SetCallback i n c)   = SetCallback i n $ f c
  mapCb (RemoveCallback i n)  = RemoveCallback i n
  mapCb NoAction = NoAction
