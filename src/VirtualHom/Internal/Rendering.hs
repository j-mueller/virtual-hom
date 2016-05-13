-- | Rendering of HTML elements
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VirtualHom.Internal.Rendering where

import           Control.Applicative
import           Control.Lens hiding (children)
import           Control.Monad.State

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (listToMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

import           VirtualHom.Internal.Element
import qualified VirtualHom.Internal.FFI as FFI

import Prelude hiding (error)

data RenderingOptions = RenderingOptions{
  _remainingIDs :: [Text],
  _lastView :: Maybe VirtualElem,
  _actionHandler :: forall a. RenderingAction a -> IO (), -- ^ Callback that will be called after all rendering actions have been executed
  _targetDivId :: Text
}

makeLenses ''RenderingOptions

-- | Prepare a new version of the view (Elem ca ()) to be rendered, using the
-- last known state from the RenderingOptions as a base line.
prepare :: RenderingOptions ->  Elem ca () -> ([RenderingAction ca], RenderingOptions)
prepare opts new = runState go opts where
  go       = maybe makeNew (flip (diff target) new) old
  target   = InsertAsChildOf $ opts^.targetDivId
  old      = opts^.lastView
  makeNew  = do
    newWithIds <- traverse (const nextId) new
    let lastView' = mapCallbacks (const ()) newWithIds
    lastView ?= lastView'
    let result = createNew target [] newWithIds
    return result

-- | Create a `RenderingOptions` value. 
renderingOptions :: 
  Text -- ^ ID of the div where VirtualHom should render its DOM 
  -> RenderingOptions
renderingOptions = RenderingOptions ids Nothing (const $ return ()) where
  ids = fmap ((<>) "virtual-hom-" . T.pack . show) [1..] -- infinite list of IDs

-- | The actual `diff` algorithm - compare the two `Element`s top-down to see
-- where they differ
diff :: MonadState RenderingOptions m =>
  InsertWhere -> -- ID of parent element (for inserting new elements)
  Elem () ElementID -> -- previous elem (diff baseline)
  Elem cb () -> -- new elem
  m [RenderingAction cb]
diff p old new = do
  newWithIds <- traverse (const nextId) new
  let (result, substitutions) = diff' p old newWithIds
  let substitute t = maybe t id $ M.lookup t substitutions
  let lastView' = mapCallbacks (const ()) $ fmap substitute newWithIds
  lastView ?= lastView'
  return result

-- | Actual implementation of diff.
diff' ::
  InsertWhere ->
  Elem () ElementID ->
  Elem cb ElementID ->
  ([RenderingAction cb], Map ElementID ElementID)
diff' i old new
  | old^.elementType == new^.elementType = diffSameType old new
  | otherwise = (createActions, M.empty) where
    del  = DeleteElement $ old^.elemID
    createActions = createNew i [del] new

-- | Generate actions for changing an existing element into a new one, assuming
-- both have the same type.
diffSameType ::
  Elem () ElementID -> -- Old
  Elem cb ElementID -> -- New
  ([RenderingAction cb], Map ElementID ElementID) -- ^ Actions to get from old to new
diffSameType old new = (contAct <> attrAct <> cbAct <> childAct, subst <> childSubst) where
  -- 1. the ID of old element should be kept
  targetId = old^.elemID
  subst = M.fromList [(new^.elemID, targetId)]
  -- 2. check if the content needs to be updated
  contAct =  if (new^.content == old^.content)
                          then [NoAction]
                          else [SetTextContent targetId $ new^.content]
  -- 3. Update the element's attributes
  attrAct = changeAttributes (old^.attributes) (new^.attributes) targetId
  -- 4. Update the element's callbacks
  cbAct = changeCallbacks (old^.callbacks) (new^.callbacks) targetId
  -- 5. Update the element's children
  firstChildLocation = maybe (InsertAsChildOf targetId) (InsertBefore) $ listToMaybe $ fmap (view elemID) $ old^.children
  (childAct, childSubst) = diffChildren firstChildLocation (old^.children) (new^.children)

diffChildren ::
  InsertWhere ->
  [Elem () ElementID] ->
  [Elem cb ElementID] ->
  ([RenderingAction cb], Map ElementID ElementID)
diffChildren w [] news   = (concat $ fmap (createNew w []) news, M.empty)
diffChildren _ olds []   = (fmap (DeleteElement . view elemID) olds, M.empty)
diffChildren w (old:olds) (new:news) = (firstDiff <> restDiffs, firstSubst <> restSubst) where
  (firstDiff, firstSubst) = diff' w old new
  (restDiffs, restSubst ) = diffChildren newPos olds news
  newPos = InsertAfter $ maybe (new^.elemID) id $ M.lookup (new^.elemID) firstSubst

changeCallbacks ::
  Callbacks ca
  -> Callbacks cb
  -> ElementID
  -> [RenderingAction cb]
changeCallbacks old new i = [
  gen (old^.blur)   (new^.blur)   "onblur",
  gen (old^.click)  (new^.click)  "onclick",
  val (old^.change) (new^.change) "onchange",
  gen (old^.contextmenu) (new^.contextmenu) "oncontextmenu",
  gen (old^.dblclick) (new^.dblclick) "ondblclick",
  gen (old^.error) (new^.error) "onerror",
  gen (old^.focus) (new^.focus) "onfocus",
  gen (old^.focusin) (new^.focusin) "onfocusin",
  gen (old^.focusout) (new^.focusout) "onfocusout",
  gen (old^.hover) (new^.hover) "onhover",
  key (old^.keydown) (new^.keydown) "onkeydown",
  key (old^.keypress) (new^.keypress) "onkeypress",
  key (old^.keyup) (new^.keyup) "onkeyup",
  gen (old^.load) (new^.load) "onload",
  gen (old^.mousedown)  (new^.mousedown)  "onmousedown",
  gen (old^.mouseenter)  (new^.mouseenter)  "onmouseenter",
  gen (old^.mouseleave)  (new^.mouseleave)  "onmouseleave",
  gen (old^.mousemove)  (new^.mousemove)  "onmousemove",
  gen (old^.mouseout)  (new^.mouseout)  "onmouseout",
  gen (old^.mouseover)  (new^.mouseover)  "onmouseover",
  gen (old^.mouseup)  (new^.mouseup)  "onmouseup",
  gen (old^.ready)  (new^.ready)  "onready",
  gen (old^.resize)  (new^.resize)  "onresize",
  gen (old^.scroll)  (new^.scroll)  "onscroll",
  gen (old^.select)  (new^.select)  "onselect",
  gen (old^.submit)  (new^.submit)  "onsubmit"
  ] where
    val o n f = case (o, n) of
      (Nothing, Nothing) -> NoAction
      (Just _,  Nothing) -> RemoveCallback i f
      (_,       Just a)  -> SetValueCallback i f a
    gen o n f = case (o, n) of
      (Nothing, Nothing) -> NoAction
      (Just _,  Nothing) -> RemoveCallback i f
      (_,       Just a)  -> SetGenericEventCallback i f a 
    key o n f = case (o, n) of
      (Nothing, Nothing) -> NoAction
      (Just _,  Nothing) -> RemoveCallback i f
      (_,       Just a)  -> SetKeyEventCallback i f a

-- | Takes the map with old attributes and the map with new attributes and
-- generates actions that will transform an element with the first set of
-- attributes to one with the second set of attributes
changeAttributes :: Map Text Text -> Map Text Text -> ElementID -> [RenderingAction cb]
changeAttributes old new i = actions where
  actions = fmap snd $ M.toList $ inner old new
  inner = M.mergeWithKey join mapOld mapNew
  join k a b
    | a == b    = Nothing
    | otherwise = Just $ SetAttribute i k b
  mapOld = M.mapWithKey $ \k _ -> RemoveAttribute i k
  mapNew = M.mapWithKey $ \k v -> SetAttribute i k v

-- | `RenderingAction`s for a single `Elem ElementID`. The 
-- `[RenderingAction cb]` parameter can be used for actions that
-- should be run after the element has been  inserted into the DOM
-- but before any of its attributes/children/content have been set
-- (eg. deletion of a reference element)
createNew :: InsertWhere -> [RenderingAction cb] -> Elem cb ElementID -> [RenderingAction cb]
createNew i deferred p = (x:deferred) ++ (y:xs) where
  y    = SetTextContent (p^.elemID) (p^.content)
  x    = NewElement i (p^.elementType) (p^.elemID) 
  cbs  = changeCallbacks emptyCb (p^.callbacks) (p^.elemID)
  i'   = InsertAsChildOf $ p^.elemID
  rest = concat $ fmap (createNew i' []) $ p^.children
  atts = changeAttributes mempty (p^.attributes) (p^.elemID)
  createdCb = case (p^.callbacks.elementCreated) of
    Nothing -> []
    Just cb -> [GenericIOAction $ cb $ p^.elemID]
  xs   = cbs ++ atts ++ rest ++ createdCb


-- | Get a new id
nextId :: MonadState RenderingOptions m => m Text
nextId = remainingIDs %%= f where
  f = (,) <$> head <*> tail
