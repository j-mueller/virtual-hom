{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
#ifndef __GHCJS__
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
#endif
#ifdef __GHCJS__
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE GHCForeignImportPrim #-}
#endif
module VirtualHom.Internal.FFI(
  -- * Rendering
  render,
  renderAction
) where

import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Monoid
import VirtualHom.Internal.Element
#ifdef __GHCJS__
import Data.JSString.Text
import GHCJS.Foreign.Callback
import GHCJS.Prim
import GHCJS.Types (JSString, JSVal, JSRef)

-- | Perform a single `RenderingAction`
renderAction :: RenderingAction (IO ()) -> IO ()
renderAction a = case a of
  NewElement p tp i -> do
    _ <- putStrLn ("Creating new element '" <> T.unpack i <> "' at position: '" <> show p <> "'")
    elm <- js_createElement $ textToJSString tp
    _ <- js_setId elm $ textToJSString i
    case p of
      InsertBefore e -> js_insertBefore elm (textToJSString e)
      InsertAfter e  -> js_insertAfter  elm (textToJSString e)
      InsertAsChildOf e -> do
        e' <- js_getElementById $ textToJSString e
        js_appendChild e' elm
  DeleteElement i -> do
    -- _ <- putStrLn $ "Deleting element: " <> show i
    js_deleteElementById $ textToJSString i
  RemoveAttribute i a -> js_removeAttributeById (textToJSString i) $ textToJSString a
  NoAction -> return ()
  RemoveCallback i n -> do
    -- _ <- putStrLn $ "Removing callback " ++ (show n) ++ " from " ++ (show i)
    js_RemoveCallbackById (textToJSString i) $ textToJSString n
  SetGenericEventCallback i n c -> do
    -- _ <- putStrLn $ "Changing GenericEvent callback " ++ (show n) ++ " of " ++ (show i)
    (asyncCallback1 $ \j -> (getGenericData j) >>= c) >>= js_setCallbackById1 (textToJSString i) (textToJSString n)
  SetValueCallback i n c -> do
    -- _ <- putStrLn $ "Changing ValueChangedData callback " ++ (show n) ++ " of " ++ (show i)
    (asyncCallback1 $ \j -> (getValueChangedData j) >>= c) >>= js_setCallbackById1 (textToJSString i) (textToJSString n)
  SetKeyEventCallback i n c -> do
    -- _ <- putStrLn $ "Changing KeyEventData callback " ++ (show n) ++ " of " ++ (show i)
    (asyncCallback1 $ \j -> (getKeyEventData j) >>= c) >>= js_setCallbackById1 (textToJSString i) (textToJSString n)    
  SetTextContent i t -> do
    -- _ <- putStrLn $ "Set text content of " ++ (show i) ++ " to " ++ (show t)
    js_setTextContent (textToJSString i) (textToJSString t)
  SetAttribute i a v -> do
    -- _ <- putStrLn $ "Set attribute " ++ (show a) ++ " of " ++ (show i) ++ " to " ++ (show v) 
    js_setAttributeById (textToJSString i) (textToJSString a) (textToJSString v)
  GenericIOAction cb -> do
    -- _ <- putStrLn "Running generic IO action"
    cb

getValueChangedData :: JSVal -> IO ValueChangedData
getValueChangedData e = ValueChangedData <$> gen <*> v where
  gen = getGenericData e
  v   = getProp e "target" >>= fmap (T.pack . fromJSString) . flip getProp "value"

getKeyEventData :: JSVal -> IO KeyboardEventData
getKeyEventData e = KeyboardEventData <$> gen <*> v where
  gen = getGenericData e
  v   = fmap (T.pack . fromJSString) $ getProp e "key"
  
getGenericData :: JSVal -> IO GenericEventData
getGenericData v = GenericEventData <$> ts <*> x <*> y where
  ts = fmap fromJSInt $ getProp v "timestamp"
  x  = fmap fromJSInt $ getProp v "pageX"
  y  = fmap fromJSInt $ getProp v "pageY"

foreign import javascript unsafe "document.getElementById($1)"
  js_getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "document.createTextNode($1)"
  js_createTextNode :: JSString -> IO JSVal

foreign import javascript unsafe "$1['appendChild']($2)"
  js_appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "var elem=document.getElementById($2);elem.parentNode.insertBefore($1, elem)"
  js_insertBefore :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "var elem=document.getElementById($2);elem.parentNode.insertBefore($1, elem.nextSibling)"
  js_insertAfter :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "document.createElement($1)"
  js_createElement :: JSString -> IO JSVal

foreign import javascript unsafe "$1['id'] = $2"
  js_setId :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1['setAttribute']($2, $3)"
  js_setAttribute :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript unsafe "$1['onclick']=$2"
  js_setOnClick :: JSVal -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "function(){var e=document.getElementById($1);if(e!=null && e.parentElement!=null)e.parentElement.removeChild(e)}()"
  js_deleteElementById :: JSString -> IO ()

foreign import javascript unsafe "document.getElementById($1)['removeAttribute']($2)"
  js_removeAttributeById :: JSString -> JSString -> IO ()

foreign import javascript unsafe "document.getElementById($1)['setAttribute']($2, $3)"
  js_setAttributeById :: JSString -> JSString -> JSString -> IO ()

foreign import javascript unsafe "document.getElementById($1)[$2]=function(){return;}"
  js_RemoveCallbackById :: JSString -> JSString -> IO ()

foreign import javascript unsafe "document.getElementById($1)['textContent']=$2"
  js_setTextContent :: JSString -> JSString -> IO ()
  
foreign import javascript unsafe "document.getElementById($1)[$2]=$3"
  js_setCallbackById1 :: JSString -> JSString -> Callback (JSVal -> IO ()) -> IO ()
  
#endif
#ifndef __GHCJS__
renderAction :: RenderingAction (IO ()) -> IO ()
renderAction = undefined
#endif

-- | Perform a bunch of renderingActions
render :: (forall c. RenderingAction c -> IO ()) -> [RenderingAction (IO ())] -> IO ()
render cb = fmap (const ()) . sequence . fmap doRender . filter (not . isNop) where
  isNop NoAction  = True
  isNop _         = False
  doRender a = renderAction a >> cb a
