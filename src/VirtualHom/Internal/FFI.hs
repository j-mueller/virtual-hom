{-# LANGUAGE CPP #-}
#ifndef __GHCJS__
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
#endif
#ifdef __GHCJS__
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
module VirtualHom.Internal.FFI(
  -- * Rendering
  render,
  renderAction
) where

import Control.Lens
import qualified Data.Map.Strict as M
import Data.Monoid
import VirtualHom.Internal.Element
#ifdef __GHCJS__
import Data.JSString.Text
import GHCJS.Foreign.Callback
import GHCJS.Prim
import GHCJS.Types (JSString, JSVal)

-- | Perform a single `RenderingAction`
renderAction :: RenderingAction (IO ()) (Elem (IO ()) ElementID) -> IO ()
renderAction a = case a of
  NewElement p def -> do
    _ <- putStrLn ("Creating new element with parent: " <> show p)
    elm <- js_createElement $ textToJSString $ def^.elementType
    t <- js_createTextNode $ textToJSString $ def^.content
    _ <- js_appendChild elm t
    _ <- sequence $ fmap (uncurry $ js_setAttribute elm) $ fmap ((,) <$> textToJSString . fst <*> textToJSString . snd) $ M.toList $ def^.attributes
    _ <- js_setId elm $ textToJSString $ view elemID def
    _ <- maybe (return ()) (\c -> asyncCallback1 (const c) >>= js_setOnClick elm) $ def^.callbacks.click
    case p of
      InsertBefore e -> js_insertBefore elm (textToJSString e)
      InsertAfter e  -> js_insertAfter  elm (textToJSString e)
      InsertAsChildOf e -> do
        e' <- js_getElementById $ textToJSString e
        js_appendChild e' elm
  DeleteElement i -> do
    _ <- putStrLn $ "Deleting element: " <> show i
    js_deleteElementById $ textToJSString i
  RemoveAttribute i a -> js_removeAttributeById (textToJSString i) $ textToJSString a
  NoAction -> return ()
  RemoveCallback i n -> do
    _ <- putStrLn $ "Removing callback " ++ (show n) ++ " from " ++ (show i)
    js_RemoveCallbackById (textToJSString i) $ textToJSString n
  SetCallback i n c -> do
    _ <- putStrLn $ "Changing callback " ++ (show n) ++ " of " ++ (show i)
    (asyncCallback c) >>= js_setCallbackById (textToJSString i) (textToJSString n)
  SetTextContent i t -> do
    _ <- putStrLn $ "Set text content of " ++ (show i) ++ " to " ++ (show t)
    js_setTextContent (textToJSString i) (textToJSString t)
  SetAttribute i a v -> js_setAttributeById (textToJSString i) (textToJSString a) (textToJSString v)

foreign import javascript unsafe "document.getElementById($1)"
  js_getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "document.createTextNode($1)"
  js_createTextNode :: JSString -> IO JSVal

foreign import javascript unsafe "$1['appendChild']($2)"
  js_appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$($1).insertBefore(document.getElementById($2))"
  js_insertBefore :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$($1).insertAfter(document.getElementById($2))"
  js_insertAfter :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "document.createElement($1)"
  js_createElement :: JSString -> IO JSVal

foreign import javascript unsafe "$1['id'] = $2"
  js_setId :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1['setAttribute']($2, $3)"
  js_setAttribute :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript unsafe "$1['onclick']=$2"
  js_setOnClick :: JSVal -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "function(){e=document.getElementById($1);if(e!=null && e.parentElement!=null)e.parentElement.removeChild(e)}()"
  js_deleteElementById :: JSString -> IO ()

foreign import javascript unsafe "document.getElementById($1)['removeAttribute']($2)"
  js_removeAttributeById :: JSString -> JSString -> IO ()

foreign import javascript unsafe "document.getElementById($1)['setAttribute']($2, $3)"
  js_setAttributeById :: JSString -> JSString -> JSString -> IO ()

foreign import javascript unsafe "$(document.getElementById($1)).off($2)"
  js_RemoveCallbackById :: JSString -> JSString -> IO ()

foreign import javascript unsafe "document.getElementById($1)['textContent']=$2"
  js_setTextContent :: JSString -> JSString -> IO ()

-- TODO: support callbacks with more than 0 arguments
foreign import javascript unsafe "$(document.getElementById($1)).on($2, $3)"
  js_setCallbackById :: JSString -> JSString -> Callback (IO ()) -> IO ()

#endif
#ifndef __GHCJS__
renderAction :: RenderingAction (IO ()) (Elem (IO ()) ElementID) -> IO ()
renderAction = undefined
#endif

-- | Perform a bunch of renderingActions
render :: [RenderingAction (IO ()) (Elem (IO ()) ElementID)] -> IO ()
render = fmap (const ()) . sequence . fmap renderAction . filter (not . isNop) where
  isNop NoAction = True
  isNop _        = False
