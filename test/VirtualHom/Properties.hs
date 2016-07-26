{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module VirtualHom.Properties where

import Control.Applicative
import Control.Lens
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Instances

import VirtualHom.Internal.Element
import VirtualHom.Internal.Rendering

instance Arbitrary (Elem () ElementID) where
  arbitrary = 
    Elem 
      <$> arbitrary 
      <*> arbitrary 
      <*> arbitrary 
      <*> pure [] -- TODO: Find a sensible way of generating trees
      <*> arbitrary
      <*> pure emptyCb
      <*> arbitrary

-- | Diff two `VirtualElem`s, excluding `NoAction` actions
diff_ :: VirtualElem -> VirtualElem -> [RenderingAction ()]
diff_ l = filter (not. noAction) . fst . diff' i l where
  i = InsertAsChildOf ""
  noAction a = case a of
    NoAction -> True
    _ -> False

instance Show VirtualElem where
  show e = show (e^.elemID) -- TODO: More details

cost :: RenderingAction () -> Int
cost a = case a of
  NoAction -> 0
  DeleteElement _ -> 2
  NewElement{} -> 2
  _ -> 1