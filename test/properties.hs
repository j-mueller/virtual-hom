{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Lens hiding (children)
import Control.Monad (ap)
import Data.Maybe (catMaybes, mapMaybe)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import VirtualHom.Internal.Element
import VirtualHom.Rendering
import VirtualHom.Properties

prop_no_changes :: VirtualElem -> Bool
prop_no_changes e = null (diff_ e e)

prop_cost_triangle_inequality :: VirtualElem -> VirtualElem -> VirtualElem -> Bool
prop_cost_triangle_inequality a b c = cab + cbc >= cac where
	cab = sum $ fmap cost $ diff_ a b
	cbc = sum $ fmap cost $ diff_ b c
	cac = sum $ fmap cost $ diff_ a c

prop_delete_children :: VirtualElem -> [VirtualElem] -> Bool
prop_delete_children parent ch = length (diff_ parent' parent) == length ch where
	parent' = parent & children .~ ch 

prop_generate_elements_before_using_them :: VirtualElem -> [VirtualElem] -> Bool
prop_generate_elements_before_using_them parent ch = all isNewElem $ map (firstPos . view elemID) ch  where
	parent' = parent & children .~ ch
	d = diff_ parent parent'
	isNewElem e = case e of
		Just NewElement{} -> True
		_ -> False
	firstPos i = firstOf (folded.filtered ((==) (Just i) . preview elementId)) d

prop_generate_elements_in_correct_order :: VirtualElem -> [VirtualElem] -> Bool
prop_generate_elements_in_correct_order parent ch = childElemeIds == newElemIds where
	parent' = parent & children .~ ch
	d = diff_ parent parent'
	childElemeIds = map (view elemID) ch
	newElemIds = mapMaybe (preview elementId) (filter isNewElem d)
	isNewElem e = case e of
		NewElement{} -> True
		_ -> False

prop_insert_elements_at_correct_position :: VirtualElem -> [VirtualElem] -> Bool
prop_insert_elements_at_correct_position parent ch = actualPositions ==  expectedPositions where
	d = diff_ parent parent' 
	parent' = parent & children .~ ch
	expectedPositions = take (length ch) $ (InsertAsChildOf $ parent ^. elemID):(fmap (InsertAfter . view elemID) ch)
	actualPositions = catMaybes $ fmap (preview insertWhere) d

prop_delete_and_insert_elements_in_correct_order :: VirtualElem -> [VirtualElem] -> Bool
prop_delete_and_insert_elements_in_correct_order parent ch = and isExpected where
	d = diff_ parent1 parent2
	parent1 = parent & children .~ ch1
	parent2 = parent & children .~ ch2
	ch1 = fmap (set elementType "div") ch
	ch2 = fmap (set elementType "a")   ch
	childNames = fmap (view elemID) ch
	expectations = childNames >>= (\n -> [isInsert n, isDelete n])
	actualCommands = (filter isDeleteOrInsert d) 
	isExpected = fmap (ap fst snd) $ zip expectations actualCommands  
	isDelete elmId command = case command of
		DeleteElement i -> i == elmId
		_ -> False
	isInsert elmId command = case command of
		NewElement _ _ i _ -> i == elmId
		_ -> False
	isDeleteOrInsert command = case command of
		DeleteElement{} -> True
		NewElement{} -> True
		_ -> False

main :: IO ()
main = $defaultMainGenerator