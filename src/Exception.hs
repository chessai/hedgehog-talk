{-# language
        TemplateHaskell
  #-}

module Exception where

import qualified Data.List as List

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

myFilter :: Ord a => (a -> Bool) -> [a] -> [a]
myFilter _ (_ : _ : _ : _ : _) = error "five things is too much!"
myFilter _ [] = []
myFilter p (x : xs) = if p x
  then x : filter p xs
  else filter p xs

myMaximum :: Ord a => [a] -> a
myMaximum (x:xs) = x `max` maximum xs

prop_equals :: Property
prop_equals = property $ do
  xs <- forAll $ Gen.list (Range.linear 1 100) (Gen.int Range.constantBounded)
  maximum xs === List.maximum xs

prop_assert :: Property
prop_assert = property $ do
  xs <- forAll $ Gen.list (Range.linear 1 100) (Gen.int Range.constantBounded)
  assert $ filter (==0) xs == []

prop_property_exception :: Property
prop_property_exception = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int Range.constantBounded)
  myMaximum xs === List.maximum xs

tests :: IO Bool
tests = checkSequential $$(discover)
