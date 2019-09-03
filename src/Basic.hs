{-# language
        GeneralizedNewtypeDeriving
      , LambdaCase
      , OverloadedStrings
      , TemplateHaskell
  #-}

module Basic where

import Control.Monad (guard)
import qualified Data.List as List
import qualified Data.Text as Text

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

--foo :: MonadGen m => m ()

prop_success :: Property
prop_success = property success

prop_discard :: Property
prop_discard = property discard

prop_failure :: Property
prop_failure = property failure

prop_test_limit :: Property
prop_test_limit = withTests 10000 . property $ success

prop_discard_limit :: Property
prop_discard_limit = withDiscards 5000 . property $ discard

prop_shrink_limit :: Property
prop_shrink_limit = withShrinks 0 . property $ do
  x <- forAll $ Gen.enum 'a' 'Z'
  assert $ x == 'z'

prop_takeEnd :: Property
prop_takeEnd = property $ do
  xs <- forAll $ Gen.string (Range.linear 0 100) Gen.unicode
  n <- forAll $ Gen.int (Range.linear 0 100)

  let string = List.reverse
        . List.take (fromIntegral n)
        . List.reverse
        $ xs
  let text = Text.unpack
        . Text.takeEnd n
        $ Text.pack xs

  string === text

tests :: IO Bool
tests = checkSequential $$(discover)
