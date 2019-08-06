module Class where

import Hedgehog.Classes

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Bifunctor
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

genEither :: Gen a -> Gen b -> Gen (Either a b)
genEither genA genB = Gen.frequency [ (1, Left <$> genA), (1, Right <$> genB) ]

genSet :: Ord a => Gen a -> Gen (Set a)
genSet genA = Set.fromList <$> Gen.list (Range.linear 0 100) genA

genMap :: Ord k => Gen k -> Gen a -> Gen (Map k a)
genMap genK genA = do
  let sz = Range.linear 0 100
  ks <- Gen.list sz genK
  as <- Gen.list sz genA
  pure $ Map.fromList (List.zip ks as)

genString :: Gen String
genString = Gen.string (Range.linear 2 6) Gen.alpha

genInt :: Gen Int
genInt = Gen.int Range.constantBounded

genSet0 :: Gen (Set Int)
genSet0 = genSet genInt

genMap0 :: Gen (Map String Int)
genMap0 = genMap genString genInt

genMap1 :: Gen a -> Gen (Map String a)
genMap1 = genMap genString

genEither0 :: Gen (Either String Int)
genEither0 = genEither genString genInt

genEither1 :: Gen a -> Gen (Either String a)
genEither1 = genEither genString

tests :: IO Bool
tests = lawsCheckMany
  [ ("Set Int", [eqLaws genSet0, monoidLaws genSet0])
  , ("Map String Int", [eqLaws genMap0, monoidLaws genMap0])
  , ("Map String", [functorLaws genMap1])
  , ("Either String Int", [eqLaws genEither0])
  , ("Either String", [functorLaws genEither1, applicativeLaws genEither1, traversableLaws genEither1])
  , ("Either", [bifunctorLaws genEither])
  -- , ("BadList", [foldableLaws genBadList])
  ]

newtype BadList a = BadList [a]
  deriving (Eq, Show)

instance Foldable BadList where
  foldMap f (BadList xs) = foldMap f xs
  foldl' f z0 (BadList xs) = foldl f z0 xs

genBadList :: Gen a -> Gen (BadList a)
genBadList genA = BadList <$> Gen.list (Range.linear 0 100) genA
