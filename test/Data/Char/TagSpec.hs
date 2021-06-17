module Data.Char.TagSpec (
    spec
  ) where

import Data.Char.Tag(fromTag', toTag, toTag')
import Data.Maybe(isJust)

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = it "Check bidirectional mapping" (forAll (suchThat (arbitrary :: Gen Char) (isJust . toTag)) (\x -> x == fromTag' (toTag' x)))
