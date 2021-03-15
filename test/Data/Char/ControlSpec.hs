module Data.Char.ControlSpec (
    spec
  ) where

import Data.Char.Control
import Data.Maybe(isJust)

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = it "Check bidirectional mapping" (forAll (suchThat (arbitrary :: Gen Char) (isJust . controlPicture)) (\x -> x == fromControlPicture' (controlPicture' x)))
