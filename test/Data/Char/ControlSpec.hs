module Data.Char.ControlSpec (
    spec
  ) where

import Data.Char.Control

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Data.Char.Control" $
  it "Check bidirectional mapping" forAll (suchThat (arbitrary :: Gen Char) (isJust . controlPicture)) (\x -> x == fromControlPicture' (controlPicture' x))
