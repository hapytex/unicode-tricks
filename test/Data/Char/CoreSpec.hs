module Data.Char.CoreSpec
  ( spec
  ) where

import Data.Char.Core
import Data.Char.CoreTest

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "isNotACharacter" $ do
    it "Opposite of isACharacter" $ property $
      \c -> isACharacter c /= isNotACharacter c
    it "No overlap with reserved" $ property $
      \c -> not (isNotACharacter c) || not (isReserved c)
    it "No overlap with isAscii" $ property $
      \c -> not (isNotACharacter c) || not (isAscii c)
  describe "isReserved" $ do
    it "Opposite of isNotReserved" $ property $
      \c -> isReserved c /= isNotReserved c
    it "No overlap with isAscii" $ property $
      \c -> not (isReserved c) || not (isAscii c)
  testUnicodeCharacter (undefined :: Char)
