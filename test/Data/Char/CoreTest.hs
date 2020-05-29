module Data.Char.CoreTest (
    testUnicodeCharacter
  ) where

import Data.Char.Core
import Data.Maybe(isJust)

import Test.Hspec
import Test.QuickCheck

testUnicodeCharacter :: (Arbitrary a, Eq a, Show a, UnicodeCharacter a) => a -> SpecWith ()
testUnicodeCharacter typ = describe "UnicodeCharacter" $ do
    it "equivalent over character" $ property (mapOverChar typ)
    it "equivalent over item" $ property (mapOverItem typ)
    it "equivalent from valid chars over item" (mapValidItem typ)

mapOverChar :: (Eq a, UnicodeCharacter a) => a -> a -> Bool
mapOverChar _ c = Just c == fromUnicodeChar (toUnicodeChar c)

mapOverItem :: UnicodeCharacter a => a -> Char -> Bool
mapOverItem t c = go (typeMapping t c)
    where go Nothing = True
          go (Just x) = c == toUnicodeChar (x `asTypeOf` t)

typeMapping :: UnicodeCharacter a => a -> Char -> Maybe a
typeMapping _ = fromUnicodeChar

mapValidItem :: UnicodeCharacter a => a -> Property
mapValidItem t = forAll (suchThat (arbitrary :: Gen Char) (isJust . typeMapping t)) (mapOverItem t :: Char -> Bool)