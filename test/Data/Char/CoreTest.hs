{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

module Data.Char.CoreTest (
    testUnicodeCharacter
  , testUnicodeText
  ) where

import Data.Char.Core
import Data.Maybe(isJust)
import Data.Typeable(Typeable, typeOf)

import Test.Hspec
import Test.QuickCheck

testUnicodeCharacter :: forall a . (Arbitrary a, Eq a, Show a, Typeable a, UnicodeCharacter a) => SpecWith ()
testUnicodeCharacter = describe ("instance UnicodeCharacter " ++ instanceName (show (typeOf (undefined :: a)))) $ do
    it "equivalent over character" $ property (mapOverChar @ a)
    it "equivalent over item" $ property (mapOverItem @ a)
    it "equivalent from valid chars over item" (mapValidItem @ a)
    it "fromUnicodeChar and fromUnicodeChar' are equivalent" (equivalentFromChar @ a)

testUnicodeText :: forall a . (Arbitrary a, Eq a, Show a, Typeable a, UnicodeText a) => SpecWith ()
testUnicodeText = describe ("instance UnicodeText " ++ instanceName (show (typeOf (undefined :: a)))) $ it "equivalent over text" $ property (mapOverText @ a)

instanceName :: String -> String
instanceName s | ' ' `elem` s = '(' : s ++ ")"
               | otherwise = s

mapOverChar :: forall a . (Eq a, UnicodeCharacter a) => a -> Bool
mapOverChar c = Just c == fromUnicodeChar (toUnicodeChar c)

mapOverItem :: forall a . UnicodeCharacter a => Char -> Bool
mapOverItem c = maybe True ((c ==) . toUnicodeChar @ a) (fromUnicodeChar @ a c)

equivalentMapping :: forall a . (Eq a, UnicodeCharacter a) => Char -> Bool
equivalentMapping c = (maybe True (fromUnicodeChar' @ a c ==)) (fromUnicodeChar @ a c)

mapValidItem :: forall a . UnicodeCharacter a => Property
mapValidItem = forAll (suchThat (arbitrary :: Gen Char) (isJust . fromUnicodeChar @ a)) (mapOverItem @ a)

equivalentFromChar :: forall a . (Eq a, UnicodeCharacter a) => Property
equivalentFromChar = forAll (suchThat (arbitrary :: Gen Char) (isJust . fromUnicodeChar @ a)) (equivalentMapping @ a)

mapOverText :: (Eq a, UnicodeText a) => a -> a -> Bool
mapOverText _ c = Just c == fromUnicodeText (toUnicodeText c)
