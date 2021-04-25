module Data.Char.CoreTest (
    testUnicodeCharacter
  , testUnicodeText
  ) where

import Data.Char.Core
import Data.Maybe(isJust)
import Data.Typeable(Typeable, typeOf)

import Test.Hspec
import Test.QuickCheck

testUnicodeCharacter :: (Arbitrary a, Eq a, Show a, Typeable a, UnicodeCharacter a) => a -> SpecWith ()
testUnicodeCharacter typ = describe ("instance UnicodeCharacter " ++ instanceName (show (typeOf typ))) $ do
    it "equivalent over character" $ property (mapOverChar typ)
    it "equivalent over item" $ property (mapOverItem typ)
    it "equivalent from valid chars over item" (mapValidItem typ)
    it "fromUnicodeChar and fromUnicodeChar' are equivalent" (equivalentFromChar typ)

testUnicodeText :: (Arbitrary a, Eq a, Show a, Typeable a, UnicodeText a) => a -> SpecWith ()
testUnicodeText typ = describe ("instance UnicodeText " ++ instanceName (show (typeOf typ))) $ it "equivalent over text" $ property (mapOverText typ)

instanceName :: String -> String
instanceName s | ' ' `elem` s = '(' : s ++ ")"
               | otherwise = s

mapOverChar :: (Eq a, UnicodeCharacter a) => a -> a -> Bool
mapOverChar _ c = Just c == fromUnicodeChar (toUnicodeChar c)

mapOverItem :: UnicodeCharacter a => a -> Char -> Bool
mapOverItem t c = go (typeMapping t c)
    where go Nothing = True
          go (Just x) = c == toUnicodeChar (x `asTypeOf` t)

equivalentMapping :: (Eq a, UnicodeCharacter a) => a -> Char -> Bool
equivalentMapping t c = go (typeMapping t c)
    where go Nothing = True
          go (Just x) = x == typeMapping' (x `asTypeOf` t) c

typeMapping :: UnicodeCharacter a => a -> Char -> Maybe a
typeMapping _ = fromUnicodeChar

typeMapping' :: UnicodeCharacter a => a -> Char -> a
typeMapping' _ = fromUnicodeChar'

mapValidItem :: UnicodeCharacter a => a -> Property
mapValidItem t = forAll (suchThat (arbitrary :: Gen Char) (isJust . typeMapping t)) (mapOverItem t :: Char -> Bool)

equivalentFromChar :: Eq a => UnicodeCharacter a => a -> Property
equivalentFromChar t = forAll (suchThat (arbitrary :: Gen Char) (isJust . typeMapping t)) (equivalentMapping t :: Char -> Bool)

mapOverText :: (Eq a, UnicodeText a) => a -> a -> Bool
mapOverText _ c = Just c == fromUnicodeText (toUnicodeText c)
