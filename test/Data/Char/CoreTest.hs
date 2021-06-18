{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

module Data.Char.CoreTest (
    testBounded
  , testUnicodeCharacter
  , testUnicodeText
  , testHashable
  , testMirrorHorizontally
  , testMirrorVertically
  ) where

import Data.Char.Core
import Data.Hashable(Hashable(hash))
import Data.Maybe(isJust)
import Data.Typeable(Typeable, typeOf)

import Test.Hspec
import Test.QuickCheck

instanceText :: String -> String
instanceText cls = "\ESC[1;34minstance\ESC[0m \ESC[1m" ++ cls ++ "\ESC[0m "

instanceText' :: forall a b . Typeable a => String -> SpecWith b -> SpecWith b
instanceText' st = describe (instanceText st ++ instanceName (show (typeOf (undefined :: a))))

testMirrorHorizontally :: forall a . (Arbitrary a, Eq a, MirrorHorizontal a, Show a, Typeable a) => SpecWith ()
testMirrorHorizontally = instanceText' @a "MirrorHorizontal" $ do
  it "test if two calls are an identity" $ property (doubleCallHorizontally @a)

doubleCallHorizontally :: (Eq a, MirrorHorizontal a) => a -> Bool
doubleCallHorizontally x = mirrorHorizontal (mirrorHorizontal x) == x

testMirrorVertically :: forall a . (Arbitrary a, Eq a, MirrorVertical a, Show a, Typeable a) => SpecWith ()
testMirrorVertically = instanceText' @a "MirrorVertical" $ do
  it "test if two calls are an identity" $ property (doubleCallVertically @a)

doubleCallVertically :: (Eq a, MirrorVertical a) => a -> Bool
doubleCallVertically x = mirrorVertical (mirrorVertical x) == x


testUnicodeCharacter :: forall a . (Arbitrary a, Eq a, Show a, Typeable a, UnicodeCharacter a) => SpecWith ()
testUnicodeCharacter = instanceText' @a "UnicodeCharacter" $ do
    it "equivalent over character" $ property (mapOverChar @ a)
    it "equivalent over item" $ property (mapOverItem @ a)
    it "equivalent from valid chars over item" (mapValidItem @ a)
    it "fromUnicodeChar and fromUnicodeChar' are equivalent" (equivalentFromChar @ a)

testBounded :: forall a . (Arbitrary a, Bounded a, Ord a, Show a, Typeable a) => SpecWith ()
testBounded = instanceText' @a "Bounded" $ do
    it "all items are greater than or equal to the lowerbound" $ property (checkLowerbound @a)
    it "all items are less than or equal to the upperbound" $ property (checkUpperbound @a)

checkLowerbound :: (Bounded a, Ord a) => a -> Bool
checkLowerbound x = x >= minBound

checkUpperbound :: (Bounded a, Ord a) => a -> Bool
checkUpperbound x = x <= maxBound

-- testBounded :: forall a . (Arbitrary a, Ord a, Show a) => SpecWith ()
-- testBounded = describe (instanceText' "Bounded") $ do
--  it "equivalent over character" (property (mapOverChar @ a))

testUnicodeText :: forall a . (Arbitrary a, Eq a, Show a, Typeable a, UnicodeText a) => SpecWith ()
testUnicodeText = describe (instanceText "UnicodeText" ++ instanceName (show (typeOf (undefined :: a)))) $ it "equivalent over text" $ property (mapOverText @ a)

testHashable :: forall a . (Arbitrary a, Eq a, Show a, Typeable a, Hashable a) => SpecWith ()
testHashable = describe (instanceText "Hashable" ++ instanceName (show (typeOf (undefined :: a)))) $ it "hashing law" $ (property (hashEquality @a))

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

hashEquality :: (Eq a, Hashable a) => a -> a -> Bool
hashEquality ca cb = hash ca == hash cb || ca /= cb
