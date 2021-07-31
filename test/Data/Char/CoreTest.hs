{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

module Data.Char.CoreTest (
    testBounded
  , testUnicodeCharacter
  , testUnicodeText
  , testHashable
  , testMirrorHorizontally
  , testMirrorVertically
  , testMirrorHorizontallyVertically
  ) where

import Data.Char.Core
import Data.Hashable(Hashable(hash))
import Data.List(intercalate)
import Data.Maybe(isJust)
import Data.Text(Text, pack, singleton)
import Data.Typeable(Typeable, typeOf)

import Test.Hspec
import Test.QuickCheck

instanceText :: String -> String
instanceText cls = "\ESC[1;34minstance\ESC[0m \ESC[1m" ++ cls ++ "\ESC[0m "

instanceTexts' :: forall a b . Typeable a => [String] -> SpecWith b -> SpecWith b
instanceTexts' sts = describe (intercalate " and " [instanceText st ++ instanceName (show (typeOf (undefined :: a))) | st <- sts])


instanceText' :: forall a b . Typeable a => String -> SpecWith b -> SpecWith b
instanceText' st = describe (instanceText st ++ instanceName (show (typeOf (undefined :: a))))

testMirrorHorizontallyVertically :: forall a . (Arbitrary a, Eq a, MirrorHorizontal a, MirrorVertical a, Show a, Typeable a) => SpecWith ()
testMirrorHorizontallyVertically = instanceTexts' @a ["MirrorHorizontal", "MirrorVertical"] $ do
  it "test if mirror horizontal, vertical, horizontal, and vertical yield the same object" $ property (doubleCallHorizontallyVertically @a)
  it "test if mirror horizontal, vertical, vertical, and horizontal yield the same object" $ property (doubleCallHorizontallyVertically2 @a)
  it "test if mirror vertical, horizontal, horizontal, vertical, yield the same object" $ property (doubleCallHorizontallyVertically3 @a)
  it "test if two horizontal mirror calls are an identity" $ property (doubleCallHorizontally @a)
  it "test if two vertical mirror calls are an identity" $ property (doubleCallVertically @a)
  it "test of a horizontal and vertical flip is the same as a vertical and horizontal flip" $ property (horizontalVerticalSame @a)

testMirrorHorizontally :: forall a . (Arbitrary a, Eq a, MirrorHorizontal a, Show a, Typeable a) => SpecWith ()
testMirrorHorizontally = instanceText' @a "MirrorHorizontal" $ do
  it "test if two calls are an identity" $ property (doubleCallHorizontally @a)

doubleCallHorizontally :: forall a . (Eq a, MirrorHorizontal a) => a -> Bool
doubleCallHorizontally x = mirrorHorizontal (mirrorHorizontal x) == x

testMirrorVertically :: forall a . (Arbitrary a, Eq a, MirrorVertical a, Show a, Typeable a) => SpecWith ()
testMirrorVertically = instanceText' @a "MirrorVertical" $ do
  it "test if two calls are an identity" $ property (doubleCallVertically @a)

horizontalVerticalSame :: forall a . (Eq a, MirrorHorizontal a, MirrorVertical a) => a -> Bool
horizontalVerticalSame x = mirrorVertical (mirrorHorizontal x) == mirrorHorizontal (mirrorVertical x)

doubleCallVertically :: forall a . (Eq a, MirrorVertical a) => a -> Bool
doubleCallVertically x = mirrorVertical (mirrorVertical x) == x

doubleCallHorizontallyVertically :: (Eq a, MirrorHorizontal a, MirrorVertical a) => a -> Bool
doubleCallHorizontallyVertically x = mirrorVertical (mirrorHorizontal (mirrorVertical (mirrorHorizontal x))) == x

doubleCallHorizontallyVertically2 :: (Eq a, MirrorHorizontal a, MirrorVertical a) => a -> Bool
doubleCallHorizontallyVertically2 x = mirrorHorizontal (mirrorVertical (mirrorVertical (mirrorHorizontal x))) == x

doubleCallHorizontallyVertically3 :: (Eq a, MirrorHorizontal a, MirrorVertical a) => a -> Bool
doubleCallHorizontallyVertically3 x = mirrorVertical (mirrorHorizontal (mirrorHorizontal (mirrorVertical x))) == x

testUnicodeCharacter :: forall a . (Arbitrary a, Eq a, Show a, Typeable a, UnicodeCharacter a) => SpecWith ()
testUnicodeCharacter = instanceText' @a "UnicodeCharacter" $ do
    it "equivalent over character" $ property (mapOverChar @a)
    it "equivalent over item" $ property (mapOverItem @a)
    it "equivalent from valid chars over item" (mapValidItem @a)
    it "fromUnicodeChar and fromUnicodeChar' are equivalent" (equivalentFromChar @a)
    it "check isInCharRange method" (property (isInCharRangeCheck @a))

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
--  it "equivalent over character" (property (mapOverChar @a))

testUnicodeText :: forall a . (Arbitrary a, Eq a, Show a, Typeable a, UnicodeText a) => SpecWith ()
testUnicodeText = describe (instanceText "UnicodeText" ++ instanceName (show (typeOf (undefined :: a)))) $ do
  it "equivalent over text" $ property (mapOverText @a)
  it "check isInTextRange method 1" (property (isInTextRangeCheck1 @a))
  it "check isInTextRange method 2" (forAll (pack <$> arbitrary) (isInTextRangeCheck2 @a))

testUnicodeCharText :: forall a . (Arbitrary a, Eq a, Show a, Typeable a, UnicodeCharacter a, UnicodeText a) => SpecWith ()
testUnicodeCharText = describe (instanceTexts' @a ["UnicodeCharacter", "UnicodeText"]) $ do
  it "equivalent range checks" $ property (unicodeCharTextInRange @a)

unicodeCharTextInRange :: forall a . (UnicodeCharacter a, UnicodeText a) => Char -> Bool
unicodeCharTextInRange c = isInCharRange c == isInTextRange tc
  where tc = singleton c

testHashable :: forall a . (Arbitrary a, Eq a, Show a, Typeable a, Hashable a) => SpecWith ()
testHashable = describe (instanceText "Hashable" ++ instanceName (show (typeOf (undefined :: a)))) $ it "hashing law" $ (property (hashEquality @a))

instanceName :: String -> String
instanceName s | ' ' `elem` s = '(' : s ++ ")"
               | otherwise = s

mapOverChar :: forall a . (Eq a, UnicodeCharacter a) => a -> Bool
mapOverChar c = Just c == fromUnicodeChar (toUnicodeChar c)

isInCharRangeCheck :: forall a . UnicodeCharacter a => Char -> Bool
isInCharRangeCheck c = (isInCharRange @a) c == isJust ((fromUnicodeChar @a) c)

isInTextRangeCheck1 :: forall a . UnicodeText a => Char -> Bool
isInTextRangeCheck1 c = (isInTextRange @a) tc == isJust ((fromUnicodeText @a) tc)
  where tc = singleton c

isInTextRangeCheck2 :: forall a . UnicodeText a => Text -> Bool
isInTextRangeCheck2 tc = (isInTextRange @a) tc == isJust ((fromUnicodeText @a) tc)

mapOverItem :: forall a . UnicodeCharacter a => Char -> Bool
mapOverItem c = maybe True ((c ==) . toUnicodeChar @a) (fromUnicodeChar @a c)

equivalentMapping :: forall a . (Eq a, UnicodeCharacter a) => Char -> Bool
equivalentMapping c = (maybe True (fromUnicodeChar' @a c ==)) (fromUnicodeChar @a c)

mapValidItem :: forall a . UnicodeCharacter a => Property
mapValidItem = forAll (suchThat (arbitrary :: Gen Char) (isJust . fromUnicodeChar @a)) (mapOverItem @a)

equivalentFromChar :: forall a . (Eq a, UnicodeCharacter a) => Property
equivalentFromChar = forAll (suchThat (arbitrary :: Gen Char) (isJust . fromUnicodeChar @a)) (equivalentMapping @a)

mapOverText :: forall a . (Eq a, UnicodeText a) => a -> Bool
mapOverText c = Just c == fromUnicodeText (toUnicodeText c)

hashEquality :: (Eq a, Hashable a) => a -> a -> Bool
hashEquality ca cb = hash ca == hash cb || ca /= cb
