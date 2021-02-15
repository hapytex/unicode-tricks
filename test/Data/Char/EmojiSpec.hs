module Data.Char.EmojiSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji
import Test.Hspec

spec :: Spec
spec = do
  testUnicodeCharacter "SkinColorModifier" (undefined :: SkinColorModifier)
  testUnicodeCharacter "MoonPhase" (undefined :: MoonPhase)
  testUnicodeCharacter "Zodiac" (undefined :: Zodiac)
  testUnicodeCharacter "Clock" (undefined :: Clock)
  testUnicodeCharacter "Fruit" (undefined :: Fruit)
  testUnicodeCharacter "Vegetable" (undefined :: Vegetable)
  testUnicodeCharacter "Bird" (undefined :: Bird)
  testUnicodeText "EmojiColoredShape" (undefined :: EmojiColoredShape)
  testUnicodeText "SkinColorModifier" (undefined :: SkinColorModifier)
  testUnicodeText "MoonPhase" (undefined :: MoonPhase)
  testUnicodeText "Zodiac" (undefined :: Zodiac)
  testUnicodeText "Clock" (undefined :: Clock)
  testUnicodeText "Gender" (undefined :: Gender)
  testUnicodeText "Flag" (undefined :: Flag)
  testUnicodeText "SubFlag" (undefined :: SubFlag)
  testUnicodeText "BloodType" (undefined :: BloodType)
  testUnicodeText "Fruit" (undefined :: Fruit)
  testUnicodeText "Vegetable" (undefined :: Vegetable)
  testUnicodeText "Bird" (undefined :: Bird)
  testUnicodeText "Plant" (undefined :: Plant)
