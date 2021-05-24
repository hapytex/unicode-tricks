{-# LANGUAGE TypeApplications #-}

module Data.Char.EmojiSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji
import Test.Hspec

spec :: Spec
spec = do
  testUnicodeCharacter @ SkinColorModifier
  testUnicodeCharacter @ MoonPhase
  testUnicodeCharacter @ Zodiac
  testUnicodeCharacter @ Clock
  testUnicodeText @ SkinColorModifier
  testUnicodeText @ MoonPhase
  testUnicodeText @ Zodiac
  testUnicodeText @ Clock
  testUnicodeText @ Gender
  testUnicodeText @ Flag
  testUnicodeText @ SubFlag
  testUnicodeText @ BloodType
  testHashable @ SkinColorModifier
  testHashable @ MoonPhase
  testHashable @ Zodiac
  testHashable @ Clock
  testHashable @ Gender
  testHashable @ Flag
  testHashable @ SubFlag
  testHashable @ BloodType
