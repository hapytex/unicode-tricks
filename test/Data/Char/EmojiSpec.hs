module Data.Char.EmojiSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji
import Test.Hspec

spec :: Spec
spec = do
  testUnicodeCharacter (undefined :: SkinColorModifier)
  testUnicodeCharacter (undefined :: MoonPhase)
  testUnicodeCharacter (undefined :: Zodiac)
  testUnicodeText (undefined :: SkinColorModifier)
  testUnicodeText (undefined :: Zodiac)
  testUnicodeText (undefined :: MoonPhase)
  testUnicodeText (undefined :: Gender)
  testUnicodeText (undefined :: Flag)
  testUnicodeText (undefined :: SubFlag)
