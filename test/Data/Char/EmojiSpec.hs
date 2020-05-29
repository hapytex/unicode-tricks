module Data.Char.EmojiSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji
import Test.Hspec

spec :: Spec
spec = do
  testUnicodeCharacter (undefined :: SkinColorModifier)
  testUnicodeCharacter (undefined :: Zodiac)
