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
  testUnicodeCharacter @ Clock
  testUnicodeText @ SkinColorModifier
  testUnicodeText @ MoonPhase
  testUnicodeText @ Clock
  testUnicodeText @ Gender
  testBounded @Clock
  testHashable @ SkinColorModifier
  testHashable @ MoonPhase
  testHashable @ Clock
  testHashable @ Gender
  testMirrorVertically @ MoonPhase
