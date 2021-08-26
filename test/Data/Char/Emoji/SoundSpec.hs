{-# LANGUAGE TypeApplications #-}

module Data.Char.Emoji.SoundSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji.Sound
import Test.Hspec

spec :: Spec
spec = do
  testBounded @Volume
  testUnicodeCharacter @Volume
  testUnicodeText @Volume
  testHashable @Volume
  testBounded @Bell
  testUnicodeCharacter @Bell
  testUnicodeText @Bell
  testHashable @Bell
  {-
  testBounded @Sound
  testUnicodeCharacter @Sound
  testUnicodeText @Sound
  testHashable @Sound
  -}
