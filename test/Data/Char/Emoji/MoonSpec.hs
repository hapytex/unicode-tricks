{-# LANGUAGE TypeApplications #-}

module Data.Char.Emoji.MoonSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji.Moon
import Test.Hspec

spec :: Spec
spec = do
  testUnicodeCharacter @ MoonPhase
  testUnicodeCharacter @ MoonFace
  testUnicodeText @ MoonPhase
  testUnicodeText @ MoonFace
  testHashable @ MoonPhase
  testHashable @ MoonFace
  testMirrorVertically @ MoonPhase
  testMirrorVertically @ MoonFace
