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
  testUnicodeText @ MoonPhase
  testHashable @ MoonPhase
  testMirrorVertically @ MoonPhase
