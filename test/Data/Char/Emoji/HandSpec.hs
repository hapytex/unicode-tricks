{-# LANGUAGE TypeApplications #-}

module Data.Char.Emoji.HandSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji.Hand
import Test.Hspec

spec :: Spec
spec = do
  testUnicodeCharacter @ HandGesture
  -- testUnicodeCharacter @ MoonFace
  -- testUnicodeText @ MoonPhase
  -- testUnicodeText @ MoonFace
  testHashable @ HandGesture
  -- testHashable @ MoonFace
