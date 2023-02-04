{-# LANGUAGE TypeApplications #-}

module Data.Char.Emoji.HandSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji.Hand
import Test.Hspec

spec :: Spec
spec = do
  testUnicodeCharacter @SingleCharHandGesture
  testUnicodeText @SingleCharHandGesture
  testHashable @SingleCharHandGesture
  -- testUnicodeCharacter @MoonFace
  -- testUnicodeText @MoonFace
  -- testHashable @MoonFace
  testUnicodeText @MultiCharHandGesture
  testHashable @MultiCharHandGesture
