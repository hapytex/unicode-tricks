{-# LANGUAGE TypeApplications #-}

module Data.Char.Emoji.SkinColorSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji.SkinColor
import Test.Hspec

spec :: Spec
spec = do
  testUnicodeCharacter @SkinColorModifier
  testUnicodeText @SkinColorModifier
  testUnicodeCharText @SkinColorModifier
  testHashable @SkinColorModifier
