{-# LANGUAGE TypeApplications #-}

module Data.Char.Emoji.ClockSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji.Clock
import Test.Hspec

spec :: Spec
spec = do
  testUnicodeCharacter @ SkinColorModifier
  testUnicodeText @ SkinColorModifier
  testHashable @ SkinColorModifier
