{-# LANGUAGE TypeApplications #-}

module Data.Char.Emoji.GenderSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji.Gender
import Test.Hspec

spec :: Spec
spec = do
  testUnicodeCharacter @ Gender
  testUnicodeText @ Gender
  testHashable @ Gender
  testMirrorVertically @ Gender
