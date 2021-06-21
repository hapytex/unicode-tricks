{-# LANGUAGE TypeApplications #-}

module Data.Char.Emoji.ZodiacSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji.Zodiac
import Test.Hspec

spec :: Spec
spec = do
  testUnicodeCharacter @ Zodiac
  testUnicodeText @ Zodiac
  testHashable @ Zodiac
