{-# LANGUAGE TypeApplications #-}

module Data.Char.Emoji.FlagSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji.Flag
import Test.Hspec

spec :: Spec
spec = do
  testUnicodeText @Flag
  testUnicodeText @SubFlag
  testUnicodeText @ExtraFlag
  testBounded @Flag
  testBounded @SubFlag
  testBounded @ExtraFlag
  testHashable @Flag
  testHashable @SubFlag
  testHashable @ExtraFlag
