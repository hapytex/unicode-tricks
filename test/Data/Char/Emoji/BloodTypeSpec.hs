{-# LANGUAGE TypeApplications #-}

module Data.Char.Emoji.BloodTypeSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji.BloodType
import Test.Hspec

spec :: Spec
spec = do
  testUnicodeText @BloodType
  testHashable @BloodType
