{-# LANGUAGE TypeApplications #-}

module Data.Char.Emoji.ScienceSpec
  ( spec
  ) where

import Data.Char.CoreTest
import Data.Char.Emoji.Science
import Test.Hspec

spec :: Spec
spec = do
  testUnicodeText @ScienceEmoji
  testHashable @ScienceEmoji
