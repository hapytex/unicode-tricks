{-# LANGUAGE TypeApplications #-}

module Data.Char.Emoji.NoEvilMonkeySpec
  ( spec
  ) where

import Data.Char.Emoji.NoEvilMonkey
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter @NoEvilMonkey
    testUnicodeText @NoEvilMonkey
    testUnicodeCharText @NoEvilMonkey
    testHashable @NoEvilMonkey
