module Data.Char.BrailleSpec
  ( spec
  ) where

import Data.Char.Braille
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter (undefined :: Braille Bool)
    testUnicodeCharacter (undefined :: Braille6 Bool)
