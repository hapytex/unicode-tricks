module Data.Char.Number.RomanSpec
  ( spec
  ) where

import Data.Char.Number.Roman
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter (undefined :: RomanLiteral)
    testUnicodeText (undefined :: RomanLiteral)
