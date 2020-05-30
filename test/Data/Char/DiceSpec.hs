module Data.Char.DiceSpec
  ( spec
  ) where

import Data.Char.Dice
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter (undefined :: DieValue)
    testUnicodeText (undefined :: DieValue)
