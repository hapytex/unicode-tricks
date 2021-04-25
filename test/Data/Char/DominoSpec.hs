module Data.Char.DominoSpec
  ( spec
  ) where

import Data.Char.Core
import Data.Char.Dice
import Data.Char.Domino
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter (undefined :: Oriented (Domino (Maybe DieValue)))
    testUnicodeCharacter (undefined :: Oriented (Domino DieValue))
    testUnicodeText (undefined :: Oriented (Domino (Maybe DieValue)))
    testUnicodeText (undefined :: Oriented (Domino DieValue))
