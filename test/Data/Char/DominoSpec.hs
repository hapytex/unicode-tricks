{-# LANGUAGE TypeApplications #-}

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
    testUnicodeCharacter @ (Oriented (Domino (Maybe DieValue)))
    testUnicodeCharacter @ (Oriented (Domino DieValue))
    testUnicodeText @ (Oriented (Domino (Maybe DieValue)))
    testUnicodeText @ (Oriented (Domino DieValue))
