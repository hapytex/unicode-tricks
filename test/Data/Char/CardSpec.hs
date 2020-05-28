module Data.Char.CardSpec
  ( spec
  ) where

import Data.Char.Card
import Data.Char.CoreTest
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = testUnicodeCharacter (undefined :: CardSuit)
