module Data.Char.CardSpec
  ( spec
  ) where

import Data.Char.Card
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter "CardSuit" (undefined :: CardSuit)
    testUnicodeText "CardSuit" (undefined :: CardSuit)
