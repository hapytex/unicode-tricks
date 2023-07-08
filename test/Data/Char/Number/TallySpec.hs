{-# LANGUAGE TypeApplications #-}

module Data.Char.Number.TallySpec
  ( spec
  ) where

import Data.Char.Number.Tally
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter @TallyLiteral
    testUnicodeText @TallyLiteral
    testUnicodeCharText @TallyLiteral
    testHashable @TallyLiteral
