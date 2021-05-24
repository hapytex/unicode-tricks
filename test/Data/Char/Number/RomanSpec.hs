{-# LANGUAGE TypeApplications #-}

module Data.Char.Number.RomanSpec
  ( spec
  ) where

import Data.Char.Number.Roman
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter @ RomanLiteral
    testUnicodeText @ RomanLiteral
    testHashable @ RomanLiteral
