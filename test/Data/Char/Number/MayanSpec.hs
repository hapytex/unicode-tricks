{-# LANGUAGE TypeApplications #-}

module Data.Char.Number.MayanSpec
  ( spec
  ) where

import Data.Char.Number.Mayan(MayanLiteral)
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter @ MayanLiteral
    testUnicodeText @ MayanLiteral
    testHashable @ MayanLiteral
