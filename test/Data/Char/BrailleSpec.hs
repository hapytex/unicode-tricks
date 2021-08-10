{-# LANGUAGE TypeApplications #-}

module Data.Char.BrailleSpec
  ( spec
  ) where

import Data.Char.Braille
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter @(Braille Bool)
    testUnicodeCharacter @(Braille6 Bool)
    testUnicodeText @(Braille Bool)
    testUnicodeText @(Braille6 Bool)
    testUnicodeCharText @(Braille Bool)
    testUnicodeCharText @(Braille6 Bool)
    testHashable @(Braille Bool)
    testHashable @(Braille6 Bool)
    testMirrorHorizontallyVertically @(Braille6 Bool)
    testMirrorHorizontallyVertically @(Braille Bool)
