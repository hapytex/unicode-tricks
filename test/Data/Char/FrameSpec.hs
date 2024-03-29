{-# LANGUAGE TypeApplications #-}

module Data.Char.FrameSpec
  ( spec
  ) where

import Data.Char.Frame
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter @(Parts Weight)
    testUnicodeCharacter @(Parts Bool)
    testUnicodeText @(Parts Weight)
    testUnicodeText @(Parts Bool)
    testUnicodeCharText @(Parts Weight)
    testUnicodeCharText @(Parts Bool)
    testHashable @(Parts Weight)
    testHashable @(Parts Bool)
    testHashable @(Horizontal Bool)
    testHashable @(Vertical Bool)
    testMirrorHorizontally @(Vertical Bool)
    testMirrorHorizontallyVertically @(Parts Bool)
    testMirrorVertically @(Horizontal Bool)
