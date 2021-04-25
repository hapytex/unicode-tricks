{-# LANGUAGE TypeApplications #-}

module Data.Char.FrameSpec
  ( spec
  ) where

import Data.Char.Frame
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter @ (Parts Weight)
    testUnicodeCharacter @ (Parts Bool)
    testUnicodeText @ (Parts Weight)
    testUnicodeText @ (Parts Bool)
