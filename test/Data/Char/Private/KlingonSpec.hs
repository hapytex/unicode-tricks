{-# LANGUAGE TypeApplications #-}

module Data.Char.Private.KlingonSpec
  ( spec
  ) where

import Data.Char.Private.Klingon
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter @ Klingon
    testUnicodeText @ Klingon
