{-# LANGUAGE TypeApplications #-}

module Data.Char.BlockSpec
  ( spec
  ) where

import Data.Char.Block
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter @ (Block Bool)
    testUnicodeText @ (Block Bool)
