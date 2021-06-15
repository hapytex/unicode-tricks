{-# LANGUAGE TypeApplications #-}

module Data.Char.ChessSpec
  ( spec
  ) where

import Data.Char.Chess
import Data.Char.CoreTest

import Test.Hspec

spec :: Spec
spec = do
  testHashable @ ChessColorBinary
  testHashable @ ChessColor
  testHashable @ ChessPieceType
  testHashable @ Rotate45
  testHashable @ ChessHybridType
  testHashable @ ChessPiece
