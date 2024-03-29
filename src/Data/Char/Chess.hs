{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Chess
-- Description : Support for chess characters in unicode.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- One can make use of a <https://www.unicode.org/charts/PDF/U2600.pdf block 2600> and <https://www.unicode.org/charts/PDF/U1FA00.pdf block 1fa00> of Unicode characters to render chess characters. One can render chess characters as /netral/, /white/, or /black/
-- pieces, for such pieces one can render these rotated by 0, 90, 180 and 270 degrees. Knights can be rendered on 45, 135, 225 and 315 degrees as well. Furthermore unicode allows to render an /equihopper/, and special variants like a /knight-queen/, /knight-rook/, and /knight-bishop/.
--
-- The module contains pattern synonyms for names that are often given to the pieces.
module Data.Char.Chess
  ( -- * Data structures to represent the possible chess pieces.
    ChessColor (White, Black, Neutral),
    ChessColorBinary (BWhite, BBlack),
    ChessPieceType (King, Queen, Rook, Bishop, Knight, Pawn, Equihopper),
    ChessHybridType (KnightQueen, KnightRook, KnightBishop),
    ChessPiece (Chess90, Chess45Knight, ChessHybrid),
    Rotate45 (R45, R135, R225, R315),

    -- * Convert the chess piece to its unicode equivalent.
    chessPiece,

    -- * Pattern synonyms of special pieces
    pattern Grasshopper,
    pattern Nightrider,
    pattern Amazon,
    pattern Terror,
    pattern OmnipotentQueen,
    pattern Superqueen,
    pattern Chancellor,
    pattern Marshall,
    pattern Empress,
    pattern Cardinal,
    pattern Princess,
  )
where

import Control.DeepSeq (NFData)
import Data.Bits ((.|.))
import Data.Char (chr)
import Data.Char.Core (Rotate90 (R0, R180))
import Data.Data (Data)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary), arbitraryBoundedEnum)
import Test.QuickCheck.Gen (oneof)

-- | A data type that defined binary colors ('BWhite', and 'BBlack'), this is
-- used for special chess pieces like a /knight queen/, /knight rook/, and
-- /knight bishop/ that only have no neutral color in unicode.
data ChessColorBinary
  = -- | /White/ color.
    BWhite
  | -- | /Black/ color.
    BBlack
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable ChessColorBinary

instance NFData ChessColorBinary

-- | The color of a chess piece, this can for most pieces be 'Black', 'White',
-- or 'Neutral'.
data ChessColor
  = -- | /White/ color.
    White
  | -- | /Black/ color.
    Black
  | -- | Neutral chess pieces, sometimes depicted half /white/ and half /black/.
    Neutral
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable ChessColor

instance NFData ChessColor

-- | The type of chess pieces. Unicode includes an 'Equihopper' as piece as
-- well.
data ChessPieceType
  = -- | The /king/ chess piece.
    King
  | -- | The /queen/ chess piece.
    Queen
  | -- | The /rook/ chess piece.
    Rook
  | -- | The /bishop/ chess piece.
    Bishop
  | -- | The /knight/ chess piece.
    Knight
  | -- | The /pawn/ chess piece.
    Pawn
  | -- | The /equihopper/ chess piece.
    Equihopper
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable ChessPieceType

instance NFData ChessPieceType

-- | Extra rotations that can be performed for knight chess pieces.
data Rotate45
  = -- | Rotation over /45/ degrees.
    R45
  | -- | Rotation over /135/ degrees.
    R135
  | -- | Rotation over /225/ degrees.
    R225
  | -- | Rotation over /315/ degrees.
    R315
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable Rotate45

instance NFData Rotate45

-- | Hybrid chess pieces like the /knight-queen/, /knight-rook/ and
-- /knight-bishop/.
data ChessHybridType
  = -- | The /knight-queen/ chess piece.
    KnightQueen
  | -- | The /knight-rook/ chess piece.
    KnightRook
  | -- | The /knight-bishop/ chess piece.
    KnightBishop
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable ChessHybridType

instance NFData ChessHybridType

-- | Chess pieces that can be represented in Unicode. These are the /king/,
-- /queen/, /rook/, /bishop/, /knight/, /pawn/, and /equihopper/ over 0, 90,
-- 180, and 270 degrees; and the /knight/ over /45/, /135/, /225/, and /315/
-- degrees in 'Black', 'White' and 'Neutral'.
-- Furthermore one can draw a /knight-queen/, /knight-rook/, and /knight-bishop/
-- pieces can be drawn without rotation and only in 'BBlack' or 'BWhite'.
data ChessPiece
  = -- | Standard pieces drawn in /black/, /white/, or /neutral/ and with rotation.
    Chess90 ChessColor ChessPieceType Rotate90
  | -- | /Knights/ have unicode characters to render these rotated over /45/, /135/, /225/ and /315/ degrees.
    Chess45Knight ChessColor Rotate45
  | -- | Hybrid chess pieces can only be rendered in 'BBlack' and 'BWhite'.
    ChessHybrid ChessHybridType ChessColorBinary
  deriving (Data, Eq, Generic, Ord, Read, Show)

instance Hashable ChessPiece

instance NFData ChessPiece

instance Arbitrary ChessColorBinary where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ChessColor where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ChessPieceType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ChessHybridType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Rotate45 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ChessPiece where
  arbitrary = oneof [Chess90 <$> arbitrary <*> arbitrary <*> arbitrary, Chess45Knight <$> arbitrary <*> arbitrary, ChessHybrid <$> arbitrary <*> arbitrary]

-- | A /grasshopper/ is a /queen/ rotated over 180 degrees.
pattern Grasshopper :: ChessColor -> ChessPiece
pattern Grasshopper c = Chess90 c Queen R180

-- | A /Nightrider/ is a /knight/ rotated over 180 degrees.
pattern Nightrider :: ChessColor -> ChessPiece
pattern Nightrider c = Chess90 c Knight R180

-- | An /amazon/ is alterative name for a /knight-queen/.
pattern Amazon :: ChessColorBinary -> ChessPiece
pattern Amazon c = ChessHybrid KnightQueen c

-- | A /terror/ is alterative name for a /knight-queen/.
pattern Terror :: ChessColorBinary -> ChessPiece
pattern Terror c = ChessHybrid KnightQueen c

-- | An /omnipotent queen/ is alterative name for a /knight-queen/.
pattern OmnipotentQueen :: ChessColorBinary -> ChessPiece
pattern OmnipotentQueen c = ChessHybrid KnightQueen c

-- | A /superqueen/ is alterative name for a /knight-queen/.
pattern Superqueen :: ChessColorBinary -> ChessPiece
pattern Superqueen c = ChessHybrid KnightQueen c

-- | A /chancellor/ is alterative name for a /knight-rook/.
pattern Chancellor :: ChessColorBinary -> ChessPiece
pattern Chancellor c = ChessHybrid KnightRook c

-- | A /marshall/ is alterative name for a /knight-rook/.
pattern Marshall :: ChessColorBinary -> ChessPiece
pattern Marshall c = ChessHybrid KnightRook c

-- | An /empress/ is alterative name for a /knight-rook/.
pattern Empress :: ChessColorBinary -> ChessPiece
pattern Empress c = ChessHybrid KnightRook c

-- | A /cardinal/ is alterative name for a /knight-bishop/.
pattern Cardinal :: ChessColorBinary -> ChessPiece
pattern Cardinal c = ChessHybrid KnightBishop c

-- | A /princess/ is alterative name for a /knight-bishop/.
pattern Princess :: ChessColorBinary -> ChessPiece
pattern Princess c = ChessHybrid KnightBishop c

_chessValue :: ChessPieceType -> ChessColor -> Int
_chessValue t c = 6 * fromEnum c + fromEnum t

-- | Convert the given 'ChessPiece' to the corresponding unicode character.
chessPiece ::
  -- | The given 'ChessPiece' to convert.
  ChessPiece ->
  -- | The unicode character that represents the given 'ChessPiece'.
  Char
chessPiece (Chess90 c Equihopper r) = chr (3 * mod (fromEnum r) 2 + fromEnum c + 0x1fa48)
chessPiece (Chess90 Neutral t R0) = chr (0x1fa00 .|. fromEnum t)
chessPiece (Chess90 c t R0) = chr (_chessValue t c + 0x2654)
chessPiece (Chess90 c t r) = chr (0x15 * fromEnum r + _chessValue t c + 0x1f9f4)
chessPiece (Chess45Knight c r) = chr (0x15 * fromEnum r + fromEnum c + 0x1fa06)
chessPiece (ChessHybrid t c) = chr (3 * fromEnum c + fromEnum t + 0x1fa4e)
