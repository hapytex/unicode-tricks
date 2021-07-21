{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings, Safe #-}

{-|
Module      : Data.Char.Emoji.Science
Description : A module to render and parse emoji related to science.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode defines nine Emoji related to science. This module has a data type together with functions
to convert this to and from a text fragment.
-}

module Data.Char.Emoji.Science (
    -- * Defining science emoji.
    ScienceEmoji(Alembic, TestTube, PetriDish, DnaDoubleHelix, Microscope, Telescope, SatelliteAntenna)
  ) where

import Control.DeepSeq(NFData)

import Data.Char(chr, ord)
import Data.Char.Core(UnicodeText(toUnicodeText, fromUnicodeText))
import Data.Data(Data)
import Data.Hashable(Hashable)
import Data.Text(singleton, unpack)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

-- | There are nine emoji that depict science.
data ScienceEmoji
  = Alembic  -- An /alembic/ is an apparatus for destillation. Normally this is depicted as ⚗️.
  | TestTube  -- ^ A /test tube/ is used to conduct chemical experiments. Normally this is depicted as 🧪.
  | PetriDish  -- ^ A /petri dish/ is used to culture microbes. Normally this is depicted as 🧫.
  | DnaDoubleHelix  -- ^ A double helix of DNA is the genetic blueprint of life. Normally this is depicted as 🧬.
  | Microscope  -- ^ A /microscope/ is used to magnify small objects. Normally this is depicted as 🔬.
  | Telescope  -- ^ A /telescope/ is used to gaze at stars and planets in the night sky. Normally this is depicted as 🔭.
  | SatelliteAntenna  -- ^ A /dish satellite/ is used to send and receive to or from communication satellites. This is normally depicited as 📡.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary ScienceEmoji where
  arbitrary = arbitraryBoundedEnum

instance Hashable ScienceEmoji

instance NFData ScienceEmoji

instance UnicodeText ScienceEmoji where
  toUnicodeText Alembic = "\x2697\xfe0f"
  toUnicodeText Microscope = "\x1f52c"
  toUnicodeText Telescope = "\x1f52d"
  toUnicodeText SatelliteAntenna = "\x1f4e1"
  toUnicodeText x = singleton (chr (fromEnum x + 0x1f9e9))
  fromUnicodeText "\x2697\xfe0f" = Just Alembic
  fromUnicodeText "\x1f52c" = Just Microscope
  fromUnicodeText "\x1f52d" = Just Telescope
  fromUnicodeText "\x1f4e1" = Just SatelliteAntenna
  fromUnicodeText "\x1f9ea" = Just TestTube
  fromUnicodeText "\x1f9eb" = Just PetriDish
  fromUnicodeText t
    | [c] <- unpack t, '\x1f9ea' <= c && c <= '\x1f9ec' = Just (toEnum (ord c - 0x1f9e9))
    | otherwise = Nothing
