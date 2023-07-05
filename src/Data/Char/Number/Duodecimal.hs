{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Number.Duodecimal
-- Description : A module used to render duodecimal numbers.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The <https://www.unicode.org/charts/PDF/U2150.pdf 2150 unicode block> contains two characters for duodecimal numbers: numbers with base 12.
--
-- In order to represent digits for 10 and 11, unicode has two codepoints: @TURNED DIGIT TWO@, and @TURNED DIGIT THREE@. This module makes it
-- more convenient to convert an 'Integral' number to these digits, as well as converting a number to its duodecimal representation.
module Data.Char.Number.Duodecimal
  ( -- * Characters used for duodecimal numbers
    char10,
    char11,

    -- * Convert values to digits
    duodecimalDigit,
    duodecimalDigit',

    -- * Convert value to a sequence of digits
    duodecimalNumber,
    duodecimalNumber',
  )
where

import Data.Bits ((.|.))
import Data.Char (chr)
import Data.Char.Core (PlusStyle, positionalNumberSystem)
import Data.Default.Class (def)
import Data.Text (Text)

-- | The character used to denote 10: @ↀ@.
char10 ::
  -- | A character used in duodecimal numbers to denote 10.
  Char
char10 = '\x2180'

-- | The character used to denote 11: @ↁ@.
char11 ::
  -- | A character used in duodecimal numbers to denote 11.
  Char
char11 = '\x2181'

_duodecimalDigit :: Int -> Char
_duodecimalDigit n
  | n < 10 = chr (0x30 .|. n)
  | otherwise = chr (0x2180 .|. n)

-- | Convert the given 'Integral' number to its unicode character. In case the
-- value is less than @0@, or greater than @11@, the behavior is unspecified.
duodecimalDigit' ::
  Integral i =>
  -- | The given number to convert.
  i ->
  -- | A unicode character that represents this digit.
  Char
duodecimalDigit' = _duodecimalDigit . fromIntegral

-- | Convert the given 'Integral' number to its unicode character wrapped in a
-- 'Just' data constructor. In case the value is less than @0@ or greater than
-- @11@, 'Nothing' is returned.
duodecimalDigit :: Integral i => i -> Maybe Char
duodecimalDigit n
  | n >= 0 && n < 12 = Just (duodecimalDigit' n)
  | otherwise = Nothing

-- | Convert the given 'Integral' number to a 'Text' object that contains
-- a sequence of duodecimal digits that represent that number. The given
-- 'PlusStyle' specifies if the number is prefixed with @+@ if it is positive.
duodecimalNumber ::
  Integral i =>
  -- | The given 'PlusStyle' to use.
  PlusStyle ->
  -- | The given number to convert.
  i ->
  -- | A string of unicode characters representing the value in duodecimal notation.
  Text
duodecimalNumber = positionalNumberSystem 12 _duodecimalDigit '+' '-'

-- | Convert the given 'Integral' number to a 'Text' object that contains
-- sequence of duodecimal digits that represent that number.
duodecimalNumber' ::
  Integral i =>
  -- | The given number to convert.
  i ->
  -- | A string of unicode characters representing the value in duodecimal notation.
  Text
duodecimalNumber' = duodecimalNumber def
