{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Math.Monospace
-- Description : Monospaced mathematical alphanumeric symbols
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- See "Data.Char.Math" for further documentation.
module Data.Char.Math.Monospace
  ( -- * Letters
    monospace,
    monospace',

    -- * Digits
    digitMonospace,
    digitMonospace',
    intToDigitMonospace,
    intToDigitMonospace',
  )
where

import Data.Char (intToDigit, isDigit)
import Data.Char.Core (isAsciiAlphaNum)
import Data.Char.Math.Internal

-- | Convert the given character to its /monospace/ equivalent for the alphabet
-- and numerical range (@A@–@Z@, @a@–@z@, and @0@–@9@). For characters outside
-- the range, the result is unspecified.
monospace' ::
  -- | The given character to convert to a /monospace/ symbol.
  Char ->
  -- | The equivalent monospace symbol for the given character.
  Char
monospace' = _baseUpperLowerNum 0x1d7c6 0x1d629

-- | Convert the given character to its /monospace/ equivalent for the alphabet
-- and numerical range (@A@–@Z@, @a@–@z@, and @0@–@9@) wrapped in a 'Just' data
-- constructor. For characters outside the range, 'Nothing' is returned.
monospace ::
  -- | The given character to convert to a /monspace/ symbol.
  Char ->
  -- | The equivalent monospace character wrapped in a 'Just' data constructor, 'Nothing' if outside the alphanumerical range.
  Maybe Char
monospace = _withCondition isAsciiAlphaNum monospace'

-- | Convert the given number (@0@–@9@) to its corresponding character in
-- /monospace/ style. Unspecified result for numbers outside this range.
intToDigitMonospace' ::
  -- | The given number to convert.
  Int ->
  -- | The corresponding character in monspace style. Unspecified outside the digit range.
  Char
intToDigitMonospace' = digitMonospace' . intToDigit

-- | Convert the given number (@0@–@9@) to its corresponding character
-- in /monospace/ style wrapped in a 'Just' data constructor. For
-- numbers outside this range, 'Nothing' is returned.
intToDigitMonospace ::
  -- | The given number to convert.
  Int ->
  -- | The corresponding symbol in /monospace/ style wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
intToDigitMonospace = _withCondition _isValidInt intToDigitMonospace'

-- | Converts the given digit (@0@–@9@) charcters to its equivalent in
-- /monospace/ style. Unspecified result for characters outside the range.
digitMonospace' ::
  -- | The given digit character to convert.
  Char ->
  -- | The corresponding character in monspace style. Unspecified outside the digit range.
  Char
digitMonospace' = monospace'

-- | Converts the given digit (@0@–@9@) charcters to its equivalent in
-- /monospace/ style wrapped in a 'Just' data constructor. 'Nothing'
-- for characters outside the range.
digitMonospace ::
  -- | The given digit character to convert.
  Char ->
  -- | The corresponding symbol in monospace style wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
digitMonospace = _withCondition isDigit digitMonospace'
