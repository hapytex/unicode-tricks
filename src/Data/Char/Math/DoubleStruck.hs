{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Math.DoubleStruck
Description : Double struck mathematical alphanumeric symbols
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Data.Char.Math.DoubleStruck
  ( -- * Letters
    doubleStruck,           doubleStruck'
    -- * Digits
  , digitDoubleStruck,      digitDoubleStruck'
  , intToDigitDoubleStruck, intToDigitDoubleStruck'
  ) where

import Data.Char(intToDigit, isDigit)
import Data.Char.Core(isAsciiAlphaNum)
import Data.Char.Math.Internal

-- | Obtain the double struck symbol for the given character. The supported
-- range of characters are the alphabet character (@A@-@Z@, and @a@-@z@), and
-- the numerical characters (@0@-@9@). For characters other than these, the
-- behaviour is unspecified.
doubleStruck'
  :: Char  -- ^ The character to convert to a /double struck/ symbol.
  -> Char  -- ^ The double struck symbol for the given character. If the character
          -- is not an ASCII alphanumerical character, the result is
          -- unspecified.
doubleStruck' 'C' = '\x2102'
doubleStruck' 'H' = '\x210d'
doubleStruck' 'N' = '\x2115'
doubleStruck' 'P' = '\x2119'
doubleStruck' 'Q' = '\x211a'
doubleStruck' 'R' = '\x211d'
doubleStruck' 'Z' = '\x2124'
doubleStruck' c = _baseUpperLowerNum 0x1d7a8 0x1d4f1 c

-- | Obtain the double struck symbol for the given character. The supported
-- range of characters are the alphabet characters (@A@-@Z@, and @a@-@z@), and
-- the numerical characters (@0@-@9@). The symbols are wrapped in the 'Just'
-- data constructor. For characters outside the range, 'Nothing' is returned.
doubleStruck
  :: Char  -- ^ The character to convert to a /double struck/ symbol.
  -> Maybe Char  -- ^ The double struck symbol for the given character wrapped
                -- in a 'Just' data constructor, 'Nothing' if there is no
                -- equivalent /double stuck/ character.
doubleStruck = _withCondition isAsciiAlphaNum doubleStruck'

-- | Convert the given number (@0@-@9@) to its corresponding character in
-- /double-struck/ style. Unspecified result for numbers outside this range.
intToDigitDoubleStruck'
  :: Int  -- ^ The given number to convert.
  -> Char  -- ^ The corresponding character in double-struck style. Unspecified outside the digit range.
intToDigitDoubleStruck' = digitDoubleStruck' . intToDigit

-- | Convert the given number (@0@-@9@) to its corresponding character
-- in /double-struck/ style wrapped in a 'Just' data constructor. For
-- numbers outside this range, 'Nothing' is returned.
intToDigitDoubleStruck
  :: Int  -- ^ The given number to convert.
  -> Maybe Char  -- ^ The corresponding symbol in /monospace/ style wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
intToDigitDoubleStruck = _withCondition _isValidInt intToDigitDoubleStruck'

-- | Converts the given digit (@0@-@9@) charcters to its equivalent in
-- /double-struck/ style. Unspecified result for characters outside the range.
digitDoubleStruck'
  :: Char  -- ^ The given digit character to convert.
  -> Char  -- ^ The corresponding character in double-struck style. Unspecified outside the digit range.
digitDoubleStruck' = doubleStruck'

-- | Converts the given digit (@0@-@9@) charcters to its equivalent in
-- /double-struck/ style wrapped in a 'Just' data constructor. 'Nothing'
-- for characters outside the range.
digitDoubleStruck
  :: Char  -- ^ The given digit character to convert.
  -> Maybe Char  -- ^ The corresponding symbol in double-struck style wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
digitDoubleStruck = _withCondition isDigit digitDoubleStruck'