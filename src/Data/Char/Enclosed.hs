{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Enclosed
Description : A module to map alphanumerical characters to their equivalent in an enclosed forms.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode defines two blocks of enclosed alphanumerical characters. The <http://unicode.org/charts/PDF/U2460.pdf U2460 block>, and the <http://unicode.org/charts/PDF/U1F100.pdf 1F100 block>.
This module aims to make it more convenient to map numbers, upper case and lower case values to their corresponding enclosed characters.
-}


module Data.Char.Enclosed (
    -- * Numbers with comma
    numberWithComma, numberWithComma'
    -- * Circled alphanumerical characters
  , circledAlpha,  circledAlpha'
  , circledNumber, circledNumber'
    -- * Parenthesized alphanumerical characters
  , parenthesizedAlpha,  parenthesizedAlpha'
  , parenthesizedNumber, parenthesizedNumber'
    -- * Numbers with period
  , numberWithPeriod, numberWithPeriod'
    -- * Double circled numbers
  , doubleCircledNumber, doubleCircledNumber'
    -- * Regional indicators
  , regionalIndicatorUppercase, regionalIndicatorUppercase'
    -- * Squared Latin letters
  , squaredUppercase, squaredUppercase'
    -- * White on black circled Latin letters
  , whiteOnBlackCircledUppercase, whiteOnBlackCircledUppercase'
    -- * White on black squared Latin letters
  , whiteOnBlackSquaredUppercase, whiteOnBlackSquaredUppercase'
    -- * White numbers on a black circle
  , numberWhiteOnBlackCircle, numberWhiteOnBlackCircle'
  ) where

import Data.Char.Core(liftNumber, liftNumber', liftNumberFrom, liftNumberFrom', liftUpperLowercase, liftUpperLowercase', liftDigit, liftDigit', liftUppercase, liftUppercase')


-- | Convert the given number to a 'Char'acter where the number is circled
-- wrapped in a 'Just'. This works for numbers in the @0-20@ range. For numbers
-- outside this range 'Nothing' is returned.
circledNumber
  :: Int  -- ^ The given number to convert.
  -> Maybe Char  -- ^ A 'Char'acter that is circled variant of the given number wrapped in a 'Just' data constructor if it exists; 'Nothing' otherwise.
circledNumber 0 = Just '\x24ea'
circledNumber n = liftNumber 20 0x245f n

-- | Convert the given number to a 'Char'acter where the number is circled.
-- This works for numbers in the @0-20@ range. For numbers outside this range,
-- the behavior is unspecified.
circledNumber'
  :: Int  -- ^ The given number to convert.
  -> Char  -- ^ A character that is the circled variant of the given number.
circledNumber' 0 = '\x24ea'
circledNumber' n = liftNumber' 0x245f n

-- | Convert the given upper case or lower case 'Char'acter to a character that
-- is circled. The result is wrapped in a 'Just' data constructor. If the value
-- is outside the @A-Z,a-z@ range, 'Nothing' is returned.
circledAlpha
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Maybe Char  -- ^ The corresponding 'Char'acter wrapped in a 'Just' data constructor. If outside the range, 'Nothing'.
circledAlpha = liftUpperLowercase 0x24b6 0x24d0

-- | Convert the given upper case or lower case 'Char'acter to a character that
-- is circled. If the value is outside the @A-Z,a-z@ range, the result is
-- unspecified.
circledAlpha'
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Char  -- ^ The corresponding 'Char'acter. If outside the range, unspecified result.
circledAlpha' = liftUpperLowercase' 0x24b6 0x24d0

-- | Convert the given number to a 'Char'acter where the number is
-- parenthesized wrapped in a 'Just' data constructor. If the number is outside
-- the @1-20@ range, 'Nothing' is returned.
parenthesizedNumber
  :: Int  -- ^ The given number to convert.
  -> Maybe Char  -- ^ The corresponding 'Char'acter wrapped in a 'Just' data constructor. If outside the range, 'Nothing'.
parenthesizedNumber = liftNumberFrom 1 20 0x2474

-- | Convert the given number to a 'Char'acter where the number is
-- parenthesized. If the number is outside the @1-20@ range, the result is
-- unspecified.
parenthesizedNumber'
  :: Int  -- ^ The given number to convert.
  -> Char  -- ^ The corresponding 'Char'acter. If outside the range, unspecified result.
parenthesizedNumber' = liftNumberFrom' 1 0x2474

-- | Convert the given number to a 'Char'acter where the number is succeeded by a
-- period (@.@) wrapped in a 'Just' data constructor. If the number is outside the
-- @0-20@ range, 'Nothing' is returned.
numberWithPeriod
  :: Int  -- ^ The given number to convert.
  -> Maybe Char  -- ^ The corresponding 'Char'acter wrapped in a 'Just' data constructor. If outside the range, 'Nothing'.
numberWithPeriod 0 = Just '\x1f100'
numberWithPeriod n = liftNumber 20 0x2487 n

-- | Convert the given number to 'Char'acter where the number is succeeded by a
-- period (@.@). If the number is outside the @0-20@ range, the result is
-- unspecified.
numberWithPeriod'
  :: Int  -- ^ The given number to convert.
  -> Char  -- ^ The corresponding 'Char'acter. If outside the range, unspecified result.
numberWithPeriod' 0 = '\x1f100'
numberWithPeriod' n = liftNumber' 0x2487 n

-- | Convert the given number to a 'Char'acter where the number is double
-- circled. The result is wrapped in a 'Just' data constructor.
-- If the given number is outside the @1-10@ range, 'Nothing' is returned.
doubleCircledNumber
  :: Int  -- ^ The given number to convert.
  -> Maybe Char  -- ^ The corresponding 'Char'acter wrapped in a 'Just' data constructor. If outside the range, 'Nothing'.
doubleCircledNumber = liftNumberFrom 1 10 0x24f5

-- | Convert the given number to a 'Char'acter where the number is double
-- circled. If the given number is outside the @1-10@ range, the result is
-- unspecified.
doubleCircledNumber'
  :: Int  -- ^ The given number to convert.
  -> Char  -- ^ The corresponding 'Char'acter. If outside the range, unspecified result.
doubleCircledNumber' = liftNumberFrom' 1 0x24f5

-- | Convert the given number to a 'Char'acter where the number is succeeded by
-- a comma (@,@). The result is wrapped in a 'Just' data constructor. If the
-- given number is outside the @0-9@ range, 'Nothing' is returned.
numberWithComma
  :: Int  -- ^ The given number to convert.
  -> Maybe Char  -- ^ The corresponding 'Char'acter wrapped in a 'Just' data constructor. If outside the range, 'Nothing'.
numberWithComma = liftDigit 0x1f101

-- | Convert the given number to a 'Char'acter where the number is succeeded by
-- a comma (@,@). If the given number is outside the @0-9@ range, the result is
-- unspecified.
numberWithComma'
  :: Int  -- ^ The given number to convert.
  -> Char  -- ^ The corresponding 'Char'acter. If outside the range, unspecified result.
numberWithComma' = liftDigit' 0x1f101

-- | Convert the given upper case or lower case 'Char'acter to a 'Char'acter
-- where it is parenthesized. The result is wrapped in a 'Just' data constructor.
-- If the value is outside the @A-Z,a-z@ range, 'Nothing' is returned.
parenthesizedAlpha
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Maybe Char  -- ^ The corresponding 'Char'acter wrapped in a 'Just' data constructor. If outside the range, 'Nothing'.
parenthesizedAlpha = liftUpperLowercase 0x1f110 0x249c

-- | Convert the given upper case or lower case character to a 'Char'acter
-- where it is parenthesized. If the value is outside the @A-Z,a-z@ range,
-- the result is unspecified.
parenthesizedAlpha'
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Char  -- ^ The corresponding 'Char'acter. If outside the range, unspecified result.
parenthesizedAlpha' = liftUpperLowercase' 0x1f110 0x249c

-- | Convert the given upper case character to a 'Char'acter where it is
-- squared (put in a square box). The result is wrapped in a 'Just' data
-- constructor. If the value is outside the @A-Z@ range, 'Nothing' is returned.
squaredUppercase
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Maybe Char  -- ^ The corresponding 'Char'acter wrapped in a 'Just' data constructor. If outside the range, 'Nothing'.
squaredUppercase = liftUppercase 0x1f130

-- | Convert the given upper case character to a 'Char'acter where it is squared
-- (put in a square box). If the value is outside the @A-Z@ range, the result is
-- unspecified.
squaredUppercase'
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Char  -- ^ The corresponding 'Char'acter. If outside the range, unspecified result.
squaredUppercase' = liftUppercase' 0x1f130

-- | Convert the given upper case character to a character where the character
-- is written in white on a black circle. The result is wrapped in a 'Just' data
-- constructor. If the given value is outside the @A-Z@ range, 'Nothing' is
-- returned.
whiteOnBlackCircledUppercase
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Maybe Char  -- ^ The corresponding 'Char'acter wrapped in a 'Just' data constructor. If outside the range, 'Nothing'.
whiteOnBlackCircledUppercase = liftUppercase 0x1f150

-- | Convert the given upper case character to a character where the character
-- is written in white on a black circle. If the given value is outside the
-- @A-Z@ range, the result is unspecified.
whiteOnBlackCircledUppercase'
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Char  -- ^ The corresponding 'Char'acter. If outside the range, unspecified result.
whiteOnBlackCircledUppercase' = liftUppercase' 0x1f150

-- | Convert the given upper case character to a character where the character
-- is written in white on a black square. The result is wrapped in a 'Just' data
-- constructor. If the given value is outside the @A-Z@ range, 'Nothing' is
-- returned.
whiteOnBlackSquaredUppercase
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Maybe Char  -- ^ The corresponding 'Char'acter wrapped in a 'Just' data constructor. If outside the range, 'Nothing'.
whiteOnBlackSquaredUppercase = liftUppercase 0x1f170

-- | Convert the given upper case character to a character where the character
-- is written in white on a black square. If the given value is outside the
-- @A-Z@ range, the result is unspecified.
whiteOnBlackSquaredUppercase'
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Char  -- ^ The corresponding 'Char'acter. If outside the range, unspecified result.
whiteOnBlackSquaredUppercase' = liftUppercase' 0x1f170

-- | Convert the given number to a character where the number is written in
-- white on a black circle. The result is wrapped in a 'Just' data constructor.
-- If the given value is outside the @0,11-20@ range, 'Nothing' is returned.
numberWhiteOnBlackCircle
  :: Int  -- ^ The given number to convert.
  -> Maybe Char  -- ^ The corresponding 'Char'acter wrapped in a 'Just' data constructor. If outside the range, 'Nothing'.
numberWhiteOnBlackCircle 0 = Just '\x24ff'
numberWhiteOnBlackCircle n = liftNumberFrom 11 20 0x24eb n

-- | Convert the given number to a character where the number is written in
-- white on a black circle. If the given value is outside the @0,11-20@ range,
-- the result is unspecified.
numberWhiteOnBlackCircle'
  :: Int  -- ^ The given number to convert.
  -> Char  -- ^ The corresponding 'Char'acter. If outside the range, unspecified result.
numberWhiteOnBlackCircle' 0 = '\x24ff'
numberWhiteOnBlackCircle' n = liftNumberFrom' 11 0x24eb n

-- | Convert the given upper case character to a regional indicator character.
-- The result is wrapped in a 'Just' data constructor. If the value is outside
-- the @A-Z@ range, 'Nothing' is returned. The regional indicators are used for
-- flag emojis. Two consecutive regional indicators that together form an ISO
-- 63166 Alpha-2 code, then this will result in the corresponding flag Emoji.
-- Deprecated countries like the Soviet Union (SU) and Yugoslavia (YU) do not
-- have a flag emoji. Antarctica (AQ), the European Union (EU) and the United
-- Nations (UN) have a flag emoji.
regionalIndicatorUppercase
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Maybe Char  -- ^ The corresponding 'Char'acter wrapped in a 'Just' data constructor. If outside the range, 'Nothing'.
regionalIndicatorUppercase = liftUppercase 0x1f1e6

-- | Convert the given upper case character to a regional indicator character.
-- If the value is outside the @A-Z@ range, the result is unspecified.
--
-- The regional indicators are used for flag emojis. Two consecutive regional
-- indicators that together form an ISO 63166 Alpha-2 code, then this will
-- result in the corresponding flag Emoji. Deprecated countries like the Soviet
-- Union (SU) and Yugoslavia (YU) do not have a flag emoji. Antarctica (AQ),
-- the European Union (EU) and the United Nations (UN) have a flag emoji.
regionalIndicatorUppercase'
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Char  -- ^ The corresponding 'Char'acter. If outside the range, unspecified result.
regionalIndicatorUppercase' = liftUppercase' 0x1f1e6
