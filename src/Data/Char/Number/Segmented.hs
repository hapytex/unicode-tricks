{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Number.Segmented
Description : A module used to render segmented numbers.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

The <https://www.unicode.org/charts/PDF/U1FB00.pdf 1FB00 Unicode block> contains symbols for legacy computing
and among them segmented digits: &#x1fbf0;&#x1fbf2;&#x1fbf3;&#x1fbf4;&#x1fbf5;&#x1fbf6;&#x1fbf7;&#x1fbf8;&#x1fbf9;
-}

module Data.Char.Number.Segmented (
    -- * Convert numbers to a seven-segmented display
    segmentedDigit, segmentedDigit'
  ) where

import Data.Char(chr)

_segmentedDigit :: Int -> Char
_segmentedDigit n
  | 0 <= n && n <= 9 = chr (0x1fbf0 + n)
  | otherwise = '\xfffd'

-- | Convert the given 'Integral' number to its Unicode character. In case the
-- value is less than @0@, or greater than @9@, the behavior is unspecified.
segmentedDigit' :: Integral i
  => i  -- ^ The given number to convert.
  -> Char  -- ^ A Unicode character that represents this digit.
segmentedDigit' = _segmentedDigit . fromIntegral

-- | Convert the given 'Integral' number to its Unicode character wrapped in a
-- 'Just' data constructor. In case the value is less than @0@ or greater than
-- @9@, 'Nothing' is returned.
segmentedDigit :: Integral i
  => i  -- ^ The given number to convert.
  -> Maybe Char  -- ^ A Unicode character that represents the given digit on a /seven-segment display/ wrapped in a 'Just' data constructor. If the number is outside the @0-9@ range, then 'Nothing' is returned.
segmentedDigit n
  | n >= 0 && n <= 9 = Just (segmentedDigit' n)
  | otherwise = Nothing
