{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Number.Segmented
Description : A module used to render segmented numbers.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

The <https://www.unicode.org/charts/PDF/U1FB00.pdf 1FB00 unicode block> contains symbols for legacy computing
and among them segmented digits: 🯰🯱🯲🯳🯴🯵🯶🯷🯸🯹.
-}

module Data.Char.Number.Segmented
  ( segmentedDigit, segmentedDigit'
  ) where


import Data.Char (chr)

_segmentedDigit :: Int -> Char
_segmentedDigit n
  | 0 <= n && n <= 9 = chr (0x1FBF0 + n)
  | otherwise        = '�'

-- | Convert the given 'Integral' number to its unicode character. In case the
-- value is less than @0@, or greater than @9@, the behavior is unspecified.
segmentedDigit' :: Integral i
  => i  -- ^ The given number to convert.
  -> Char  -- ^ A unicode character that represents this digit.
segmentedDigit' = _segmentedDigit . fromIntegral

-- | Convert the given 'Integral' number to its unicode character wrapped in a
-- 'Just' data constructor. In case the value is less than @0@ or greater than
-- @9@, 'Nothing' is returned.
segmentedDigit :: Integral i => i -> Maybe Char
segmentedDigit n
  | n >= 0 && n < 9 = Just (segmentedDigit' n)
  | otherwise       = Nothing
