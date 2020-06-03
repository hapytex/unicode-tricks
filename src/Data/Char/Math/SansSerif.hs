{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Math.SansSerif
Description : Sans serif mathematical alphanumeric symbols
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Data.Char.Math.SansSerif
  ( -- * Sans-serif mathematical alphabet symbols
    sansSerif,               sansSerif'
  , sansSerifNoBold,         sansSerifNoBold'
  , sansSerifBold,           sansSerifBold'
  , sansSerifNoItalic,       sansSerifNoItalic'
  , sansSerifItalic,         sansSerifItalic'
  , sansSerifNoBoldNoItalic, sansSerifNoBoldNoItalic'
  , sansSerifBoldNoItalic,   sansSerifBoldNoItalic'
  , sansSerifNoBoldItalic,   sansSerifNoBoldItalic'
  , sansSerifBoldItalic,     sansSerifBoldItalic'
    -- * Digit characters
    -- ** Character conversion
  , digitSansSerif,        digitSansSerif'
  , digitSansSerifRegular, digitSansSerifRegular'
  , digitSansSerifBold,    digitSansSerifBold'
    -- ** Int to digit characters
  , intToDigitSansSerif,        intToDigitSansSerif'
  , intToDigitSansSerifRegular, intToDigitSansSerifRegular'
  , intToDigitSansSerifBold,    intToDigitSansSerifBold'
  ) where


import Data.Char.Math.SansSerif.Digit
import Data.Char.Math.SansSerif.Latin
