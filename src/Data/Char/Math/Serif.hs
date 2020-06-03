{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Math.Serif
Description : Serif mathematical alphanumeric symbols
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Data.Char.Math.Serif
  ( -- * Serif mathematical alphabet symbols
    serif,               serif'
  , serifNoBold,         serifNoBold'
  , serifBold,           serifBold'
  , serifNoItalic,       serifNoItalic'
  , serifItalic,         serifItalic'
  , serifNoBoldNoItalic, serifNoBoldNoItalic'
  , serifBoldNoItalic,   serifBoldNoItalic'
  , serifNoBoldItalic,   serifNoBoldItalic'
  , serifBoldItalic,     serifBoldItalic'
    -- * Digit characters
  , digitSerif,            digitSerif'
  , digitSerifRegular,     digitSerifRegular'
  , digitSerifBold,        digitSerifBold'
    -- ** Int to digit characters
  , intToDigitSerif,            intToDigitSerif'
  , intToDigitSerifRegular,     intToDigitSerifRegular'
  , intToDigitSerifBold,        intToDigitSerifBold'
  ) where


import Data.Char.Math.Serif.Latin
import Data.Char.Math.Serif.Digit

