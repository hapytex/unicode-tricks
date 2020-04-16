{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Math
Description : A module to write math unicode alphanumerical characters.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module aims to make it more convenient to transform mathematical characters
to their /double struck/, /fraktur/, /calligraphic/, etc. equivalent.
-}

module Data.Char.Math (
    -- * Double struck symbols
    doubleStruck, doubleStruck'
    -- * Fraktur symbols
  , frakturRegular, frakturRegular'
  , frakturBold,    frakturBold'
  , fraktur,        fraktur'
  ) where

import Data.Char(chr, isAsciiLower, isAsciiUpper, ord)
import Data.Char.Core(Emphasis(NoBold, Bold), isAsciiAlpha, isAsciiAlphaNum)

_ordc :: Char -> Int -> Char
_ordc = (chr .) . (+) . ord

_baseUpperLower :: Int -> Char -> Char
_baseUpperLower b c
    | isAsciiUpper c = oc (b+6)
    | otherwise = oc b
    where oc = _ordc c

_withCondition :: (Char -> Bool) -> (Char -> Char) -> Char -> Maybe Char
_withCondition p f = go
    where go x | p x = Just (f x)
               | otherwise = Nothing

-- | Obtain the double struck symbol for the given character. The supported
-- range of characters are the alphabet character (@A@-@Z@, and @a@-@z@), and
-- the numerical characters (@0@-@9@). For characters other than these, the
-- behaviour is unspecified.
doubleStruck'
  :: Char  -- ^ The character to convert to a /double struck/ symbol.
  -> Char -- ^ The double struck symbol for the given character. If the character
          -- is not an ASCII alphanumerical character, the result is
          -- unspecified.
doubleStruck' 'C' = '\x2102'
doubleStruck' 'H' = '\x210d'
doubleStruck' 'N' = '\x2115'
doubleStruck' 'P' = '\x2119'
doubleStruck' 'Q' = '\x211a'
doubleStruck' 'R' = '\x211d'
doubleStruck' 'Z' = '\x2124'
doubleStruck' c
    | isAsciiUpper c = oc 0x1d4f7
    | isAsciiLower c = oc 0x1d4f1
    | otherwise = oc 0x1d7a8
    where oc = _ordc c

-- | Obtain the double struck symbol for the given character. The supported
-- range of characters are the alphabet characters (@A@-@Z@, and @a@-@z@), and
-- the numerical characters (@0@-@9@). The symbols are wrapped in the 'Just'
-- data constructor. For characters outside the range, 'Nothing' is returned.
doubleStruck
  :: Char -- ^ The character to convert to a /double struck/ symbol.
  -> Maybe Char -- ^ The double struck symbol for the given character wrapped
                -- in a 'Just' data constructor, 'Nothing' if there is no
                -- equivalent /double stuck/ character.
doubleStruck = _withCondition isAsciiAlphaNum doubleStruck'

-- | Obtain the fraktur symbol for the given character in a regular (not /bold/)
-- style. The supported range of characters are the alphabet characters
-- (@A@-@Z@, and @a@-@z@). In case the character is not in this range, it is
-- unspecified what will be returned.
frakturRegular'
  :: Char -- ^ The character to convert to a regular /fraktur/ symbol.
  -> Char -- ^ The equivalent /fraktur/ charater for the given character.
frakturRegular' 'C' = '\x212d'
frakturRegular' 'H' = '\x210c'
frakturRegular' 'I' = '\x2111'
frakturRegular' 'R' = '\x211c'
frakturRegular' 'Z' = '\x2128'
frakturRegular' c = _baseUpperLower 0x1d4bd c

-- | Obtain the fraktur symbol for the given character in a regular (not /bold/
-- style). The result is wrapped in a 'Just' data constructor. The range of
-- supported characters are the alphabet characters (@A@-@Z@, and @a@-@z@).
-- In case a character outside the range is passed to the function, 'Nothing' is
-- returned.
frakturRegular
  :: Char -- ^ The character to convert to a regular /fraktur/ symbol.
  -> Maybe Char -- ^ The fraktur symbol for the given character wrapped
                -- in a 'Just' data constructor, 'Nothing' if there is no
                -- equivalent /fraktur/ character.
frakturRegular = _withCondition isAsciiAlpha frakturRegular'

-- | Obtain the fraktur symbol for the given character in a /bold/
-- style. The supported range of characters are the alphabet characters
-- (@A@-@Z@, and @a@-@z@). In case the character is not in this range, it is
-- unspecified what will be returned.
frakturBold'
  :: Char -- ^ The character to convert to a bold /fraktur/ symbol.
  -> Char -- ^ The equivalent /fraktur/ charater for the given character.
frakturBold' = _baseUpperLower 0x1d525

-- | Obtain the fraktur symbol for the given character in a /bold/.
-- The result is wrapped in a 'Just' data constructor. The range of
-- supported characters are the alphabet characters (@A@-@Z@, and @a@-@z@).
-- In case a character outside the range is passed to the function, 'Nothing' is
-- returned.
frakturBold
  :: Char -- ^ The character to convert to a bold /fraktur/ symbol.
  -> Maybe Char -- ^ The fraktur symbol for the given character wrapped
                -- in a 'Just' data constructor, 'Nothing' if there is no
                -- equivalent /fraktur/ character.
frakturBold = _withCondition isAsciiAlpha frakturBold'

-- | Obtain the fraktur symbol for the given character in the given emphasis
-- style. The supported range of characters are the alphabet characters
-- (@A@-@Z@, and @a@-@z@). In case the character is not in this range, it is
-- unspecified what will be returned.
fraktur'
  :: Emphasis -- ^ The given emphasis style to use.
  -> Char -- ^ The character to convert to a /fraktur/ symbol in the given emphasis style.
  -> Char -- ^ The equivalent /fraktur/ charater for the given character.
fraktur' NoBold = frakturRegular'
fraktur' Bold = frakturBold'

-- | Obtain the fraktur symbol for the given character in the given emphais
-- style. The result is wrapped in a 'Just' data constructor. The range of
-- supported characters are the alphabet characters (@A@-@Z@, and @a@-@z@).
-- In case a character outside the range is passed to the function, 'Nothing' is
-- returned.
fraktur
  :: Emphasis -- ^ The given emphasis style to use.
  -> Char -- ^ The character to convert to a /fraktur/ symbol with the given emphasis style.
  -> Maybe Char -- ^ The fraktur symbol for the given character wrapped
                -- in a 'Just' data constructor, 'Nothing' if there is no
                -- equivalent /fraktur/ character.
fraktur NoBold = frakturRegular
fraktur Bold = frakturBold
