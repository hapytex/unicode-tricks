{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Math.Fraktur
-- Description : Fraktur mathematical alphanumeric symbols
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- See "Data.Char.Math" for further documentation.
module Data.Char.Math.Fraktur
  ( fraktur,
    fraktur',
    frakturRegular,
    frakturRegular',
    frakturBold,
    frakturBold',
  )
where

import Data.Char.Core (Emphasis, isAsciiAlpha, splitEmphasis)
import Data.Char.Math.Internal

-- | Obtain the fraktur symbol for the given character in a regular (not /bold/)
-- style. The supported range of characters are the alphabet characters
-- (@A@–@Z@, and @a@–@z@). In case the character is not in this range, it is
-- unspecified what will be returned.
frakturRegular' ::
  -- | The character to convert to a regular /fraktur/ symbol.
  Char ->
  -- | The equivalent /fraktur/ charater for the given character.
  Char
frakturRegular' 'C' = '\x212d'
frakturRegular' 'H' = '\x210c'
frakturRegular' 'I' = '\x2111'
frakturRegular' 'R' = '\x211c'
frakturRegular' 'Z' = '\x2128'
frakturRegular' c = _baseUpperLower 0x1d4bd c

-- | Obtain the fraktur symbol for the given character in a regular (not /bold/
-- style). The result is wrapped in a 'Just' data constructor. The range of
-- supported characters are the alphabet characters (@A@–@Z@, and @a@–@z@).
-- In case a character outside the range is passed to the function, 'Nothing' is
-- returned.
frakturRegular ::
  -- | The character to convert to a regular /fraktur/ symbol.
  Char ->
  -- | The fraktur symbol for the given character wrapped
  -- in a 'Just' data constructor, 'Nothing' if there is no
  -- equivalent /fraktur/ character.
  Maybe Char
frakturRegular = _withCondition isAsciiAlpha frakturRegular'

-- | Obtain the fraktur symbol for the given character in a /bold/
-- style. The supported range of characters are the alphabet characters
-- (@A@–@Z@, and @a@–@z@). In case the character is not in this range, it is
-- unspecified what will be returned.
frakturBold' ::
  -- | The character to convert to a bold /fraktur/ symbol.
  Char ->
  -- | The equivalent /fraktur/ charater for the given character.
  Char
frakturBold' = _baseUpperLower 0x1d525

-- | Obtain the fraktur symbol for the given character in a /bold/.
-- The result is wrapped in a 'Just' data constructor. The range of
-- supported characters are the alphabet characters (@A@–@Z@, and @a@–@z@).
-- In case a character outside the range is passed to the function, 'Nothing' is
-- returned.
frakturBold ::
  -- | The character to convert to a bold /fraktur/ symbol.
  Char ->
  -- | The fraktur symbol for the given character wrapped
  -- in a 'Just' data constructor, 'Nothing' if there is no
  -- equivalent /fraktur/ character.
  Maybe Char
frakturBold = _withCondition isAsciiAlpha frakturBold'

-- | Obtain the fraktur symbol for the given character in the given emphasis
-- style. The supported range of characters are the alphabet characters
-- (@A@–@Z@, and @a@–@z@). In case the character is not in this range, it is
-- unspecified what will be returned.
fraktur' ::
  -- | The given emphasis style to use.
  Emphasis ->
  -- | The character to convert to a /fraktur/ symbol in the given emphasis style.
  Char ->
  -- | The equivalent /fraktur/ charater for the given character.
  Char
fraktur' = splitEmphasis frakturRegular' frakturBold'

-- | Obtain the fraktur symbol for the given character in the given emphais
-- style. The result is wrapped in a 'Just' data constructor. The range of
-- supported characters are the alphabet characters (@A@–@Z@, and @a@–@z@).
-- In case a character outside the range is passed to the function, 'Nothing' is
-- returned.
fraktur ::
  -- | The given emphasis style to use.
  Emphasis ->
  -- | The character to convert to a /fraktur/ symbol with the given emphasis style.
  Char ->
  -- | The fraktur symbol for the given character wrapped
  -- in a 'Just' data constructor, 'Nothing' if there is no
  -- equivalent /fraktur/ character.
  Maybe Char
fraktur = splitEmphasis frakturRegular frakturBold
