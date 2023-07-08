{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Math.Script
-- Description : Script/calligraphy mathematical alphanumeric symbols
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- See "Data.Char.Math" for further documentation.
module Data.Char.Math.Script
  ( script,
    script',
    scriptRegular,
    scriptRegular',
    scriptBold,
    scriptBold',
    calligraphy,
    calligraphy',
    calligraphyRegular,
    calligraphyRegular',
    calligraphyBold,
    calligraphyBold',
  )
where

import Data.Char.Core (Emphasis, isAsciiAlpha, splitEmphasis)
import Data.Char.Math.Internal

-- | Convert the given character to its /script/ or /calligraphic/ symbol. This
-- symbol is /not/ written in boldface.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
scriptRegular' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted in calligraphy, not in bold.
  Char
scriptRegular' 'B' = '\x212c'
scriptRegular' 'E' = '\x2130'
scriptRegular' 'F' = '\x2131'
scriptRegular' 'H' = '\x210b'
scriptRegular' 'I' = '\x2110'
scriptRegular' 'L' = '\x2112'
scriptRegular' 'M' = '\x2133'
scriptRegular' 'R' = '\x211b'
scriptRegular' 'e' = '\x212f'
scriptRegular' 'g' = '\x210a'
scriptRegular' 'o' = '\x2134'
scriptRegular' c = _baseUpperLower 0x1d455 c

-- | Convert the given character to its /script/ or /calligraphic/ symbol
-- wrapped in a 'Just' data constructor. This symbol is /not/ written in
-- boldface.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
scriptRegular ::
  -- | The given character to convert.
  Char ->
  -- | The calligraphy symbol for the given character wrapped
  -- in a 'Just' data constructor, 'Nothing' if there is no
  -- equivalent /calligraphy/ character.
  Maybe Char
scriptRegular = _withCondition isAsciiAlpha scriptRegular'

-- | Convert the given character to its /script/ or /calligraphic/ symbol. This
-- symbol is written in boldface.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
scriptBold' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted in calligraphy, and in bold.
  Char
scriptBold' = _baseUpperLower 0x1d489

-- | Convert the given character to its /script/ or /calligraphic/ symbol
-- wrapped in a 'Just' data constructor. This symbol is written in
-- boldface.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
scriptBold ::
  -- | The given character to convert.
  Char ->
  -- | The calligraphy symbol for the given character wrapped
  -- in a 'Just' data constructor, 'Nothing' if there is no
  -- equivalent /calligraphy/ character.
  Maybe Char
scriptBold = _withCondition isAsciiAlpha scriptBold'

-- | Convert the given character to its /script/ or /calligraphic/ symbol. This
-- symbol is written in the given 'Emphasis' style.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
script' ::
  -- | The given 'Emphasis' style to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted in calligraphy, and depending on the 'Emphasis' in bold or not.
  Char
script' = splitEmphasis scriptRegular' scriptBold'

-- | Convert the given character to its /script/ or /calligraphic/ symbol
-- wrapped in a 'Just' data constructor. This symbol is /not/ written in
-- the given 'Emphasis' style.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
script ::
  -- | The given 'Emphasis' style to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The calligraphy symbol for the given character wrapped
  -- in a 'Just' data constructor, 'Nothing' if there is no
  -- equivalent /calligraphy/ character.
  Maybe Char
script = splitEmphasis scriptRegular scriptBold

-- | Convert the given character to its /script/ or /calligraphic/ symbol. This
-- symbol is /not/ written in boldface.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
-- This is an alias of 'scriptRegular''.
calligraphyRegular' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted in calligraphy, not in bold.
  Char
calligraphyRegular' = scriptRegular'

-- | Convert the given character to its /script/ or /calligraphic/ symbol
-- wrapped in a 'Just' data constructor. This symbol is /not/ written in
-- boldface.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
-- This is an alias of 'scriptRegular'.
calligraphyRegular ::
  -- | The given character to convert.
  Char ->
  -- | The calligraphy symbol for the given character wrapped
  -- in a 'Just' data constructor, 'Nothing' if there is no
  -- equivalent /calligraphy/ character.
  Maybe Char
calligraphyRegular = scriptRegular

-- | Convert the given character to its /script/ or /calligraphic/ symbol. This
-- symbol is written in boldface.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
-- This is an alias of 'scriptBold''.
calligraphyBold' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted in calligraphy, and in bold.
  Char
calligraphyBold' = scriptBold'

-- | Convert the given character to its /script/ or /calligraphic/ symbol
-- wrapped in a 'Just' data constructor. This symbol is written in boldface.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
-- This is an alias of 'scriptBold'.
calligraphyBold ::
  -- | The given character to convert.
  Char ->
  -- | The calligraphy symbol for the given character wrapped
  -- in a 'Just' data constructor, 'Nothing' if there is no
  -- equivalent /calligraphy/ character.
  Maybe Char
calligraphyBold = scriptBold

-- | Convert the given character to its /script/ or /calligraphic/ symbol. This
-- symbol is written in the given 'Emphasis' style.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
-- This is an alias of 'script''.
calligraphy' ::
  -- | The given 'Emphasis' style to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted in calligraphy, and depending on the 'Emphasis' in bold or not.
  Char
calligraphy' = script'

-- | Convert the given character to its /script/ or /calligraphic/ symbol
-- wrapped in a 'Just' data constructor. This symbol is /not/ written in
-- the given 'Emphasis' style.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
-- This is an alias of 'script'.
calligraphy ::
  -- | The given 'Emphasis' style to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The calligraphy symbol for the given character wrapped
  -- in a 'Just' data constructor, 'Nothing' if there is no
  -- equivalent /calligraphy/ character.
  Maybe Char
calligraphy = script
