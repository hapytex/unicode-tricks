{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Control
Description : Visualizing control characters.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode has a <https://www.unicode.org/charts/PDF/U2400.pdf block> named /Control Pictures/ that visualizes control characters such as NULL, SUB, LF, DEL, etc.
This module aims to make it more convenient to convert the control characters to their visualization and vice versa. Only ASCII control characters and
the space are supported.
-}

module Data.Char.Control (
    -- * Conversion to control pictures
    controlPicture, controlPicture'
    -- * Conversion from control picturesa
  , fromControlPicture, fromControlPicture'
    -- * Check if a 'Char' is a control 'Char'
  , isAsciiControl, isControl, hasControlVisualization
    -- * Alternative characters
  , blankSymbol, openBox, newLine, alternativeDelete, alternativeSubstitute
  ) where

import Data.Bits((.&.), (.|.))
import Data.Char(chr, ord, isControl)

-- | Check if the given 'Char' is a control character in the ASCII range.
isAsciiControl
  :: Char  -- ^ The given 'Char' to check.
  -> Bool  -- ^ 'True' if the given 'Char' is a control character in the ASCII range; otherwise 'False'.
isAsciiControl c = c <= '\x7f' && isControl c

-- | Check if for the given 'Char' there is a visualization.
hasControlVisualization
  :: Char  -- ^ The given 'Char' to check.
  -> Bool  -- ^ 'True' if the given control character can be visualized; 'False' otherwise.
hasControlVisualization ' ' = True
hasControlVisualization c = isAsciiControl c

-- | Another symbol used to denote a /space/ that works with @␢@. The 'controlPicture' function uses @␠@.
blankSymbol
  :: Char -- ^ Another character for /space/.
blankSymbol = '\x2422'

-- | Another symbol used to denote a /space/ that works with @␣@. The 'controlPicture' function uses @␠@.
openBox
  :: Char -- ^ Another character for /space/.
openBox = '\x2423'

-- | Another symbol used to denote a /new line/ that works with @␤@. The control picture function uses @␊@.
newLine
  :: Char -- ^ Another character for a /new line/.
newLine = '\x2424'

-- | Another symbol used to denote a /delete/ character that works with @␥@. The control picture function uses @␡@.
alternativeDelete
  :: Char -- ^ Another character for /delete/.
alternativeDelete = '\x2425'

-- | Another symbol used to denote a /substitute/ character that works with @␦@. The control picture function uses @␚@.
alternativeSubstitute
  :: Char -- ^ Another character for /substitute/.
alternativeSubstitute = '\x2426'

-- | Convert the given control 'Char' to a 'Char' that visualizes that characters.
-- This is sometimes done by diagonal lettering of the characters denoting the control
-- character. If the given 'Char' is not a control character, 'Nothing' is returned.
controlPicture
  :: Char  -- ^ The given control 'Char' to convert.
  -> Maybe Char  -- The corresponding 'Char' that visualizes the control 'Char' wrapped in a 'Just'; 'Nothing' if the given 'Char' is not a control character.
controlPicture c
  | c <= ' ' = Just (controlPicture' c)
  | otherwise = Nothing

-- | Convert the given control 'Char' to a 'Char' that visualizes that character.
-- If the given 'Char' is not a control character, it is unspecified what happens.
controlPicture'
  :: Char  -- ^ The given control 'Char'.
  -> Char  -- ^ The corresponding 'Char' that visualizes the control 'Char'.
controlPicture' c
  | c <= ' ' = chr (0x2400 .|. ord c)
  | otherwise = '\x2421'

-- | Convert the given visualization of a control 'Char' to that control 'Char' wrapped
-- in a 'Just'. If the given 'Char' is not a visualization of a control character,
-- 'Nothing' is returned.
fromControlPicture
  :: Char  -- ^ The given /visualization/ of control 'Char'.
  -> Maybe Char  -- ^ The corresponding control 'Char' wrapped in a 'Just' if the given character is the visualization of a control character; otherwise 'Nothing'.
fromControlPicture c
  | '\x2400' <= c && c <= '\x2426' = Just (fromControlPicture' c)
  | otherwise = Nothing

-- | Convert the given visualization of a control 'Char' to that control 'Char'.
-- If the given 'Char' is not a visualization of a control character,
-- it is unspecified what happens.
fromControlPicture'
  :: Char  -- ^ The given /visualization/ of control 'Char'.
  -> Char  -- ^ The corresponding control 'Char'.
fromControlPicture' '\x2421' = '\x7f'
fromControlPicture' '\x2422' = ' '
fromControlPicture' '\x2423' = ' '
fromControlPicture' '\x2424' = '\x0a'
fromControlPicture' '\x2425' = '\x7f'
fromControlPicture' '\x2426' = '\x1a'
fromControlPicture' c = chr (0x7f .&. ord c)
