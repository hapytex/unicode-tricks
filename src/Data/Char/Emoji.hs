{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Emoji
Description : A module that defines emoji and ways to render and modify emoji.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode defines 2182 emoji characters, this module aims to make working with emoji characters more convenient.
-}

module Data.Char.Emoji (
    -- * Submodule import
    module Data.Char.Emoji.Core
  , module Data.Char.Emoji.BloodType
  , module Data.Char.Emoji.Clock
  , module Data.Char.Emoji.Flag
  , module Data.Char.Emoji.Gender
  , module Data.Char.Emoji.Moon
  , module Data.Char.Emoji.SkinColor
  , module Data.Char.Emoji.Zodiac
  ) where

import Data.Char.Emoji.Core
import Data.Char.Emoji.BloodType
import Data.Char.Emoji.Clock
import Data.Char.Emoji.Flag
import Data.Char.Emoji.Gender
import Data.Char.Emoji.Moon
import Data.Char.Emoji.SkinColor
import Data.Char.Emoji.Zodiac
