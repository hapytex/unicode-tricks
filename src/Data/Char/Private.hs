{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Private
-- Description : Determine if characters belong to the /private use area/.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Unicode has a <https://www.unicode.org/charts/PDF/UE000.pdf block> named /Private Use Area/ that can be used for all sorts of characters, and each font can decide to allocate
-- certain characters for this block. In the submodules, popular usages are supoorted.
module Data.Char.Private
  ( -- * Character bounds
    privateStart,
    privateStop,
    isPrivateChar,

    -- * Generating private characters
    privateCharGen,
  )
where

import Test.QuickCheck (Gen, choose)

-- | The first character in the /private use area/.
privateStart ::
  -- | The first 'Char' in the /private use area/.
  Char
privateStart = '\xe000'

-- | The last character in the /private use area/.
privateStop ::
  -- | The last 'Char' in the /private use area/.
  Char
privateStop = '\xf8ff'

-- | Checks if the given 'Char' is in the /private use area/.
isPrivateChar ::
  -- | The given 'Char' to check.
  Char ->
  -- | 'True' if the 'Char' is in the /private use area/; 'False' otherwise.
  Bool
isPrivateChar c = privateStart <= c && c <= privateStop

-- | A generator of 'Char'acters in the /private use area/.
privateCharGen ::
  -- | A 'Gen' that generates 'Char' objects in the /private use area/.
  Gen Char
privateCharGen = choose (privateStart, privateStop)
