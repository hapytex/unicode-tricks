{-# LANGUAGE CPP, PatternSynonyms, Safe #-}

{-|
Module      : Data.Char.Emoji.Core
Description : A module that defines basic properties applicable to all /Emoji/.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines Emoji patterns.
-}

module Data.Char.Emoji.Core (
    -- * Append the emoji suffix
    withEmojiSuffix
    -- * Pattern synonyms
  , pattern EmojiSuffix
  ) where

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup(Semigroup((<>)))
#endif
import Data.String(IsString, fromString)

-- | A 'Char'acter that is often used as a suffix to turn a character into an
-- emoji.
pattern EmojiSuffix :: Char
pattern EmojiSuffix = '\xfe0f'

-- | Append the 'EmojiSuffix' to the string-like value.
withEmojiSuffix :: (Semigroup s, IsString s)
  => s  -- ^ The string-like object to append the 'EmojiSuffix' to.
  -> s  -- ^ The string-like object with an 'EmojiSuffix' as suffix.
withEmojiSuffix = (<> fromString [EmojiSuffix])
