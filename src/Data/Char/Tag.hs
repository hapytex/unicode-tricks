{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Tags
Description : Write characters that have been used to add tags to the text.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode has a /tags/ block. This is used to make hidden annotations to specify the language of the text, this is no longer recommended.
Since then this Unicode block has been repurposed as modifiers for region flag emoji. These are used for example in the flag of Scotland
with @"\x1f3f4\xe0067\xe0062\xe0073\xe0063\xe0074\xe007f"@ where the first character is the emoji of a black flag, the following two
characters are tags for @g@, and @b@ to present /Great Brittain/, then the following three characters are used to specify the
region where @s@, @c@ and @t@ are used to specify /Scotland/ and finally the /stateful tag terminator/ to end the Emoji sequence.
-}

module Data.Char.Tag {-# WARNING "Using tags to convey language tags is strongly discouraged by the Unicode developers." #-} (
    -- | Check if the given item is a tag; or has a tag counterpart.
    isTag, isAsciiTag, hasTagCounterPart
    -- * Convert from and to tags.
  , toTag, toTags, toTag', toTags', fromTag, fromTag', fromTags, fromTags'
    -- * Constants for two special Unicode codepoints.
  , languageTag, cancelTag
  ) where

import Data.Bits((.|.), (.&.), complement)
import Data.Char(chr, ord)

_tagOffset :: Int
_tagOffset = 0xe0000

-- | Check if the given 'Char' is a /tag/.
isTag
  :: Char  -- ^ The given 'Char'acter to check.
  -> Bool  -- 'True' if the given 'Char' is a /tag/; otherwise 'False'.
isTag '\xe0000' = True
isTag c = '\xe0020' <= c && c <= '\xe007f'

-- | Check if the given 'Char'acter has a tag counterpart. This
-- is only the case for visible ASCII characters.
hasTagCounterPart
  :: Char  -- ^ The given 'Char'acter to check.
  -> Bool  -- ^ 'True' if for the given 'Char' a /tag/ variant exists; otherwise 'False'.
hasTagCounterPart c = ' ' <= c && c <= '~'

-- | Check if the given item is a tag for a visible ASCII character.
isAsciiTag
  :: Char  -- ^ The given 'Char'acter to check.
  -> Bool  -- ^ 'True' if for the given tag, a visible ASCII character exists.
isAsciiTag c = '\xe0020' <= c && c <= '\xe007e'

-- | Convert the given 'Char' to a /tag/ wrapped in a 'Just' data constructor.
-- if there is no tag for the given 'Char', it returns 'Nothing'.
toTag
  :: Char  -- ^ The given 'Char'acter to convert to a tag character.
  -> Maybe Char  -- ^ The corresponding tag 'Char'acter wrapped in a 'Just' if such tag character exists; otherwise 'Nothing'.
toTag c
  | hasTagCounterPart c = Just (toTag' c)
  | otherwise = Nothing

-- | Try to convert the given string of characters, to a string of tag characters. If one of the
-- conversions failed, 'Nothing' is returned.
toTags
  :: String  -- ^ The given 'String' of 'Char'acters to convert to a 'String' of tag characters.
  -> Maybe String  -- ^ The string of tags wrapped in a 'Just' data constructor; 'Nothing' if at least one of the given 'Char'acters has no tag counterpart.
toTags = mapM toTag

-- | Convert the given 'Char'acter to the corresponding /tag/ character. If the given
-- character has no tag counterpart, it is unspecified what will happen.
toTag'
  :: Char  -- ^ The given 'Char'acter to convert to its corresponding /tag/ character.
  -> Char  -- ^ The corresponding tag 'Char'acter. If the given 'Char'acter has no /tag/ counterpart, it is unspecified what will happen.
toTag' = chr . (_tagOffset .|.) . ord

-- | Convert the given string of 'Char'acters to a string of corresponding tag characters. If a 'Char'acter has no
-- corresponding /tag/ 'Char'acter, the behavior is unspecified.
toTags'
  :: String  -- ^ The given 'String' of 'Char'acters to convert to tags.
  -> String  -- ^ A 'String' of tag characters that correspond to the given 'String'.
toTags' = map toTag'

-- | Convert the given tag 'Char'acter to its visible ASCII counterpart wrapped in a 'Just' data constructor. If there
-- is no such counterpart, 'Nothing' is returned.
fromTag
  :: Char  -- ^ The given tag that corresponds to a visible ASCII character.
  -> Maybe Char  -- ^ The visible ASCII character wrapped in a 'Just' if such character exists; 'Nothing' otherwise.
fromTag c
  | isAsciiTag c = Just (fromTag' c)
  | otherwise = Nothing

-- | Convert the given string of tag 'Char'acters to the visible ASCII counterparts wrapped in a 'Just' data constructor.
-- If there is a 'Char' in the given 'String' that has no such counterpart, 'Nothing' is returned.
fromTags
  :: String  -- ^ The corresponding 'String' of tag characters, which are convert to the visible ASCII counterpart.
  -> Maybe String  -- ^ A 'String' with the visible ASCII counterparts wrapped in a 'Just'; 'Nothing' if there is at least one character that has no such counterpart.
fromTags = mapM fromTag

-- | Convert the given tag 'Char'acter to its visible ASCII counterpart. If the given 'Char'acter has no such counterpart,
-- the behavior is unspecified.
fromTag'
  :: Char  -- ^ The given /tag/ with a visible ASCII counterpart.
  -> Char  -- ^ The visible ASCII counterpart of the given tag.
fromTag' = chr . (complement _tagOffset .&.) . ord

-- | Convert the given 'String' of tags to the corresponding string of visible ASCII characters. If at least one
-- of the given 'Char'acters has no visible ASCII counterpart, the behavior is unspecified.
fromTags'
  :: String  -- ^ The given 'String' of characters to convert to their visible ASCII counterpart.
  -> String  -- ^ A string with the visible ASCII counterparts. The result is unspecified if at least one of the strings has no such counterpart.
fromTags' = map fromTag'

{-# DEPRECATED languageTag "Unicode no longer encourages to use this tag character, and will likely eventually be removed." #-}
-- | A 'Char'acter that specfies the beginning of the language specification of the text. Since tags should no longer
-- be used to specify languages, this character is deprecated.
languageTag :: Char  -- ^ A 'Char'acter that once denoted the beginning of the language tags of a document.
languageTag = '\xe0001'

-- | A tag /Char/acter that specifies the end of the sequence of modifiers.
cancelTag :: Char  -- ^ A 'Char'acter that specifies that the sequence of emoji modifiers has ended.
cancelTag = '\xe007f'
