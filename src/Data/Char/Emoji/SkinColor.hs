{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, PatternSynonyms, Safe #-}

{-|
Module      : Data.Char.Emoji.Clock
Description : A module that defines emoji that display a clock.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

For several emoji, one can specify the color of the skin of the person(s)
of the emoji. This module defines the skin color modifiers together with
its values on the Fitzpatrick scale.
-}

module Data.Char.Emoji.SkinColor (
    -- * Skin color modifier
    SkinColorModifier(Light, MediumLight, Medium, MediumDark, Dark), OptionalSkinColor, fromFitzpatrick
    -- * Create emoji with a 'SkinColorModifier'
  , withSkinModifier, withOptionalSkinModifier
    -- * Pattern synonyms for the 'SkinColorModifier' elements
  , pattern FitzpatrickI, pattern FitzpatrickII, pattern FitzpatrickIII, pattern FitzpatrickIV, pattern FitzpatrickV, pattern FitzpatrickVI
  ) where

import Control.DeepSeq(NFData)

import Data.Char.Core(UnicodeCharacter(fromUnicodeChar, fromUnicodeChar', toUnicodeChar), UnicodeText, mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Data(Data)
import Data.Hashable(Hashable)
import Data.Text(Text, snoc)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

_skinColorOffset :: Int
_skinColorOffset = 0x1f3fb

-- | Some emoji deal with people. One can change the color of the skin with the
-- 'SkinColorModifier'. For the skin color, the <https://en.wikipedia.org/wiki/Fitzpatrick_scale /Fitzpatrick scale/> is used.
-- A numerical classification system for skin types.
data SkinColorModifier
  = Light  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ one or two to the Emoji.
  | MediumLight  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ three to the Emoji.
  | Medium  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ four to the Emoji.
  | MediumDark  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ five to the Emoji.
  | Dark  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ six to the Emoji.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary SkinColorModifier where
    arbitrary = arbitraryBoundedEnum

instance Hashable SkinColorModifier

instance NFData SkinColorModifier

instance UnicodeCharacter SkinColorModifier where
    toUnicodeChar = mapFromEnum _skinColorOffset
    fromUnicodeChar = mapToEnumSafe _skinColorOffset
    fromUnicodeChar' = mapToEnum _skinColorOffset

-- | Append the given 'Text' object with the Unicode character to modify its skin color.
withSkinModifier
  :: Text  -- ^ The given 'Text' object where we want to specify the skin color.
  -> SkinColorModifier  -- ^ The given'SkinColorModifier' to apply.
  -> Text  -- ^ The given 'Text' object combined with the given 'SkinColorModifier'.
withSkinModifier t = snoc t . toUnicodeChar

-- | Append the given 'Text' object with the Unicode character to modify its skin color. If 'Nothing', then no modification is applied.
withOptionalSkinModifier
  :: Text  -- ^ The given 'Text' object where we want to specify the skin color.
  -> OptionalSkinColor  -- ^ The given'OptionalSkinColor' to apply.
  -> Text  -- ^ The given 'Text' object combined with the given 'SkinColorModifier'.
withOptionalSkinModifier t = maybe t (withSkinModifier t)

instance UnicodeText SkinColorModifier

-- | The 'SkinColorModifier' that corresponds to type one of the /Fitzpatrick
-- scale/.
pattern FitzpatrickI :: SkinColorModifier
pattern FitzpatrickI = Light

-- | The 'SkinColorModifier' that corresponds to type two of the /Fitzpatrick
-- scale/.
pattern FitzpatrickII :: SkinColorModifier
pattern FitzpatrickII = Light

-- | The 'SkinColorModifier' that corresponds to type three of the /Fitzpatrick
-- scale/.
pattern FitzpatrickIII :: SkinColorModifier
pattern FitzpatrickIII = MediumLight

-- | The 'SkinColorModifier' that corresponds to type four of the /Fitzpatrick
-- scale/.
pattern FitzpatrickIV :: SkinColorModifier
pattern FitzpatrickIV = Medium

-- | The 'SkinColorModifier' that corresponds to type five of the /Fitzpatrick
-- scale/.
pattern FitzpatrickV :: SkinColorModifier
pattern FitzpatrickV = MediumDark

-- | The 'SkinColorModifier' that corresponds to type six of the /Fitzpatrick
-- scale/.
pattern FitzpatrickVI :: SkinColorModifier
pattern FitzpatrickVI = Dark

-- | Convert the given /Fitzpatrick skin type/ to the corresponding
-- 'SkinColorModifier' wrapped in a 'Just', if no such 'SkinColorModifier'
-- exists, 'Nothing' is returned.
fromFitzpatrick :: Integral i
  => i  -- ^ The given /Fitzpatrick skin type/.
  -> Maybe SkinColorModifier  -- ^ The corresponding 'SkinColorModifier' wrapped in a 'Just'; 'Nothing' if no such modifier exists.
fromFitzpatrick 1 = Just Light
fromFitzpatrick 2 = Just Light
fromFitzpatrick 3 = Just MediumLight
fromFitzpatrick 4 = Just Medium
fromFitzpatrick 5 = Just MediumDark
fromFitzpatrick 6 = Just Dark
fromFitzpatrick _ = Nothing

-- | For emoji often the skin color is optional: in case there is no skin color specified, the emoji have often a /yellow/ skin color.
type OptionalSkinColor = Maybe SkinColorModifier
