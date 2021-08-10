{-# LANGUAGE DeriveTraversable, DeriveDataTypeable, DeriveGeneric, PatternSynonyms, Safe, ScopedTypeVariables, TupleSections, TypeApplications #-}

{-|
Module      : Data.Char.Emoji.SkinColor
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
    SkinColorModifier(Light, MediumLight, Medium, MediumDark, Dark), OptionalSkinColorModifier, fromFitzpatrick, isSkinColorModifier
    -- * Create emoji with a 'SkinColorModifier'
  , WithSkinColorModifierUnicodeText(withSkinModifier, withOptionalSkinModifier, withoutOptionalSkinModifier), withSkinModifier', withOptionalSkinModifier', withoutOptionalSkinModifier'
    -- * Pattern synonyms for the 'SkinColorModifier' elements
  , pattern FitzpatrickI, pattern FitzpatrickII, pattern FitzpatrickIII, pattern FitzpatrickIV, pattern FitzpatrickV, pattern FitzpatrickVI
  ) where

import Control.DeepSeq(NFData)

import Data.Char(ord)
import Data.Char.Core(UnicodeCharacter(fromUnicodeChar, fromUnicodeChar', toUnicodeChar, isInCharRange), UnicodeText(toUnicodeText, fromUnicodeText, isInTextRange), generateIsInTextRange', mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Data(Data)
import Data.Hashable(Hashable)
import Data.Hashable.Lifted(Hashable1)
import Data.Text(Text, snoc, unsnoc)

import GHC.Generics(Generic, Generic1)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1, arbitraryBoundedEnum)

_skinColorOffset :: Int
_skinColorOffset = 0x1f3fb

_skinColorLimit :: Int
_skinColorLimit = 0x1f3ff

-- | Check if the given 'Char'acter is a skin color modifier.
isSkinColorModifier
  :: Char  -- ^ The given 'Char'acter to check.
  -> Bool  -- ^ 'True' if the given 'Char'acter is a skin color modifier, 'False' otherwise.
isSkinColorModifier c = _skinColorOffset <= oc && oc <= _skinColorLimit
  where oc = ord c

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
    isInCharRange c = '\x1f3fb' <= c && c <= '\x1f3ff'

instance UnicodeText SkinColorModifier where
  isInTextRange = generateIsInTextRange' @SkinColorModifier

-- | Append the given 'Text' object with the Unicode character to modify its skin color.
withSkinModifier'
  :: Text  -- ^ The given 'Text' object where we want to specify the skin color.
  -> SkinColorModifier  -- ^ The given'SkinColorModifier' to apply.
  -> Text  -- ^ The given 'Text' object combined with the given 'SkinColorModifier'.
withSkinModifier' t
  | Just (t', '\xfe0f') <- unsnoc t = snoc t' . toUnicodeChar
  | otherwise = snoc t . toUnicodeChar

-- | Append the given 'Text' object with the Unicode character to modify its skin color. If 'Nothing', then no modification is applied.
withOptionalSkinModifier'
  :: Text  -- ^ The given 'Text' object where we want to specify the skin color.
  -> OptionalSkinColorModifier  -- ^ The given'OptionalSkinColorModifier' to apply.
  -> Text  -- ^ The given 'Text' object combined with the given 'SkinColorModifier'.
withOptionalSkinModifier' t = maybe t (withSkinModifier' t)

-- | Convert the given 'Text' object to a wrapped 'Text' object with an 'OptionalSkinColorModifier'.
withoutOptionalSkinModifier'
  :: Text  -- ^ The given 'Text' to decompose.
  -> (Text, OptionalSkinColorModifier)  -- ^ A 2-tuple where the first item is the remaining 'Text' and where the second item is an optioanl 'SkinColorModifier'.
withoutOptionalSkinModifier' t
  | Just (t', s) <- unsnoc t, isSkinColorModifier s = (t', Just (fromUnicodeChar' s))
  | otherwise = (t, Nothing)

-- | A typeclass where one can specify that the object can be rendered with a given /skin color modifier/.
class UnicodeText a => WithSkinColorModifierUnicodeText a where
    -- | Apply the given 'SkinColorModifier' to the item and obtain a 'Text' object where the item
    -- has been modified with the 'SkinColorModifier'.
    withSkinModifier
      :: a  -- ^ The given item to render to a unicode 'Text' object.
      -> SkinColorModifier  -- ^ The given skin color modifier to apply.
      -> Text -- ^ The corresponding 'Text' where we applied the given 'SkinColorModifier'.
    withSkinModifier = withSkinModifier' . toUnicodeText

    -- | Apply the given 'SkinColorModifier' to the item given it is not 'Nothing' such that
    -- the object is rendered with the given /skin color modifier/.
    withOptionalSkinModifier
      :: a  -- ^ The given item to render to a unicode 'Text' object.
      -> OptionalSkinColorModifier  -- ^ The given optional skin color modifier.
      -> Text -- ^ The corresponding 'Text' where we applied the given 'SkinColorModifier'.
    withOptionalSkinModifier = withOptionalSkinModifier' . toUnicodeText

    -- | Convert the given Text to an item with an 'OptionalSkinColorModifier' that might
    -- have been applied.
    withoutOptionalSkinModifier
      :: Text  -- ^ The given 'Text' object that should be decoded.
      -> Maybe (a, OptionalSkinColorModifier)  -- ^ An optional 2-tuple with the item that has been read, and an optional 'SkinColorModifier'.
    withoutOptionalSkinModifier t = (, m) <$> fromUnicodeText t'
        where ~(t', m) = withoutOptionalSkinModifier' t
    {-# MINIMAL #-}

data SkinModified a = SkinModified a SkinColorModifier
  deriving (Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

instance Arbitrary1 SkinModified where
  liftArbitrary arb = SkinModified <$> arb <*> arbitrary

instance Arbitrary a => Arbitrary (SkinModified a) where
  arbitrary = arbitrary1

instance Bounded a => Bounded (SkinModified a) where
  minBound = SkinModified minBound minBound
  maxBound = SkinModified maxBound maxBound

instance Enum a => Enum (SkinModified a) where
  fromEnum (SkinModified a m) = 5 * fromEnum a + fromEnum m
  toEnum n = SkinModified (toEnum q) (toEnum r)
    where ~(q, r) = quotRem n 5

instance Hashable a => Hashable (SkinModified a)

instance Hashable1 SkinModified

instance WithSkinColorModifierUnicodeText a => UnicodeText (SkinModified a) where
  toUnicodeText (SkinModified x m) = withSkinModifier x m
  fromUnicodeText t
    | Just (x, Just m) <- withoutOptionalSkinModifier t = Just (SkinModified x m)
    | otherwise = Nothing
  isInTextRange t
    | Just (_, Just _) <- withoutOptionalSkinModifier @a t = True
    | otherwise = False

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
type OptionalSkinColorModifier = Maybe SkinColorModifier
