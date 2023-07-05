{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Data.Char.Card
-- Description : Support to work with card characters in unicode.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The <https://www.unicode.org/charts/PDF/U1F0A0.pdf 1F0A0 code block> can be used to work with cards. This module makes working with the three sets of cards more convenient. The first set
-- are cards with a suit (four suits) and a rank (14 ranks), which allows us to generate 56 cards. Next there are three jokes with the colors /red/, /black/ and /white/. Finally
-- there are 21 trump cards and the fool card (a tarot card which normally has no number).
--
-- The module contains pattern synonyms that define common aliasses for these cards.
module Data.Char.Card
  ( -- * Data structures to define a card
    CardSuit (Spades, Hearts, Diamonds, Clubs),
    CardRank (Ace, R2, R3, R4, R5, R6, R7, R8, R9, R10, Jack, Knight, Queen, King),
    JokerColor (Red, Black, White),
    Trump
      ( Fool,
        Trump1,
        Trump2,
        Trump3,
        Trump4,
        Trump5,
        Trump6,
        Trump7,
        Trump8,
        Trump9,
        Trump10,
        Trump11,
        Trump12,
        Trump13,
        Trump14,
        Trump15,
        Trump16,
        Trump17,
        Trump18,
        Trump19,
        Trump20,
        Trump21
      ),
    Card (Back, Card, Joker, Trump),

    -- * Converting cards to the corresponding Unicode character
    back,
    card,
    card',
    joker,
    trump,

    -- * Pattern synonyms for cards

    -- ** Aliasses for the card suits
    pattern Swords,
    pattern Cups,
    pattern Pentacles,
    pattern Wands,

    -- ** Aliasses for the card ranks

    -- *** Aliasses for the jack
    pattern Valet,
    pattern Bube,
    pattern Unter,
    pattern Page,
    pattern Fante,

    -- *** Aliasses for the knight
    pattern Chevalier,
    pattern Ober,
    pattern Ritter,
    pattern Cavall,
    pattern Cavaliere,

    -- *** Aliasses for the queen
    pattern Dame,
    pattern Königin,
    pattern Regina,

    -- *** Aliasses for the king
    pattern Roi,
    pattern König,
    pattern Re,

    -- ** Trump patterns

    -- *** The four ages
    pattern Childhood,
    pattern Youth,
    pattern Maturity,
    pattern OldAge,

    -- *** The four times of the day
    pattern Morning,
    pattern Afternoon,
    pattern Evening,
    pattern Night,

    -- *** The four elements
    pattern Earth,
    pattern Air,
    pattern Water,
    pattern Fire,

    -- *** The four leisures
    pattern Dance,
    pattern Shopping,
    pattern OpenAir,
    pattern VisualArts,

    -- *** The four seasons
    pattern Spring,
    pattern Summer,
    pattern Autumn,
    pattern Winter,

    -- *** The game
    pattern Game,

    -- *** Folly
    pattern Collective,
    pattern Individual,
  )
where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL, (.|.))
import Data.Char (chr)
import Data.Char.Core (UnicodeCharacter (fromUnicodeChar, fromUnicodeChar', isInCharRange, toUnicodeChar), UnicodeText (isInTextRange), generateIsInTextRange', mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Data (Data)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary), arbitraryBoundedEnum)
import Test.QuickCheck.Gen (oneof)

_suitOffset :: Int
_suitOffset = 0x2660

-- | A data type for the card suits
data CardSuit
  = -- | The /spades/ card suit.
    Spades
  | -- | The /hearts/ card suit.
    Hearts
  | -- | The /diamonds/ card suit.
    Diamonds
  | -- | The /clubs/ card suit.
    Clubs
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable CardSuit

instance NFData CardSuit

-- | A data type for the rank of the card.
data CardRank
  = -- | The /ace/ card rank.
    Ace
  | -- | Card rank 2.
    R2
  | -- | Card rank 3.
    R3
  | -- | Card rank 4.
    R4
  | -- | Card rank 5.
    R5
  | -- | Card rank 6.
    R6
  | -- | Card rank 7.
    R7
  | -- | Card rank 8.
    R8
  | -- | Card rank 9.
    R9
  | -- | Card rank 10.
    R10
  | -- | The /jack/ card rank.
    Jack
  | -- | The /knight/ card rank.
    Knight
  | -- | The /queen/ card rank.
    Queen
  | -- | The /king/ card rank.
    King
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable CardRank

instance NFData CardRank

-- | A data type to represent the three colors for which there are jokers:
-- /red/, /black/ and /white/.
data JokerColor
  = -- | The /red/ joker.
    Red
  | -- | The /black/ joker.
    Black
  | -- | The /white/ joker.
    White
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable JokerColor

instance NFData JokerColor

-- | A data type for the trump cards, often used for /tarot/.
data Trump
  = -- | The /fool/ trump card, this tarot card is normally not numbered.
    Fool
  | -- | Tarot card /I/.
    Trump1
  | -- | Tarot card /II/.
    Trump2
  | -- | Tarot card /III/.
    Trump3
  | -- | Tarot card /IV/.
    Trump4
  | -- | Tarot card /V/.
    Trump5
  | -- | Tarot card /VI/.
    Trump6
  | -- | Tarot card /VII/.
    Trump7
  | -- | Tarot card /VIII/.
    Trump8
  | -- | Tarot card /IX/.
    Trump9
  | -- | Tarot card /X/.
    Trump10
  | -- | Tarot card /XI/.
    Trump11
  | -- | Tarot card /XII/.
    Trump12
  | -- | Tarot card /XIII/.
    Trump13
  | -- | Tarot card /XIV/.
    Trump14
  | -- | Tarot card /XV/.
    Trump15
  | -- | Tarot card /XVI/.
    Trump16
  | -- | Tarot card /XVII/.
    Trump17
  | -- | Tarot card /XVIII/.
    Trump18
  | -- | Tarot card /XIX/.
    Trump19
  | -- | Tarot card /XX/.
    Trump20
  | -- | Tarot card /XXI/.
    Trump21
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable Trump

instance NFData Trump

-- | A data type that represents the possible types of cards for which there is
-- a Unicode characters. This is the back of a card, a card with a suit and
-- rank, three /jokers/, and the 21 /trump/ cards and the /fool/.
data Card
  = -- | The back of the card.
    Back
  | -- | A card that is a combination of a 'CardSuit' and a 'CardRank'. There are 56 possibilities.
    Card CardSuit CardRank
  | -- | Three possible 'JokerColor' cards.
    Joker JokerColor
  | Trump Trump -- The 21 't:Trump' cards (together with the 'Fool', which is usually not numbered).
  deriving (Data, Eq, Generic, Ord, Read, Show)

instance Arbitrary CardSuit where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CardRank where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary JokerColor where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Trump where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Card where
  arbitrary = oneof [Card <$> arbitrary <*> arbitrary, pure Back, Joker <$> arbitrary, Trump <$> arbitrary]

instance Bounded Card where
  minBound = Back
  maxBound = Trump maxBound

instance Hashable Card

instance NFData Card

instance UnicodeCharacter CardSuit where
  toUnicodeChar = mapFromEnum _suitOffset
  fromUnicodeChar = mapToEnumSafe _suitOffset
  fromUnicodeChar' = mapToEnum _suitOffset
  isInCharRange c = '\x2660' <= c && c <= '\x2663'

instance UnicodeText CardSuit where
  isInTextRange = generateIsInTextRange' @CardSuit

-- | The unicode character that represents the /back/ of the card.
back :: Char
back = '\x1f0a0'

-- | Convert the given 'CardSuit' and 'CardRank' to the equivalent unicode
-- character for this card.
card' ::
  -- | The given 'CardSuit' for the card.
  CardSuit ->
  -- | The given 'CardRank' for the card.
  CardRank ->
  -- | The corresponding unicode character with the given suit and rank.
  Char
card' s r = chr (shiftL (fromEnum s) 4 + fromEnum r + 0x1f0a1)

-- | Convert the given 'JokerColor' to the unicode character which represents
-- this joker color.
joker ::
  -- | The given 'JokerColor' to convert to a unicode character.
  JokerColor ->
  -- | The unicode character that represents the joker with the given color.
  Char
joker c = chr (shiftL (fromEnum c) 4 + 0x1f0bf)

-- | Convert the given 't:Trump' value to the unicode equivalent.
trump ::
  -- | The given 't:Trump' value to convert to a unicode character.
  Trump ->
  -- | The unicode character that represents the given 't:Trump' playing card.
  Char
trump t = chr (0x1f0e0 .|. fromEnum t)

-- | Convert the given 'Card' object to its unicode equivalent.
card :: Card -> Char
card Back = back
card (Card s r) = card' s r
card (Joker c) = joker c
card (Trump t) = trump t

-- | /Swords/ is an alias for the /spades/ card suit.
pattern Swords :: CardSuit
pattern Swords = Spades

-- | /Cups/ is an alias for the /hearts/ card suit.
pattern Cups :: CardSuit
pattern Cups = Hearts

-- | /Pentacles/ is an alias for the /diamonds/ card suit.
pattern Pentacles :: CardSuit
pattern Pentacles = Diamonds

-- | /Wands/ is an alias for the /clubs/ card suit.
pattern Wands :: CardSuit
pattern Wands = Clubs

-- | In France, the /jack/ is sometimes called the /valet/.
pattern Valet :: CardRank
pattern Valet = Jack

-- | In Germany, Austria and Switzerland, the /jack/ is sometimes called the /bube/.
pattern Bube :: CardRank
pattern Bube = Jack

-- | In Germany and Switzerland, the /jack/ is sometimes called the /unter/.
pattern Unter :: CardRank
pattern Unter = Jack

-- | An alternative name for the /jack/ is /page/.
pattern Page :: CardRank
pattern Page = Jack

-- | In Italy, the /jack/ is sometimes called the /fante/.
pattern Fante :: CardRank
pattern Fante = Jack

-- | In France, the /knight/ is sometimes called the /chevalier/.
pattern Chevalier :: CardRank
pattern Chevalier = Knight

-- | In Germany, the /knight/ is sometimes called the /ober/.
pattern Ober :: CardRank
pattern Ober = Knight

-- | In Germany, the /knight/ is sometimes called the /ritter/.
pattern Ritter :: CardRank
pattern Ritter = Knight

-- | An alternative name for the /jack/ is /cavall/.
pattern Cavall :: CardRank
pattern Cavall = Knight

-- | An alternative name for the /jack/ is /cavaliere/.
pattern Cavaliere :: CardRank
pattern Cavaliere = Knight

-- | An alternative name for the /queen/ is /dame/.
pattern Dame :: CardRank
pattern Dame = Queen

-- | In Germany, the /queen/ is sometimes called the /königin/.
pattern Königin :: CardRank
pattern Königin = Queen

-- | In Italy, the /queen/ is sometimes called the /regina/.
pattern Regina :: CardRank
pattern Regina = Queen

-- | In France, the /king/ is sometimes called the /roi/.
pattern Roi :: CardRank
pattern Roi = King

-- | In Germany, the /king/ is sometimes called the /könig/.
pattern König :: CardRank
pattern König = King

-- | In Italy, the /queen/ is sometimes called the /re/.
pattern Re :: CardRank
pattern Re = King

-- | The trump card with number /I/ is named /individual/.
pattern Individual :: Trump
pattern Individual = Trump1

-- | The trump card with number /II/ is named /childhood/.
pattern Childhood :: Trump
pattern Childhood = Trump2

-- | The trump card with number /III/ is named /youth/.
pattern Youth :: Trump
pattern Youth = Trump3

-- | The trump card with number /IV/ is named /maturity/.
pattern Maturity :: Trump
pattern Maturity = Trump4

-- | The trump card with number /V/ is named /old age/.
pattern OldAge :: Trump
pattern OldAge = Trump5

-- | The trump card with number /VI/ is named /morning/.
pattern Morning :: Trump
pattern Morning = Trump6

-- | The trump card with number /VII/ is named /afternoon/.
pattern Afternoon :: Trump
pattern Afternoon = Trump7

-- | The trump card with number /VIII/ is named /evening/.
pattern Evening :: Trump
pattern Evening = Trump8

-- | The trump card with number /IX/ is named /night/.
pattern Night :: Trump
pattern Night = Trump9

-- | The trump card with number /X/ is named /earth/.
pattern Earth :: Trump
pattern Earth = Trump10

-- | The trump card with number /X/ is named /air/.
pattern Air :: Trump
pattern Air = Trump10

-- | The trump card with number /XI/ is named /water/.
pattern Water :: Trump
pattern Water = Trump11

-- | The trump card with number /XI/ is named /fire/.
pattern Fire :: Trump
pattern Fire = Trump11

-- | The trump card with number /XII/ is named /dance/.
pattern Dance :: Trump
pattern Dance = Trump12

-- | The trump card with number /XIII/ is named /shopping/.
pattern Shopping :: Trump
pattern Shopping = Trump13

-- | The trump card with number /XIV/ is named /open air/.
pattern OpenAir :: Trump
pattern OpenAir = Trump14

-- | The trump card with number /XV/ is named /visual arts/.
pattern VisualArts :: Trump
pattern VisualArts = Trump15

-- | The trump card with number /XVI/ is named /spring/.
pattern Spring :: Trump
pattern Spring = Trump16

-- | The trump card with number /XVII/ is named /summer/.
pattern Summer :: Trump
pattern Summer = Trump17

-- | The trump card with number /XVIII/ is named /autumn/.
pattern Autumn :: Trump
pattern Autumn = Trump18

-- | The trump card with number /XIX/ is named /winter/.
pattern Winter :: Trump
pattern Winter = Trump19

-- | The trump card with number /XX/ is named the /game/.
pattern Game :: Trump
pattern Game = Trump20

-- | The trump card with number /XXI/ is named /collective/.
pattern Collective :: Trump
pattern Collective = Trump21
