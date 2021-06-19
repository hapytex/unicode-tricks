{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric, DeriveTraversable, FlexibleInstances, PatternSynonyms, Safe #-}

{-|
Module      : Data.Char.Domino
Description : A module that defines domino values, and their unicode equivalent.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines values for domino pieces, and converts these to unicode characters of the <https://www.unicode.org/charts/PDF/U1F030.pdf 1F030 unicode block>.
-}

module Data.Char.Domino (
    -- * Data types to represent domino values
    Domino(Domino, Back, leftTop, rightBottom), pattern (:|)
  , OrientedDomino, SimpleDomino, ComplexDomino
    -- * Render domino values
  , dominoH, dominoH'
  , dominoV, dominoV'
  , domino , domino'
    -- * Convert from 'Char'acters
  , fromDomino, fromDomino'
  ) where

import Control.DeepSeq(NFData, NFData1)
import Control.Monad((>=>))

import Data.Char(chr, ord)
import Data.Char.Core(MirrorHorizontal(mirrorHorizontal), MirrorVertical(mirrorVertical), UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar'), UnicodeText, Orientation(Horizontal, Vertical), Oriented(Oriented))
import Data.Char.Dice(DieValue)
import Data.Data(Data)
import Data.Function(on)
import Data.Functor.Classes(Eq1(liftEq), Ord1(liftCompare))
import Data.Hashable(Hashable)
import Data.Hashable.Lifted(Hashable1)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup((<>))
#endif

import GHC.Generics(Generic, Generic1)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)
import Test.QuickCheck.Gen(frequency)

-- | A domino piece, which has two items. Depending on the orientation, the
-- items are located at the /top/ and /bottom/; or /left/ and /right/.
data Domino a
  = Domino  -- ^ The front side of the domino piece.
  {
    leftTop :: a  -- ^ The part that is located at the /left/ side in case the piece is located /horizontally/, or at the /top/ in case the piece is located /vertically/.
  , rightBottom :: a  -- ^ The part that is located at the /right/ side in case the piece is located /horizontally/, or at the /bottom/ in case the piece is located /vertically/.
  }
  | Back  -- ^ The back side of the domino piece.
  deriving (Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

instance Eq1 Domino where
  liftEq cmp (Domino lta rba) (Domino ltb rbb) = cmp lta ltb && cmp rba rbb
  liftEq _ Back Back = True
  liftEq _ _ _ = False

instance Hashable1 Domino

instance Hashable a => Hashable (Domino a)

instance NFData a => NFData (Domino a)

instance NFData1 Domino

instance Ord1 Domino where
  liftCompare cmp (Domino lta rba) (Domino ltb rbb) = cmp lta ltb <> cmp rba rbb
  liftCompare _ (Domino _ _) Back = LT
  liftCompare _ Back Back = EQ
  liftCompare _ Back (Domino _ _) = GT

-- | A pattern synonym that makes it more convenient to write expressions that
-- look like domino's like for example @II :| IV@.
pattern (:|)
  :: a  -- ^ The item that is located at the left, or the top.
  -> a  -- ^ The item that is located at the right, or the bottom.
  -> Domino a  -- ^ The domino that is constructed.
pattern (:|) x y = Domino x y

-- | A type alias that specifies that 'OrientedDomino' is an 'Oriented' type
-- that wraps a 'Domino' item.
type OrientedDomino a = Oriented (Domino a)

-- | A 'SimpleDomino' is a 'Domino' that contains 'DieValue' objects, it thus
-- can not have an "empty" value.
type SimpleDomino = Domino DieValue

-- | A 'ComplexDomino' is a 'Domino' that contains 'Maybe' values wrapping a
-- 'DieValue'. In case of a 'Nothing', that side is considered /empty/.
type ComplexDomino = Domino (Maybe DieValue)

instance Applicative Domino where
    pure x = Domino x x
    Domino fa fb <*> Domino a b = Domino (fa a) (fb b)
    _ <*> _ = Back

instance Arbitrary a => Arbitrary (Domino a) where
    arbitrary = arbitrary1

instance Arbitrary1 Domino where
    liftArbitrary arb = frequency [(1, pure Back), (3, Domino <$> arb <*> arb)]

instance Bounded a => Bounded (Domino a) where
    minBound = Domino minBound minBound
    maxBound = Back

_offsetDominoHorizontal :: Int
_offsetDominoHorizontal = 0x1f030

_offsetDominoVertical :: Int
_offsetDominoVertical = 0x1f062

_domino :: Int -> ComplexDomino -> Char
_domino n = go
    where go Back = chr n
          go (Domino a b) = chr (7 * _val a + _val b + n + 1)
          _val Nothing = 0
          _val (Just x) = 1 + fromEnum x

_fromDomino :: Int -> ComplexDomino
_fromDomino (-1) = Back
_fromDomino n = on Domino go a b
    where (a, b) = quotRem n 7
          go 0 = Nothing
          go k = Just (toEnum (k-1))

-- | Convert the given 'Char'acter to an 'Oriented' 'ComplexDomino' object. If
-- the given 'Char'acter is not a valid domino character, the result is
-- unspecified.
fromDomino'
  :: Char  -- ^ The given 'Char'acter to convert to an 'Oriented' 'ComplexDomino' object.
  -> Oriented ComplexDomino  -- ^ The equivalent 'Oriented' 'ComplexDomino' object for the given 'Char'acter.
fromDomino' = go . ord
    where go n | n >= _offsetDominoVertical = go' _offsetDominoVertical n Vertical
               | otherwise = go' _offsetDominoHorizontal n Horizontal
          go' k = Oriented . _fromDomino . pred . subtract k

-- | Convert the given 'Char'acter to an 'Oriented' 'ComplexDomino' object. If
-- the given 'Char'acter wrapped in a 'Just' data constructor if the 'Char'acter
-- is a valid domino character; otherwise 'Nothing'.
fromDomino
  :: Char  -- ^ The given 'Char'acter to convert to an 'Oriented' 'ComplexDomino' object.
  -> Maybe (Oriented ComplexDomino)  -- ^ The equivalent 'Oriented' 'ComplexDomino' object for the given 'Char'acter wrapped in a 'Just'; 'Nothing' if the character is not a domino character.
fromDomino c
    | c < '\x1f030' || c > '\x1f093' = Nothing
    | otherwise = Just (fromDomino' c)

toSimple :: Domino (Maybe a) -> Maybe (Domino a)
toSimple Back = Just Back
toSimple (Domino (Just a) (Just b)) = Just (Domino a b)
toSimple _ = Nothing

-- | Convert a 'ComplexDomino' value to a unicode character rendering the domino
-- value /horizontally/.
dominoH
  :: ComplexDomino  -- ^ The 'ComplexDomino' object to render horizontally.
  -> Char  -- ^ The unicode character that represents the given 'ComplexDomino' value in a horizontal manner.
dominoH = _domino _offsetDominoHorizontal

-- | Convert a 'SimpleDomino' value to a unicode character rendering the domino
-- value /horizontally/.
dominoH'
  :: SimpleDomino  -- ^ The 'SimpleDomino' object to render horizontally.
  -> Char  -- ^ The unicode character that represents the given 'SimpleDomino' value in a horizontal manner.
dominoH' = dominoH . fmap Just

-- | Convert a 'ComplexDomino' value to a unicode character rendering the domino
-- value /vertically/.
dominoV
  :: ComplexDomino  -- ^ The 'ComplexDomino' object to render vertically.
  -> Char  -- ^ The unicode character that represents the given 'ComplexDomino' value in a vertical manner.
dominoV = _domino _offsetDominoVertical

-- | Convert a 'SimpleDomino' value to a unicode character rendering the domino
-- value /vertically/.
dominoV'
  :: SimpleDomino  -- ^ The 'SimpleDomino' object to render vertically.
  -> Char  -- ^ The unicode character that represents the given 'SimpleDomino' value in vertical manner.
dominoV' = dominoV . fmap Just

-- | Convert an 'OrientedDomino' to its unicode equivalent, where the sides of
-- the domino can be empty.
domino
  :: OrientedDomino (Maybe DieValue)  -- ^ The 'OrientedDomino' to render.
  -> Char  -- ^ The unicode characters that represents the 'OrientedDomino' value.
domino (Oriented d Horizontal) = dominoH d
domino (Oriented d Vertical) = dominoV d

-- | Convert an 'OrientedDomino' to its unicode equivalent, where the sides of
-- the domino can /not/ be empty.
domino'
  :: OrientedDomino DieValue  -- ^ The 'OrientedDomino' to render.
  -> Char  -- ^ The unicode characters that represents the 'OrientedDomino' value.
domino' = domino . fmap (fmap Just)

instance UnicodeCharacter (Oriented (Domino (Maybe DieValue))) where
    toUnicodeChar = domino
    fromUnicodeChar = fromDomino
    fromUnicodeChar' = fromDomino'

instance MirrorHorizontal (Oriented (Domino a)) where
  mirrorHorizontal (Oriented (Domino a b) Vertical) = Oriented (Domino b a) Vertical
  mirrorHorizontal o@(Oriented Back _) = o
  mirrorHorizontal o@(Oriented _ Horizontal) = o

instance MirrorVertical (Oriented (Domino a)) where
  mirrorVertical (Oriented (Domino a b) Horizontal) = Oriented (Domino b a) Horizontal
  mirrorVertical o@(Oriented Back _) = o
  mirrorVertical o@(Oriented _ Vertical) = o

instance UnicodeCharacter (Oriented (Domino DieValue)) where
    toUnicodeChar = domino'
    fromUnicodeChar = fromDomino >=> traverse toSimple

instance UnicodeText (Oriented (Domino (Maybe DieValue)))
instance UnicodeText (Oriented (Domino DieValue))
