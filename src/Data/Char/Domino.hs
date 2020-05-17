{-# LANGUAGE DeriveTraversable, PatternSynonyms, Safe #-}

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
  ) where

import Data.Char(chr)
import Data.Char.Core(Orientation(Horizontal, Vertical), Oriented(Oriented))
import Data.Char.Dice(DieValue)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)
import Test.QuickCheck.Gen(frequency)

-- | A domino piece, which has two items. Depending on the orientation, the
-- items are located at the /top/ and /bottom/; or /left/ and /right/.
data Domino a
  = Domino -- ^ The front side of the domino piece.
  {
    leftTop :: a -- ^ The part that is located at the /left/ side in case the piece is located /horizontally/, or at the /top/ in case the piece is located /vertically/.
  , rightBottom :: a -- ^ The part that is located at the /right/ side in case the piece is located /horizontally/, or at the /bottom/ in case the piece is located /vertically/.
  }
  | Back  -- ^ The back side of the domino piece.
  deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)

-- | A pattern synonym that makes it more convenient to write expressions that
-- look like domino's like for example @II :| IV@.
pattern (:|)
  :: a -- ^ The item that is located at the left, or the top.
  -> a -- ^ The item that is located at the right, or the bottom.
  -> Domino a -- ^ The domino that is constructed.
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

_domino :: Int -> ComplexDomino -> Char
_domino n = go
    where go Back = chr n
          go (Domino a b) = chr (7 * _val a + _val b + n + 1)
          _val Nothing = 0
          _val (Just x) = 1 + fromEnum x

-- | Convert a 'ComplexDomino' value to a unicode character rendering the domino
-- value /horizontally/.
dominoH
  :: ComplexDomino -- ^ The 'ComplexDomino' object to render horizontally.
  -> Char -- ^ The unicode character that represents the given 'ComplexDomino' value in a horizontal manner.
dominoH = _domino 0x1f030

-- | Convert a 'SimpleDomino' value to a unicode character rendering the domino
-- value /horizontally/.
dominoH'
  :: SimpleDomino -- ^ The 'SimpleDomino' object to render horizontally.
  -> Char -- ^ The unicode character that represents the given 'SimpleDomino' value in a horizontal manner.
dominoH' = dominoH . fmap Just

-- | Convert a 'ComplexDomino' value to a unicode character rendering the domino
-- value /vertically/.
dominoV
  :: ComplexDomino -- ^ The 'ComplexDomino' object to render vertically.
  -> Char -- ^ The unicode character that represents the given 'ComplexDomino' value in a vertical manner.
dominoV = _domino 0x1f062

-- | Convert a 'SimpleDomino' value to a unicode character rendering the domino
-- value /vertically/.
dominoV'
  :: SimpleDomino -- ^ The 'SimpleDomino' object to render vertically.
  -> Char -- ^ The unicode character that represents the given 'SimpleDomino' value in vertical manner.
dominoV' = dominoV . fmap Just

-- | Convert an 'OrientedDomino' to its unicode equivalent, where the sides of
-- the domino can be empty.
domino
  :: OrientedDomino (Maybe DieValue) -- ^ The 'OrientedDomino' to render.
  -> Char -- ^ The unicode characters that represents the 'OrientedDomino' value.
domino (Oriented d Horizontal) = dominoH d
domino (Oriented d Vertical) = dominoV d

-- | Convert an 'OrientedDomino' to its unicode equivalent, where the sides of
-- the domino can /not/ be empty.
domino'
  :: OrientedDomino DieValue -- ^ The 'OrientedDomino' to render.
  -> Char -- ^ The unicode characters that represents the 'OrientedDomino' value.
domino' = domino . fmap (fmap Just)
