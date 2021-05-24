{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveTraversable, FlexibleInstances, Safe #-}

{-|
Module      : Data.Char.Block
Description : A module used to render blocks in unicode.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode has 2-by-2 blocks, this module aims to make it more convenient to render such blocks.
-}

module Data.Char.Block(
    -- * Datastructures to store the state of the frame.
    Row(Row, left, right)
  , Block(Block, upper, lower)
    -- * A unicode character that is (partially) filled block.
  , filled
    -- * Convert a 'Char'acter to a (partially) filled block.
  , fromBlock, fromBlock'
  ) where

import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar), UnicodeText)
import Data.Data(Data)
import Data.Hashable(Hashable)
import Data.Hashable.Lifted(Hashable1)
import Data.Maybe(fromJust)

import GHC.Generics(Generic, Generic1)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)

-- | A data type that determines the state of the /row/ in a block.
-- it determines the left and the right part of the row of the block.
data Row a = Row {
    left :: a  -- ^ The left part of a row of the block.
  , right :: a  -- ^ The right part of the row of the block.
  } deriving (Bounded, Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

instance Hashable1 Row
instance Hashable a => Hashable (Row a)

-- | A data type that determines the state of the four subparts of the block.
data Block a = Block {
    upper :: Row a  -- ^ The upper part of the block.
  , lower :: Row a  -- ^ The lower part of the block.
  } deriving (Bounded, Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

instance Hashable a => Hashable (Block a)
instance Hashable1 Block

instance Applicative Row where
    pure x = Row x x
    Row fl fr <*> Row l r = Row (fl l) (fr r)

instance Applicative Block where
    pure x = Block (pure x) (pure x)
    Block fu fl <*> Block u l = Block (fu <*> u) (fl <*> l)

instance Arbitrary a => Arbitrary (Row a) where
    arbitrary = arbitrary1

instance Arbitrary1 Row where
    liftArbitrary arb = Row <$> arb <*> arb

instance Arbitrary a => Arbitrary (Block a) where
    arbitrary = arbitrary1

instance Arbitrary1 Block where
    liftArbitrary arb = Block <$> arb' <*> arb'
        where arb' = liftArbitrary arb

-- | Convert the given 'Char'acter to a 'Block' of 'Bool's wrapped in a 'Just'
-- if it exists; 'Nothing' otherwise.
fromBlock
  :: Char  -- ^ The given 'Char'acter to convert to a 'Block' of 'Bool's.
  -> Maybe (Block Bool)  -- The equivalent 'Block' of 'Bool's wrapped in a 'Just' if such block exists; 'Nothing' otherwise.
fromBlock ' ' = Just (Block (Row False False) (Row False False))
fromBlock '\x2580' = Just (Block (Row True  True ) (Row False False))
fromBlock '\x2584' = Just (Block (Row False False) (Row True  True ))
fromBlock '\x2588' = Just (Block (Row True  True ) (Row True  True ))
fromBlock '\x258c' = Just (Block (Row True  False) (Row True  False))
fromBlock '\x2590' = Just (Block (Row False True ) (Row False True ))
fromBlock '\x2596' = Just (Block (Row False False) (Row True  False))
fromBlock '\x2597' = Just (Block (Row False False) (Row False True ))
fromBlock '\x2598' = Just (Block (Row True  False) (Row False False))
fromBlock '\x2599' = Just (Block (Row True  False) (Row True  True ))
fromBlock '\x259a' = Just (Block (Row True  False) (Row False True ))
fromBlock '\x259b' = Just (Block (Row True  True ) (Row True  False))
fromBlock '\x259c' = Just (Block (Row True  True ) (Row False True ))
fromBlock '\x259d' = Just (Block (Row False True ) (Row False False))
fromBlock '\x259e' = Just (Block (Row False True ) (Row True  False))
fromBlock '\x259f' = Just (Block (Row False True ) (Row True  True ))
fromBlock _ = Nothing

-- | Convert the given 'Char'acter to a 'Block' of 'Bool's if it exists; unspecified result otherwise.
fromBlock'
  :: Char  -- ^ The given 'Char'acter to convert to a 'Block' of 'Bool's.
  -> Block Bool  -- ^ The equivalent 'Block' of 'Bool's.
fromBlock' = fromJust . fromBlock

-- | Convert the given 'Block' value to a block character in unicode.
-- 'True' means that part is filled, and 'False' means the part is not filled.
filled
    :: Block Bool  -- ^ The given 'Block' of 'Bool's to convert to a 'Char'acter.
    -> Char  -- ^ The equivalent Unicode 'Char'acter for the given 'Block' of 'Bool's.
filled (Block (Row False False) (Row False False)) = ' '
filled (Block (Row True  True ) (Row False False)) = '\x2580'
filled (Block (Row False False) (Row True  True )) = '\x2584'
filled (Block (Row True  True ) (Row True  True )) = '\x2588'
filled (Block (Row True  False) (Row True  False)) = '\x258c'
filled (Block (Row False True ) (Row False True )) = '\x2590'
filled (Block (Row False False) (Row True  False)) = '\x2596'
filled (Block (Row False False) (Row False True )) = '\x2597'
filled (Block (Row True  False) (Row False False)) = '\x2598'
filled (Block (Row True  False) (Row True  True )) = '\x2599'
filled (Block (Row True  False) (Row False True )) = '\x259a'
filled (Block (Row True  True ) (Row True  False)) = '\x259b'
filled (Block (Row True  True ) (Row False True )) = '\x259c'
filled (Block (Row False True ) (Row False False)) = '\x259d'
filled (Block (Row False True ) (Row True  False)) = '\x259e'
filled (Block (Row False True ) (Row True  True )) = '\x259f'

instance UnicodeCharacter (Block Bool) where
    toUnicodeChar = filled
    fromUnicodeChar = fromBlock

instance UnicodeText (Block Bool)
