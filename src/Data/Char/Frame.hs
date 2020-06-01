{-# LANGUAGE DeriveTraversable, FlexibleInstances, PatternSynonyms, Safe #-}

{-|
Module      : Data.Char.Frame
Description : A module used to render frames with light and heavy lines.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A frame is represented as a pair of horizontal and vertical lines. These can be any items, but currently only booleans and weight objects are covered to convert the item to a corresponding character.
-}

module Data.Char.Frame(
    -- * Line weight
    Weight(Empty, Light, Heavy)
    -- * Datastructures to store the four directions
  , Horizontal(Horizontal, left, right)
  , Vertical(Vertical, up, down)
  , Parts(Parts)
    -- * Type aliasses and pattern synonyms for convenient 'Parts'
  , Simple, Weighted
  , pattern Frame
    -- * Functions to render specific frame values
  , simple, simple', simpleWithArc, weighted
    -- * Convert a 'Simple' to a 'Weighted'
  , simpleToWeighted, simpleToLight, simpleToHeavy, weightedToSimple
    -- * Convert a 'Char'acter to a frame
  , fromWeighted, fromWeighted', fromLight, fromLight', fromHeavy, fromHeavy', fromSimple, fromSimple'
  ) where

import Data.Bool(bool)
import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar'), UnicodeText)
import Data.Maybe(fromJust)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1, arbitraryBoundedEnum)

-- | A data type that determines the state of the /horizontal/ lines of
-- the frame ('left' and 'right').
data Horizontal a = Horizontal { 
    left :: a  -- ^ The state of the left line of the frame.
  , right :: a  -- ^ The state of the right line of the frame.
  } deriving (Bounded, Eq, Foldable, Functor, Ord, Read, Show, Traversable)

-- | A data type that determines the state of the /vertical/ lines of the frame
-- ('up' and 'down').
data Vertical a = Vertical {
    up :: a  -- ^ The state of the line in the up direction of the frame.
  , down :: a  -- ^ The state of the line in the down direction of the frame.
  } deriving (Bounded, Eq, Foldable, Functor, Ord, Read, Show, Traversable)

-- | A data type that specifies the four lines that should (not) be drawn for
-- the frame.
data Parts a = Parts (Vertical a) (Horizontal a) deriving (Bounded, Eq, Foldable, Functor, Ord, Read, Show, Traversable)

-- | The weights of the frame lines, these can be 'Empty', 'Light' or 'Heavy'.
data Weight
  = Empty  -- ^ The frame does not contain such line.
  | Light  -- ^ The frame contains such line.
  | Heavy  -- ^ The frame contains such line, in /boldface/.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Semigroup a => Semigroup (Horizontal a) where
    Horizontal a1 a2 <> Horizontal b1 b2 = Horizontal (a1 <> b1) (a2 <> b2)

instance Semigroup a => Semigroup (Vertical a) where
    Vertical a1 a2 <> Vertical b1 b2 = Vertical (a1 <> b1) (a2 <> b2)

instance Semigroup a => Semigroup (Parts a) where
    Parts a1 a2 <> Parts b1 b2 = Parts (a1 <> b1) (a2 <> b2)

instance Monoid a => Monoid (Horizontal a) where
    mempty = Horizontal mempty mempty

instance Monoid a => Monoid (Vertical a) where
    mempty = Vertical mempty mempty

instance Monoid a => Monoid (Parts a) where
    mempty = Parts mempty mempty

instance Arbitrary Weight where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary a => Arbitrary (Horizontal a) where
    arbitrary = arbitrary1

instance Arbitrary1 Horizontal where
    liftArbitrary arb = Horizontal <$> arb <*> arb

instance Arbitrary a => Arbitrary (Vertical a) where
    arbitrary = arbitrary1

instance Arbitrary1 Vertical where
    liftArbitrary arb = Vertical <$> arb <*> arb

instance Arbitrary a => Arbitrary (Parts a) where
    arbitrary = arbitrary1

instance Arbitrary1 Parts where
    liftArbitrary arb = Parts <$> liftArbitrary arb <*> liftArbitrary arb

instance Applicative Horizontal where
    pure x = Horizontal x x
    Horizontal fa fb <*> Horizontal xa xb = Horizontal (fa xa) (fb xb)

instance Applicative Vertical where
    pure x = Vertical x x
    Vertical fa fb <*> Vertical xa xb = Vertical (fa xa) (fb xb)

instance Applicative Parts where
    pure x = Parts (pure x) (pure x)
    Parts fa fb <*> Parts xa xb = Parts (fa <*> xa) (fb <*> xb)

-- | A pattern that makes pattern matching and expressions with 'Parts' more convenient.
pattern Frame
  :: a  -- ^ The state of the line in the /up/ direction.
  -> a  -- ^ The state of the line in the /down/ direction.
  -> a  -- ^ The state of the line in the /left/ direction.
  -> a  -- ^ The state of the line in the /right/ direction.
  -> Parts a  -- ^ The 'Parts' pattern with the state of the given lines.
pattern Frame u d l r = Parts (Vertical u d) (Horizontal l r)

-- | A type synonym that makes it more convenient to work with a 'Parts' object
-- that wraps 'Bool's. Usually 'True' means it should draw a line, and 'False'
-- that there is no line in that direction. The 'UnicodeCharacter' instance of a
-- 'Simple' works by converting 'True' to a 'Light', and vice versa.
type Simple = Parts Bool

-- | A type synonym that makes it more convenient to work with a 'Parts' object
-- that wraps 'Weight' objects. These specify the weight .
type Weighted = Parts Weight

-- | Convert a 'Weighted' object to a 'Simple' object by converting the 'Light'
-- and 'Heavy' parts to 'True' and the 'Empty' parts to 'False'.
weightedToSimple
  :: Weighted  -- ^ The 'Weighted' object to convert.
  -> Simple  -- ^ The 'Simple' object that takes "True' for parts that were 'Light' and 'Heavy'; and 'False' for 'Empty' parts.
weightedToSimple = fmap (Empty <)

-- | Convert a 'Simple' frame to a 'Weighted' frame by converting 'True' to the
-- given 'Weight' value.
simpleToWeighted
  :: Weight  -- ^ The 'Weight' that is used for 'True' values.
  -> Simple  -- ^ The 'Simple' frame to convert.
  -> Weighted  -- ^ The resulting 'Weighted' frame.
simpleToWeighted = fmap . bool Empty

-- | Convert a 'Simple' frame to a 'Weighted' frame by converting 'True' to
-- 'Light'.
simpleToLight
  :: Simple  -- ^ The 'Simple' frame to convert.
  -> Weighted  -- ^ The resulting 'Weighted' frame.
simpleToLight = simpleToWeighted Light

-- | Convert a 'Simple' frame to a 'Weighted' frame by converting 'True' to
-- 'Heavy'.
simpleToHeavy
  :: Simple  -- ^ The 'Simple frame to convert.
  -> Weighted  -- ^ The resulting 'Weighted' frame.
simpleToHeavy = simpleToWeighted Heavy

-- | Convert a 'Simple' frame to a corresponding 'Char'. Here 'True' is
-- mapped to a 'Light' line.
simple
  :: Simple  -- ^ The given 'Simple' frame to convert.
  -> Char  -- ^ The corresponding characer for this 'Simple' frame.
simple = weighted . simpleToLight

-- | Convert a 'Simple' frame to a corresponding 'Char'. Here 'True' is mapped
-- to a 'Heavy' line.
simple'
  :: Simple  -- ^ The given 'Simple' frame to convert.
  -> Char  -- ^ The corresponding characer for this 'Simple' frame.
simple' = weighted . simpleToHeavy

-- | Generate a 'Char' where turns are done with an /arc/ instead of a corner.
-- This can only be done for 'Light' lines.
simpleWithArc
  :: Simple  -- ^ The given 'Simple' frame to convert.
  -> Char  -- ^ The corresponding characer for this 'Simple' frame.
simpleWithArc (Parts (Vertical False True) (Horizontal False True)) = '\x256d'
simpleWithArc (Parts (Vertical False True) (Horizontal True False)) = '\x256e'
simpleWithArc (Parts (Vertical True False) (Horizontal False True)) = '\x256f'
simpleWithArc (Parts (Vertical True False) (Horizontal True False)) = '\x2570'
simpleWithArc x = simple x

-- | Converts a given 'Weighted' to the char that can be used to render frames.
weighted
  :: Weighted  -- ^ The 'Weighted' object that specifies how the lines on the four directions should look like.
  -> Char  -- ^ The character that represents these lines.
weighted (Parts (Vertical Empty Empty) (Horizontal Empty Empty)) = ' '
weighted (Parts (Vertical Empty Empty) (Horizontal Light Light)) = '\x2500'
weighted (Parts (Vertical Empty Empty) (Horizontal Heavy Heavy)) = '\x2501'
weighted (Parts (Vertical Light Light) (Horizontal Empty Empty)) = '\x2502'
weighted (Parts (Vertical Heavy Heavy) (Horizontal Empty Empty)) = '\x2503'
weighted (Parts (Vertical Empty Light) (Horizontal Empty Light)) = '\x250c'
weighted (Parts (Vertical Empty Light) (Horizontal Empty Heavy)) = '\x250d'
weighted (Parts (Vertical Empty Heavy) (Horizontal Empty Light)) = '\x250e'
weighted (Parts (Vertical Empty Heavy) (Horizontal Empty Heavy)) = '\x250f'
weighted (Parts (Vertical Empty Light) (Horizontal Light Empty)) = '\x2510'
weighted (Parts (Vertical Empty Light) (Horizontal Heavy Empty)) = '\x2511'
weighted (Parts (Vertical Empty Heavy) (Horizontal Light Empty)) = '\x2512'
weighted (Parts (Vertical Empty Heavy) (Horizontal Heavy Empty)) = '\x2513'
weighted (Parts (Vertical Light Empty) (Horizontal Empty Light)) = '\x2514'
weighted (Parts (Vertical Light Empty) (Horizontal Empty Heavy)) = '\x2515'
weighted (Parts (Vertical Heavy Empty) (Horizontal Empty Light)) = '\x2516'
weighted (Parts (Vertical Heavy Empty) (Horizontal Empty Heavy)) = '\x2517'
weighted (Parts (Vertical Light Empty) (Horizontal Light Empty)) = '\x2518'
weighted (Parts (Vertical Light Empty) (Horizontal Heavy Empty)) = '\x2519'
weighted (Parts (Vertical Heavy Empty) (Horizontal Light Empty)) = '\x251a'
weighted (Parts (Vertical Heavy Empty) (Horizontal Heavy Empty)) = '\x251b'
weighted (Parts (Vertical Light Light) (Horizontal Empty Light)) = '\x251c'
weighted (Parts (Vertical Light Light) (Horizontal Empty Heavy)) = '\x251d'
weighted (Parts (Vertical Heavy Light) (Horizontal Empty Light)) = '\x251e'
weighted (Parts (Vertical Light Heavy) (Horizontal Empty Light)) = '\x251f'
weighted (Parts (Vertical Heavy Heavy) (Horizontal Empty Light)) = '\x2520'
weighted (Parts (Vertical Heavy Light) (Horizontal Empty Heavy)) = '\x2521'
weighted (Parts (Vertical Light Heavy) (Horizontal Empty Heavy)) = '\x2522'
weighted (Parts (Vertical Heavy Heavy) (Horizontal Empty Heavy)) = '\x2523'
weighted (Parts (Vertical Light Light) (Horizontal Light Empty)) = '\x2524'
weighted (Parts (Vertical Light Light) (Horizontal Heavy Empty)) = '\x2525'
weighted (Parts (Vertical Heavy Light) (Horizontal Light Empty)) = '\x2526'
weighted (Parts (Vertical Light Heavy) (Horizontal Light Empty)) = '\x2527'
weighted (Parts (Vertical Heavy Heavy) (Horizontal Light Empty)) = '\x2528'
weighted (Parts (Vertical Heavy Light) (Horizontal Heavy Empty)) = '\x2529'
weighted (Parts (Vertical Light Heavy) (Horizontal Heavy Empty)) = '\x252a'
weighted (Parts (Vertical Heavy Heavy) (Horizontal Heavy Empty)) = '\x252b'
weighted (Parts (Vertical Empty Light) (Horizontal Light Light)) = '\x252c'
weighted (Parts (Vertical Empty Light) (Horizontal Heavy Light)) = '\x252d'
weighted (Parts (Vertical Empty Light) (Horizontal Light Heavy)) = '\x252e'
weighted (Parts (Vertical Empty Light) (Horizontal Heavy Heavy)) = '\x252f'
weighted (Parts (Vertical Empty Heavy) (Horizontal Light Light)) = '\x2530'
weighted (Parts (Vertical Empty Heavy) (Horizontal Heavy Light)) = '\x2531'
weighted (Parts (Vertical Empty Heavy) (Horizontal Light Heavy)) = '\x2532'
weighted (Parts (Vertical Empty Heavy) (Horizontal Heavy Heavy)) = '\x2533'
weighted (Parts (Vertical Light Empty) (Horizontal Light Light)) = '\x2534'
weighted (Parts (Vertical Light Empty) (Horizontal Heavy Light)) = '\x2535'
weighted (Parts (Vertical Light Empty) (Horizontal Light Heavy)) = '\x2536'
weighted (Parts (Vertical Light Empty) (Horizontal Heavy Heavy)) = '\x2537'
weighted (Parts (Vertical Heavy Empty) (Horizontal Light Light)) = '\x2538'
weighted (Parts (Vertical Heavy Empty) (Horizontal Heavy Light)) = '\x2539'
weighted (Parts (Vertical Heavy Empty) (Horizontal Light Heavy)) = '\x253a'
weighted (Parts (Vertical Heavy Empty) (Horizontal Heavy Heavy)) = '\x253b'
weighted (Parts (Vertical Light Light) (Horizontal Light Light)) = '\x253c'
weighted (Parts (Vertical Light Light) (Horizontal Heavy Light)) = '\x253d'
weighted (Parts (Vertical Light Light) (Horizontal Light Heavy)) = '\x253e'
weighted (Parts (Vertical Light Light) (Horizontal Heavy Heavy)) = '\x253f'
weighted (Parts (Vertical Heavy Light) (Horizontal Light Light)) = '\x2540'
weighted (Parts (Vertical Light Heavy) (Horizontal Light Light)) = '\x2541'
weighted (Parts (Vertical Heavy Heavy) (Horizontal Light Light)) = '\x2542'
weighted (Parts (Vertical Heavy Light) (Horizontal Heavy Light)) = '\x2543'
weighted (Parts (Vertical Heavy Light) (Horizontal Light Heavy)) = '\x2544'
weighted (Parts (Vertical Light Heavy) (Horizontal Heavy Light)) = '\x2545'
weighted (Parts (Vertical Light Heavy) (Horizontal Light Heavy)) = '\x2546'
weighted (Parts (Vertical Heavy Light) (Horizontal Heavy Heavy)) = '\x2547'
weighted (Parts (Vertical Light Heavy) (Horizontal Heavy Heavy)) = '\x2548'
weighted (Parts (Vertical Heavy Heavy) (Horizontal Heavy Light)) = '\x2549'
weighted (Parts (Vertical Heavy Heavy) (Horizontal Light Heavy)) = '\x254a'
weighted (Parts (Vertical Heavy Heavy) (Horizontal Heavy Heavy)) = '\x254b'
weighted (Parts (Vertical Empty Empty) (Horizontal Light Empty)) = '\x2574'
weighted (Parts (Vertical Light Empty) (Horizontal Empty Empty)) = '\x2575'
weighted (Parts (Vertical Empty Empty) (Horizontal Empty Light)) = '\x2576'
weighted (Parts (Vertical Empty Light) (Horizontal Empty Empty)) = '\x2577'
weighted (Parts (Vertical Empty Empty) (Horizontal Heavy Empty)) = '\x2578'
weighted (Parts (Vertical Heavy Empty) (Horizontal Empty Empty)) = '\x2579'
weighted (Parts (Vertical Empty Empty) (Horizontal Empty Heavy)) = '\x257a'
weighted (Parts (Vertical Empty Heavy) (Horizontal Empty Empty)) = '\x257b'
weighted (Parts (Vertical Empty Empty) (Horizontal Light Heavy)) = '\x257c'
weighted (Parts (Vertical Light Heavy) (Horizontal Empty Empty)) = '\x257d'
weighted (Parts (Vertical Empty Empty) (Horizontal Heavy Light)) = '\x257e'
weighted (Parts (Vertical Heavy Light) (Horizontal Empty Empty)) = '\x257f'

-- | Convert the given 'Char'acter to a 'Parts' object of 'Weight' objects.
-- If the given 'Char'acter is not a /frame/ of 'Weight's, the result is
-- unspecified.
fromWeighted'
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Weighted  -- ^ The equivalent 'Weighted' object.
fromWeighted' = fromJust . fromWeighted

-- | Convert the given 'Char'acter to the equivalent 'Simple' object wrapped in
-- a 'Just' data constructor if it exists; 'Nothing' otherwise. The parts of the
-- frame should only be 'Empty' or 'Light', if it contains a 'Heavy' object
-- 'Nothing' is returned.
fromLight
  :: Char  -- ^ The given 'Char'acter to convert to a 'Simple'.
  -> Maybe Simple  -- ^ The equivalent 'Simple' object wrapped in a 'Just' data constructor if it exists; 'Nothing' otherwise.
fromLight ' ' = Just (Parts (Vertical False False) (Horizontal False False))
fromLight '\x2500' = Just (Parts (Vertical False False) (Horizontal True True))
fromLight '\x2502' = Just (Parts (Vertical True True) (Horizontal False False))
fromLight '\x250c' = Just (Parts (Vertical False True) (Horizontal False True))
fromLight '\x2510' = Just (Parts (Vertical False True) (Horizontal True False))
fromLight '\x2514' = Just (Parts (Vertical True False) (Horizontal False True))
fromLight '\x2518' = Just (Parts (Vertical True False) (Horizontal True False))
fromLight '\x251c' = Just (Parts (Vertical True True) (Horizontal False True))
fromLight '\x2524' = Just (Parts (Vertical True True) (Horizontal True False))
fromLight '\x252c' = Just (Parts (Vertical False True) (Horizontal True True))
fromLight '\x2534' = Just (Parts (Vertical True False) (Horizontal True True))
fromLight '\x253c' = Just (Parts (Vertical True True) (Horizontal True True))
fromLight '\x2574' = Just (Parts (Vertical False False) (Horizontal True False))
fromLight '\x2575' = Just (Parts (Vertical True False) (Horizontal False False))
fromLight '\x2576' = Just (Parts (Vertical False False) (Horizontal False True))
fromLight '\x2577' = Just (Parts (Vertical False True) (Horizontal False False))
fromLight _ = Nothing

-- | Convert the given 'Char'acter to the equivalent 'Simple' object if it
-- exists; unspecified output otherwise. The parts of the frame should only be
-- 'Empty' or 'Light'.
fromLight'
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Simple  -- ^ The equivalent 'Simple' object looking at 'Empty' and 'Light' parts.
fromLight' = fromJust . fromLight

-- | Convert the given 'Char'acter to the equivalent 'Simple' object wrapped in
-- a 'Just' data constructor if it exists; 'Nothing' otherwise. The parts of the
-- frame should only be 'Empty' or 'Heavy', if it contains a 'Light' object
-- 'Nothing' is returned.
fromHeavy
  :: Char  -- ^ The given 'Char'acter to convert to a 'Simple'.
  -> Maybe Simple  -- ^ The equivalent 'Simple' object wrapped in a 'Just' data constructor if it exists; 'Nothing' otherwise.
fromHeavy ' ' = Just (Parts (Vertical False False) (Horizontal False False))
fromHeavy '\x2501' = Just (Parts (Vertical False False) (Horizontal True True))
fromHeavy '\x2503' = Just (Parts (Vertical True True) (Horizontal False False))
fromHeavy '\x250f' = Just (Parts (Vertical False True) (Horizontal False True))
fromHeavy '\x2513' = Just (Parts (Vertical False True) (Horizontal True False))
fromHeavy '\x2517' = Just (Parts (Vertical True False) (Horizontal False True))
fromHeavy '\x251b' = Just (Parts (Vertical True False) (Horizontal True False))
fromHeavy '\x2523' = Just (Parts (Vertical True True) (Horizontal False True))
fromHeavy '\x252b' = Just (Parts (Vertical True True) (Horizontal True False))
fromHeavy '\x2533' = Just (Parts (Vertical False True) (Horizontal True True))
fromHeavy '\x253b' = Just (Parts (Vertical True False) (Horizontal True True))
fromHeavy '\x254b' = Just (Parts (Vertical True True) (Horizontal True True))
fromHeavy '\x2578' = Just (Parts (Vertical False False) (Horizontal True False))
fromHeavy '\x2579' = Just (Parts (Vertical True False) (Horizontal False False))
fromHeavy '\x257a' = Just (Parts (Vertical False False) (Horizontal False True))
fromHeavy '\x257b' = Just (Parts (Vertical False True) (Horizontal False False))
fromHeavy _ = Nothing

-- | Convert the given 'Char'acter to the equivalent 'Simple' object if it
-- exists; unspecified output otherwise. The parts of the frame should only be
-- 'Empty' or 'Heavy'.
fromHeavy'
  :: Char  -- ^ The given 'Char'acter to convert.
  -> Simple  -- ^ The equivalent 'Simple' object looking at 'Empty' and 'Heavy' parts.
fromHeavy' = fromJust . fromHeavy

-- | Convert the given 'Char'acter to a 'Simple', if no such 'Simple' object
-- exists, the output is unspecified. Parts that are 'Light' or 'Heavy' are
-- mapped to 'True', and parts that are 'Empty' are mapped to 'False'.
fromSimple'
  :: Char  -- ^ The given 'Char'acter to convert'.
  -> Simple  -- ^ The equivalent 'Simple' object if it exists.
fromSimple' = weightedToSimple . fromWeighted'

-- | Convert the given 'Char'acter to a 'Simple' object wrapped in a 'Just' if
-- such 'Simple' object exists; 'Nothing' otherwise. Parts that are 'Light' or
-- 'Heavy' are mapped to 'True', and parts that are 'Empty' are mapped to
-- 'False'.
fromSimple
  :: Char  -- The given 'Char'acter to convert.
  -> Maybe Simple  -- ^ Ther equivalent 'Simple' object wrapped in a 'Just' data constructor if it exists; 'Nothing' otherwise.
fromSimple = fmap weightedToSimple . fromWeighted

-- | Convert the given 'Char'acter to a 'Parts' object of 'Weight' objects
-- wrapped in a 'Just' data constructor if it is a /block/ character; 'Nothing'
-- otherwise.
fromWeighted
  :: Char  -- ^ The given 'Char'acter to convert to a 'Weighted' object.
  -> Maybe Weighted  -- ^ A 'Weighted' object wrapped in a 'Just' if the character is a frame of 'Weight's; 'Nothing' otherwise.
fromWeighted ' ' = Just (Parts (Vertical Empty Empty) (Horizontal Empty Empty))
fromWeighted '\x2500' = Just (Parts (Vertical Empty Empty) (Horizontal Light Light))
fromWeighted '\x2501' = Just (Parts (Vertical Empty Empty) (Horizontal Heavy Heavy))
fromWeighted '\x2502' = Just (Parts (Vertical Light Light) (Horizontal Empty Empty))
fromWeighted '\x2503' = Just (Parts (Vertical Heavy Heavy) (Horizontal Empty Empty))
fromWeighted '\x250c' = Just (Parts (Vertical Empty Light) (Horizontal Empty Light))
fromWeighted '\x250d' = Just (Parts (Vertical Empty Light) (Horizontal Empty Heavy))
fromWeighted '\x250e' = Just (Parts (Vertical Empty Heavy) (Horizontal Empty Light))
fromWeighted '\x250f' = Just (Parts (Vertical Empty Heavy) (Horizontal Empty Heavy))
fromWeighted '\x2510' = Just (Parts (Vertical Empty Light) (Horizontal Light Empty))
fromWeighted '\x2511' = Just (Parts (Vertical Empty Light) (Horizontal Heavy Empty))
fromWeighted '\x2512' = Just (Parts (Vertical Empty Heavy) (Horizontal Light Empty))
fromWeighted '\x2513' = Just (Parts (Vertical Empty Heavy) (Horizontal Heavy Empty))
fromWeighted '\x2514' = Just (Parts (Vertical Light Empty) (Horizontal Empty Light))
fromWeighted '\x2515' = Just (Parts (Vertical Light Empty) (Horizontal Empty Heavy))
fromWeighted '\x2516' = Just (Parts (Vertical Heavy Empty) (Horizontal Empty Light))
fromWeighted '\x2517' = Just (Parts (Vertical Heavy Empty) (Horizontal Empty Heavy))
fromWeighted '\x2518' = Just (Parts (Vertical Light Empty) (Horizontal Light Empty))
fromWeighted '\x2519' = Just (Parts (Vertical Light Empty) (Horizontal Heavy Empty))
fromWeighted '\x251a' = Just (Parts (Vertical Heavy Empty) (Horizontal Light Empty))
fromWeighted '\x251b' = Just (Parts (Vertical Heavy Empty) (Horizontal Heavy Empty))
fromWeighted '\x251c' = Just (Parts (Vertical Light Light) (Horizontal Empty Light))
fromWeighted '\x251d' = Just (Parts (Vertical Light Light) (Horizontal Empty Heavy))
fromWeighted '\x251e' = Just (Parts (Vertical Heavy Light) (Horizontal Empty Light))
fromWeighted '\x251f' = Just (Parts (Vertical Light Heavy) (Horizontal Empty Light))
fromWeighted '\x2520' = Just (Parts (Vertical Heavy Heavy) (Horizontal Empty Light))
fromWeighted '\x2521' = Just (Parts (Vertical Heavy Light) (Horizontal Empty Heavy))
fromWeighted '\x2522' = Just (Parts (Vertical Light Heavy) (Horizontal Empty Heavy))
fromWeighted '\x2523' = Just (Parts (Vertical Heavy Heavy) (Horizontal Empty Heavy))
fromWeighted '\x2524' = Just (Parts (Vertical Light Light) (Horizontal Light Empty))
fromWeighted '\x2525' = Just (Parts (Vertical Light Light) (Horizontal Heavy Empty))
fromWeighted '\x2526' = Just (Parts (Vertical Heavy Light) (Horizontal Light Empty))
fromWeighted '\x2527' = Just (Parts (Vertical Light Heavy) (Horizontal Light Empty))
fromWeighted '\x2528' = Just (Parts (Vertical Heavy Heavy) (Horizontal Light Empty))
fromWeighted '\x2529' = Just (Parts (Vertical Heavy Light) (Horizontal Heavy Empty))
fromWeighted '\x252a' = Just (Parts (Vertical Light Heavy) (Horizontal Heavy Empty))
fromWeighted '\x252b' = Just (Parts (Vertical Heavy Heavy) (Horizontal Heavy Empty))
fromWeighted '\x252c' = Just (Parts (Vertical Empty Light) (Horizontal Light Light))
fromWeighted '\x252d' = Just (Parts (Vertical Empty Light) (Horizontal Heavy Light))
fromWeighted '\x252e' = Just (Parts (Vertical Empty Light) (Horizontal Light Heavy))
fromWeighted '\x252f' = Just (Parts (Vertical Empty Light) (Horizontal Heavy Heavy))
fromWeighted '\x2530' = Just (Parts (Vertical Empty Heavy) (Horizontal Light Light))
fromWeighted '\x2531' = Just (Parts (Vertical Empty Heavy) (Horizontal Heavy Light))
fromWeighted '\x2532' = Just (Parts (Vertical Empty Heavy) (Horizontal Light Heavy))
fromWeighted '\x2533' = Just (Parts (Vertical Empty Heavy) (Horizontal Heavy Heavy))
fromWeighted '\x2534' = Just (Parts (Vertical Light Empty) (Horizontal Light Light))
fromWeighted '\x2535' = Just (Parts (Vertical Light Empty) (Horizontal Heavy Light))
fromWeighted '\x2536' = Just (Parts (Vertical Light Empty) (Horizontal Light Heavy))
fromWeighted '\x2537' = Just (Parts (Vertical Light Empty) (Horizontal Heavy Heavy))
fromWeighted '\x2538' = Just (Parts (Vertical Heavy Empty) (Horizontal Light Light))
fromWeighted '\x2539' = Just (Parts (Vertical Heavy Empty) (Horizontal Heavy Light))
fromWeighted '\x253a' = Just (Parts (Vertical Heavy Empty) (Horizontal Light Heavy))
fromWeighted '\x253b' = Just (Parts (Vertical Heavy Empty) (Horizontal Heavy Heavy))
fromWeighted '\x253c' = Just (Parts (Vertical Light Light) (Horizontal Light Light))
fromWeighted '\x253d' = Just (Parts (Vertical Light Light) (Horizontal Heavy Light))
fromWeighted '\x253e' = Just (Parts (Vertical Light Light) (Horizontal Light Heavy))
fromWeighted '\x253f' = Just (Parts (Vertical Light Light) (Horizontal Heavy Heavy))
fromWeighted '\x2540' = Just (Parts (Vertical Heavy Light) (Horizontal Light Light))
fromWeighted '\x2541' = Just (Parts (Vertical Light Heavy) (Horizontal Light Light))
fromWeighted '\x2542' = Just (Parts (Vertical Heavy Heavy) (Horizontal Light Light))
fromWeighted '\x2543' = Just (Parts (Vertical Heavy Light) (Horizontal Heavy Light))
fromWeighted '\x2544' = Just (Parts (Vertical Heavy Light) (Horizontal Light Heavy))
fromWeighted '\x2545' = Just (Parts (Vertical Light Heavy) (Horizontal Heavy Light))
fromWeighted '\x2546' = Just (Parts (Vertical Light Heavy) (Horizontal Light Heavy))
fromWeighted '\x2547' = Just (Parts (Vertical Heavy Light) (Horizontal Heavy Heavy))
fromWeighted '\x2548' = Just (Parts (Vertical Light Heavy) (Horizontal Heavy Heavy))
fromWeighted '\x2549' = Just (Parts (Vertical Heavy Heavy) (Horizontal Heavy Light))
fromWeighted '\x254a' = Just (Parts (Vertical Heavy Heavy) (Horizontal Light Heavy))
fromWeighted '\x254b' = Just (Parts (Vertical Heavy Heavy) (Horizontal Heavy Heavy))
fromWeighted '\x2574' = Just (Parts (Vertical Empty Empty) (Horizontal Light Empty))
fromWeighted '\x2575' = Just (Parts (Vertical Light Empty) (Horizontal Empty Empty))
fromWeighted '\x2576' = Just (Parts (Vertical Empty Empty) (Horizontal Empty Light))
fromWeighted '\x2577' = Just (Parts (Vertical Empty Light) (Horizontal Empty Empty))
fromWeighted '\x2578' = Just (Parts (Vertical Empty Empty) (Horizontal Heavy Empty))
fromWeighted '\x2579' = Just (Parts (Vertical Heavy Empty) (Horizontal Empty Empty))
fromWeighted '\x257a' = Just (Parts (Vertical Empty Empty) (Horizontal Empty Heavy))
fromWeighted '\x257b' = Just (Parts (Vertical Empty Heavy) (Horizontal Empty Empty))
fromWeighted '\x257c' = Just (Parts (Vertical Empty Empty) (Horizontal Light Heavy))
fromWeighted '\x257d' = Just (Parts (Vertical Light Heavy) (Horizontal Empty Empty))
fromWeighted '\x257e' = Just (Parts (Vertical Empty Empty) (Horizontal Heavy Light))
fromWeighted '\x257f' = Just (Parts (Vertical Heavy Light) (Horizontal Empty Empty))
fromWeighted _ = Nothing

instance UnicodeCharacter (Parts Weight) where
    toUnicodeChar = weighted
    fromUnicodeChar = fromWeighted
    fromUnicodeChar' = fromWeighted'

instance UnicodeCharacter (Parts Bool) where
    toUnicodeChar = simple
    fromUnicodeChar = fromLight
    fromUnicodeChar' = fromLight'

instance UnicodeText (Parts Weight)
instance UnicodeText (Parts Bool)
