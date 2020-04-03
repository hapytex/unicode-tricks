{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}

module Data.Char.Frame where

import Data.Bool(bool)

data Horizontal a = Horizontal { left :: a, right :: a } deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)
data Vertical a = Vertical { up :: a, down :: a } deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)
data Parts a = Parts (Vertical a) (Horizontal a) deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)
data Weight = Empty | Light | Heavy deriving (Bounded, Enum, Eq, Ord, Read, Show)

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

instance Applicative Horizontal where
    pure x = Horizontal x x
    Horizontal fa fb <*> Horizontal xa xb = Horizontal (fa xa) (fb xb)

instance Applicative Vertical where
    pure x = Vertical x x
    Vertical fa fb <*> Vertical xa xb = Vertical (fa xa) (fb xb)

instance Applicative Parts where
    pure x = Parts (pure x) (pure x)
    Parts fa fb <*> Parts xa xb = Parts (fa <*> xa) (fb <*> xb)

type Simple = Parts Bool
type Weighted = Parts Weight

simpleToWeighted :: Weight -> Simple -> Weighted
simpleToWeighted = fmap . bool Empty

simpleToLight :: Simple -> Weighted
simpleToLight = simpleToWeighted Light

simpleToHeavy :: Simple -> Weighted
simpleToHeavy = simpleToWeighted Heavy

simple :: Simple -> Char
simple = weighted . simpleToLight

simple' :: Simple -> Char
simple' = weighted . simpleToHeavy

simpleWithArc :: Simple -> Char
simpleWithArc (Parts (Vertical False True) (Horizontal False True)) = '\x256d'
simpleWithArc (Parts (Vertical False True) (Horizontal True False)) = '\x256e'
simpleWithArc (Parts (Vertical True False) (Horizontal False True)) = '\x256f'
simpleWithArc (Parts (Vertical True False) (Horizontal True False)) = '\x2570'
simpleWithArc x = simple x

weighted :: Weighted -> Char
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
