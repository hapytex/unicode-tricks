{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Brackets
Description : Determine and manipulate bracket characters.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode considers 60 characters to be brackets: brackets are organized in /pairs/: each opening bracket has a corresponding closing bracket and vice versa.

The following characters are considered brackets where the first character is closed by the last character, the second by the last but one, etc.:

@
([{&#3898;&#3900;&#5787;&#8261;&#8317;&#8333;&#8968;&#8970;&#9001;&#10088;&#10090;&#10092;&#10094;&#10096;&#10098;&#10100;&#10181;&#10214;&#10216;&#10218;&#10220;&#10222;&#10627;&#10629;&#10631;&#10633;&#10635;&#10637;&#10639;&#10641;&#10643;&#10645;&#10647;&#10712;&#10714;&#10748;&#11810;&#11812;&#11814;&#11816;&#12296;&#12298;&#12300;&#12302;&#12304;&#12308;&#12310;&#12312;&#12314;&#65113;&#65115;&#65117;&#65288;&#65339;&#65371;&#65375;&#65378;&#65379;&#65376;&#65373;&#65341;&#65289;&#65118;&#65116;&#65114;&#12315;&#12313;&#12311;&#12309;&#12305;&#12303;&#12301;&#12299;&#12297;&#11817;&#11815;&#11813;&#11811;&#10749;&#10715;&#10713;&#10648;&#10646;&#10644;&#10642;&#10638;&#10640;&#10636;&#10634;&#10632;&#10630;&#10628;&#10223;&#10221;&#10219;&#10217;&#10215;&#10182;&#10101;&#10099;&#10097;&#10095;&#10093;&#10091;&#10089;&#9002;&#8971;&#8969;&#8334;&#8318;&#8262;&#5788;&#3901;&#3899;}])
@

These characters span over several code blocks.
-}


module Data.Char.Brackets (
  -- * Listing and converting brackets
    bracketMaps, brackets, openBrackets, closeBrackets, toOpen, toClose
  -- * Check the given bracket type
  , BracketType(Open, Close), isBracket, bracketType, bracketType', isOpenBracket, isCloseBracket
  -- * Determine the opposite bracket
  , getOppositeChar, getOppositeChar'
  ) where

import Prelude hiding (lookup)

import Control.Applicative((<|>))

import Data.Map(Map, fromList, lookup, member)
import Data.Maybe(fromMaybe)
import Data.Tuple(swap)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

-- | A data type that is used to specify the /type/ of bracket.
data BracketType
  = Open  -- ^ The bracket is used to "open" a context.
  | Close  -- ^ The bracket is used to "close" a context.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Arbitrary BracketType where
  arbitrary = arbitraryBoundedEnum

-- | A list of 2-tuples where the first item
-- of each tuple is the opening bracket, and the
-- second item its closing counterpart.
bracketMaps :: [(Char, Char)]
bracketMaps = [
    ('(', ')')
  , ('[', ']')
  , ('{', '}')
  , ('\x0f3a', '\x0f3b')
  , ('\x0f3c', '\x0f3d')
  , ('\x169b', '\x169c')
  , ('\x2045', '\x2046')
  , ('\x207d', '\x207e')
  , ('\x208d', '\x208e')
  , ('\x2308', '\x2309')
  , ('\x230a', '\x230b')
  , ('\x2329', '\x232a')
  , ('\x2768', '\x2769')
  , ('\x276a', '\x276b')
  , ('\x276c', '\x276d')
  , ('\x276e', '\x276f')
  , ('\x2770', '\x2771')
  , ('\x2772', '\x2773')
  , ('\x2774', '\x2775')
  , ('\x27c5', '\x27c6')
  , ('\x27e6', '\x27e7')
  , ('\x27e8', '\x27e9')
  , ('\x27ea', '\x27eb')
  , ('\x27ec', '\x27ed')
  , ('\x27ee', '\x27ef')
  , ('\x2983', '\x2984')
  , ('\x2985', '\x2986')
  , ('\x2987', '\x2988')
  , ('\x2989', '\x298a')
  , ('\x298b', '\x298c')
  , ('\x298d', '\x2990')
  , ('\x298f', '\x298e')
  , ('\x2991', '\x2992')
  , ('\x2993', '\x2994')
  , ('\x2995', '\x2996')
  , ('\x2997', '\x2998')
  , ('\x29d8', '\x29d9')
  , ('\x29da', '\x29db')
  , ('\x29fc', '\x29fd')
  , ('\x2e22', '\x2e23')
  , ('\x2e24', '\x2e25')
  , ('\x2e26', '\x2e27')
  , ('\x2e28', '\x2e29')
  , ('\x3008', '\x3009')
  , ('\x300a', '\x300b')
  , ('\x300c', '\x300d')
  , ('\x300e', '\x300f')
  , ('\x3010', '\x3011')
  , ('\x3014', '\x3015')
  , ('\x3016', '\x3017')
  , ('\x3018', '\x3019')
  , ('\x301a', '\x301b')
  , ('\xfe59', '\xfe5a')
  , ('\xfe5b', '\xfe5c')
  , ('\xfe5d', '\xfe5e')
  , ('\xff08', '\xff09')
  , ('\xff3b', '\xff3d')
  , ('\xff5b', '\xff5d')
  , ('\xff5f', '\xff60')
  , ('\xff62', '\xff63')
  ]

-- | The list of all brackets characters.
brackets :: [Char]  -- ^ The list of all 'Char's that are brackets.
brackets = [ci | ~(ca, cb) <- bracketMaps, ci <- [ca, cb]]

-- | A list of 'Char's that contains all opening brackets.
openBrackets :: [Char]  -- ^ The list of all 'Char's that are /opening brackets/.
openBrackets = map fst bracketMaps

-- | A list of 'Char's that contains all closing brackets.
closeBrackets :: [Char]  -- ^ The list of all 'Char's that are /closing brackets/.
closeBrackets = map snd bracketMaps

-- | A 'Map' that maps the given /open bracket/
-- characters to the corresponding /close bracket/.
toClose :: Map Char Char
toClose = fromList bracketMaps

-- | A 'Map' that maps the given /close bracket/
-- characters to the corresponding /open bracket/.
toOpen :: Map Char Char
toOpen = fromList (map swap bracketMaps)

-- | Check if the given 'Char' is a /bracket/ character.
isBracket
  :: Char -- ^ The given 'Char' to test.
  -> Bool  -- ^ 'True' if the given 'Char' is an open bracket; 'False' otherwise.
isBracket c = go toClose || go toOpen
  where go = member c

-- | Check if the given 'Char' is an /open bracket/.
isOpenBracket
  :: Char  -- ^ The given 'Char' to test.
  -> Bool  -- ^ 'True' if the 'Char' is an /open bracket/; 'False' otherwise.
isOpenBracket = (`member` toClose)

-- | Check if the given 'Char' is a /close bracket/.
isCloseBracket
  :: Char  -- ^ The given 'Char' to test.
  -> Bool  -- ^ 'True' if the 'Char' is an /close bracket/; 'False' otherwise.
isCloseBracket = (`member` toOpen)

-- | Check the 'BracketType' of the 'Char' wrapped in a 'Just' data construct;
-- 'Nothing' if the given 'Char' is /not/ a /bracket/ character.
bracketType :: Char -> Maybe BracketType
bracketType c
  | go toClose = Just Open
  | go toOpen = Just Close
  | otherwise = Nothing
  where go = member c

-- | Check the 'BracketType' of the 'Char'. For a 'Char' that is /not/ a /bracket/
-- the behavior is unspecified.
bracketType' :: Char -> BracketType
bracketType' c
  | member c toClose = Open
  | otherwise = Close

-- | Get the bracket character that is the /counterpart/
-- of the given /bracket/ character wrapped in a 'Just' data
-- constructor. If the given 'Char' is not a bracket, 'Nothing'
-- is returned.
getOppositeChar
  :: Char  -- ^ The given 'Char' for which we want to determine the opposite bracket.
  -> Maybe Char  -- ^ The opposite bracket wrapped in a 'Just' if the given 'Char' is a bracket character; 'Nothing' otherwise.
getOppositeChar c = go toClose <|> go toOpen
  where go = lookup c

-- | Get the bracket character that is the /counterpart/
-- of the given /bracket/ character. If the given 'Char'
-- is not a bracket, the given 'Char' is returned.
getOppositeChar'
  :: Char  -- ^ The given 'Char' for which we want to determine the opposite bracket.
  -> Char  -- ^ The opposite bracket if the given 'Char' is a /bracket/; otherwise the given 'Char'.
getOppositeChar' = fromMaybe <*> getOppositeChar
