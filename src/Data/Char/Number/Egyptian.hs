{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Number.Egyptian
Description : A module to generate ancient Egyptian numerals.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

One can make use of a <http://unicode.org/charts/PDF/U13000.pdf.pdf block of Unicode characters> to typeset ancient /Egyptian/ hieroglyphs. This module aims to make it more convenient
to work with ancient Egyptian numerals.

Ancient Egyptian numerals use a /sign-value system/ with radix 10. This means that there are hieroglyphs for 1 to 9, for 10, 20, ..., 90, for 100, 200, ..., 900, etc. With the /heh/ as
largest value denoting one million.

In Egyptian numerals, one could write numbers left-to-right and right-to-left (or even vertical). The human or animal figures always point to the /beginning/ of the line.
Because in the Unicode standard the /heh/ (denoting one million) looks to the left, the values are written left-to-right.
-}

module Data.Char.Number.Egyptian (
    -- * Ancient plus and minus
    plus, minus
    -- * Ancient Egyptian characters for powers of 10
  , nfr, singleStroke, cattleHobble, coilOfRope, waterLily, bentFinger, tadpole, heh
    -- * Ancient Egyptian characters for numbers
  , singleStroke1, singleStroke2, singleStroke3, singleStroke4, singleStroke5, singleStroke6, singleStroke7, singleStroke8, singleStroke9, singleStroke5'
  , cattleHobble1, cattleHobble2, cattleHobble3, cattleHobble4, cattleHobble5, cattleHobble6, cattleHobble7, cattleHobble8, cattleHobble9, cattleHobble2', cattleHobble3', cattleHobble4', cattleHobble5'
  , coilOfRope1,   coilOfRope2,   coilOfRope3,   coilOfRope4,   coilOfRope5,   coilOfRope6,   coilOfRope7,   coilOfRope8,   coilOfRope9,   coilOfRope5'
  , waterLily1,    waterLily2,    waterLily3,    waterLily4,    waterLily5,    waterLily6,    waterLily7,    waterLily8,    waterLily9
  , bentFinger1,   bentFinger2,   bentFinger3,   bentFinger4,   bentFinger5,   bentFinger6,   bentFinger7,   bentFinger8,   bentFinger9,   bentFinger5'
    -- * Generating ancient Egyptian numerals
  , egyptianNumber, egyptianNumber', egyptianNumber''
  ) where

import Data.Char(chr, ord)
import Data.Char.Core(Ligate, PlusStyle, signValueSystem, splitLigate)
import Data.Default(def)
import qualified Data.Text as T
import Data.Text(Text, singleton, empty)

-- | The single stroke character, an ancient Egyptian character to denote /one/. This is hieroglyph Z015.
singleStroke :: Char  -- ^ The ancient Egyptian character to denote /one/.
singleStroke = '\x133fa'

-- | The single stroke character, an ancient Egyptian character to denote /one/. This is hieroglyph Z015.
singleStroke1 :: Char  -- ^ The ancient Egyptian character to denote /one/.
singleStroke1 = singleStroke

-- | The double stroke character, an ancient Egyptian character to denote /two/. This is hieroglyph Z015A.
singleStroke2 :: Char  -- ^ The ancient Egyptian character to denote /two/.
singleStroke2 = '\x133fb'

-- | The triple stroke character, an ancient Egyptian character to denote /three/. This is hieroglyph Z015B.
singleStroke3 :: Char  -- ^ The ancient Egyptian character to denote /three/.
singleStroke3 = '\x133fc'

-- | The quadruple stroke character, an ancient Egyptian character to denote /four/. This is hieroglyph Z015C.
singleStroke4 :: Char  -- ^ The ancient Egyptian character to denote /four/.
singleStroke4 = '\x133fd'

-- | The quintuple stroke character, an ancient Egyptian character to denote /five/. This is hieroglyph Z015D.
singleStroke5 :: Char  -- ^ The ancient Egyptian character to denote /five/.
singleStroke5 = '\x133fe'

-- | An alternative version of the quintuple stroke character, an ancient Egyptian character
-- to denote /five/. This is hieroglyph Z015I.
singleStroke5' :: Char  -- ^ The ancient Egyptian character to denote /five/.
singleStroke5' = '\x13403'

-- | The sextuple stroke character, an ancient Egyptian character to denote /six/. This is hieroglyph Z015E.
singleStroke6 :: Char  -- ^ The ancient Egyptian character to denote /six/.
singleStroke6 = '\x133ff'

-- | The septuple stroke character, an ancient Egyptian character to denote /seven/. This is hieroglyph Z015F.
singleStroke7 :: Char  -- ^ The ancient Egyptian character to denote /seven/.
singleStroke7 = '\x13400'

-- | The octuple stroke character, an ancient Egyptian character to denote /eight/. This is hieroglyph Z015G.
singleStroke8 :: Char  -- ^ The ancient Egyptian character to denote /eight/.
singleStroke8 = '\x13401'

-- | The nonuple stroke character, an ancient Egyptian character to denote /nine/. This is hieroglyph Z015H.
singleStroke9 :: Char  -- ^ The ancient Egyptian character to denote /nine/.
singleStroke9 = '\x13402'

-- | The cattle hobble character, an ancient Egyptian character to denote /ten/. This is hieroglyph V020.
cattleHobble :: Char  -- ^ The ancient Egyptian character to denote /ten/.
cattleHobble = '\x13386'

-- | The cattle hobble character, an ancient Egyptian character to denote /ten/.
cattleHobble1 :: Char  -- ^ The ancient Egyptian character to denote /ten/. This is hieroglyph V020.
cattleHobble1 = cattleHobble

-- | The two cattle hobble character, an ancient Egyptian character to denote /twenty/.
cattleHobble2 :: Char  -- ^ The ancient Egyptian character to denote /twenty/. This is hieroglyph V020A.
cattleHobble2 = '\x13387'

-- | An alternative version of the two cattle hobble character, an ancient Egyptian character
-- to denote /twenty/. This is hieroglyph V020I.
cattleHobble2' :: Char  -- ^ The ancient Egyptian character to denote /twenty/.
cattleHobble2' = '\x1338f'

-- | The three cattle hobble character, an ancient Egyptian character to denote /thirty/. This is hieroglyph V020B.
cattleHobble3 :: Char  -- ^ The ancient Egyptian character to denote /thirty/.
cattleHobble3 = '\x13388'

-- | An alternative version of the three cattle hobble character, an ancient Egyptian character
-- to denote /thirty/. This is hieroglyph V020J.
cattleHobble3' :: Char  -- ^ The ancient Egyptian character to denote /thirty/.
cattleHobble3' = '\x13390'

-- | The four cattle hobble character, an ancient Egyptian character to denote /forty/. This is hieroglyph V020C.
cattleHobble4 :: Char  -- ^ The ancient Egyptian character to denote /forty/.
cattleHobble4 = '\x13389'

-- | An alternative version of the four cattle hobble character, an ancient Egyptian character
-- to denote /fourty/. This is hieroglyph V020K.
cattleHobble4' :: Char  -- ^ The ancient Egyptian character to denote /fourty/.
cattleHobble4' = '\x13391'

-- | The five cattle hobble character, an ancient Egyptian character to denote /fifty/.
cattleHobble5 :: Char  -- ^ The ancient Egyptian character to denote /fifty/. This is hieroglyph V020D.
cattleHobble5 = '\x1338a'

-- | An alternative version of the five cattle hobble character, an ancient Egyptian character
-- to denote /fifty/. This is hieroglyph V020L.
cattleHobble5' :: Char  -- ^ The ancient Egyptian character to denote /fifty/.
cattleHobble5' = '\x13392'

-- | The six cattle hobble character, an ancient Egyptian character to denote /sixty/. This is hieroglyph V020E.
cattleHobble6 :: Char  -- ^ The ancient Egyptian character to denote /sixty/.
cattleHobble6 = '\x1338b'

-- | The seven cattle hobble character, an ancient Egyptian character to denote /seventy/. This is hieroglyph V020F.
cattleHobble7 :: Char  -- ^ The ancient Egyptian character to denote /seventy/.
cattleHobble7 = '\x1338c'

-- | The eight cattle hobble character, an ancient Egyptian character to denote /eighty/. This is hieroglyph V020G.
cattleHobble8 :: Char  -- ^ The ancient Egyptian character to denote /eighty/.
cattleHobble8 = '\x1338d'

-- | The nine cattle hobble character, an ancient Egyptian character to denote /ninety/. This is hieroglyph V020H.
cattleHobble9 :: Char  -- ^ The ancient Egyptian character to denote /ninety/.
cattleHobble9 = '\x1338e'

-- | The coil of rope character, an ancient Egyptian character to denote /hundred/. This is hieroglyph V001.
coilOfRope :: Char  -- ^ The ancient Egyptian character to denote /hundred/.
coilOfRope = '\x13362'

-- | The coil of rope character, an ancient Egyptian character to denote /hundred/. This is hieroglyph V001.
coilOfRope1 :: Char  -- ^ The ancient Egyptian character to denote /hundred/.
coilOfRope1 = coilOfRope1

-- | The double coil of rope character, an ancient Egyptian character to denote /two hundred/. This is hieroglyph V001A.
coilOfRope2 :: Char  -- ^ The ancient Egyptian character to denote /two hundred/.
coilOfRope2 = '\x13363'

-- | The tripple coil of rope character, an ancient Egyptian character to denote /three hundred/. This is hieroglyph V001B.
coilOfRope3 :: Char  -- ^ The ancient Egyptian character to denote /three hundred/.
coilOfRope3 = '\x13364'

-- | The quadruple coil of rope character, an ancient Egyptian character to denote /four hundred/. This is hieroglyph V001C.
coilOfRope4 :: Char  -- ^ The ancient Egyptian character to denote /four hundred/.
coilOfRope4 = '\x13365'

-- | The quintuple coil of rope character, an ancient Egyptian character to denote /five hundred/. This is hieroglyph V001D.
coilOfRope5 :: Char  -- ^ The ancient Egyptian character to denote /five hundred/.
coilOfRope5 = '\x13366'

-- | An alternative version of the quintuple coil of rope character, an ancient Egyptian character
-- to denote /five hundred/. This is hieroglyph V001I.
coilOfRope5' :: Char  -- ^ The ancient Egyptian character to denote /five hundred/.
coilOfRope5' = '\x1336b'

-- | The sextuple coil of rope character, an ancient Egyptian character to denote /six hundred/. This is hieroglyph V001E.
coilOfRope6 :: Char  -- ^ The ancient Egyptian character to denote /six hundred/.
coilOfRope6 = '\x13367'

-- | The septuple coil of rope character, an ancient Egyptian character to denote /seven hundred/. This is hieroglyph V001F.
coilOfRope7 :: Char  -- ^ The ancient Egyptian character to denote /seven hundred/.
coilOfRope7 = '\x13368'

-- | The octuple coil of rope character, an ancient Egyptian character to denote /eight hundred/. This is hieroglyph V001G.
coilOfRope8 :: Char  -- ^ The ancient Egyptian character to denote /eight hundred/.
coilOfRope8 = '\x13369'

-- | The nonuple coil of rope character, an ancient Egyptian character to denote /nine hundred/. This is hieroglyph V001H.
coilOfRope9 :: Char  -- ^ The ancient Egyptian character to denote /nine hundred/.
coilOfRope9 = '\x1336a'

-- | The water lily character, an ancient Egyptian character to denote /thousand/. This is hieroglyph M012.
waterLily :: Char  -- ^ The ancient Egyptian character to denote /thousand/.
waterLily = '\x131bc'

-- | The water lily character, an ancient Egyptian character to denote /thousand/. This is hieroglyph M012.
waterLily1 :: Char  -- ^ The ancient Egyptian character to denote /thousand/.
waterLily1 = waterLily

-- | The two water lilies character, an ancient Egyptian character to denote /two thousand/. This is hieroglyph M012A.
waterLily2 :: Char  -- ^ The ancient Egyptian character to denote /two thousand/.
waterLily2 = '\x131bd'

-- | The three water lilies character, an ancient Egyptian character to denote /three thousand/. This is hieroglyph M012B.
waterLily3 :: Char  -- ^ The ancient Egyptian character to denote /three thousand/.
waterLily3 = '\x131be'

-- | The four water lilies character, an ancient Egyptian character to denote /four thousand/. This is hieroglyph M012C.
waterLily4 :: Char  -- ^ The ancient Egyptian character to denote /four thousand/.
waterLily4 = '\x131bf'

-- | The five water lilies character, an ancient Egyptian character to denote /five thousand/. This is hieroglyph M012D.
waterLily5 :: Char  -- ^ The ancient Egyptian character to denote /five thousand/.
waterLily5 = '\x131c0'

-- | The six water lilies character, an ancient Egyptian character to denote /six thousand/. This is hieroglyph M012E.
waterLily6 :: Char  -- ^ The ancient Egyptian character to denote /six thousand/.
waterLily6 = '\x131c1'

-- | The seven water lilies character, an ancient Egyptian character to denote /seven thousand/. This is hieroglyph M012F.
waterLily7 :: Char  -- ^ The ancient Egyptian character to denote /seven thousand/.
waterLily7 = '\x131c2'

-- | The eight water lilies character, an ancient Egyptian character to denote /eight thousand/. This is hieroglyph M012G.
waterLily8 :: Char  -- ^ The ancient Egyptian character to denote /eight thousand/.
waterLily8 = '\x131c3'

-- | The nine water lilies character, an ancient Egyptian character to denote /nine thousand/. This is hieroglyph M012H.
waterLily9 :: Char  -- ^ The ancient Egyptian character to denote /nine thousand/.
waterLily9 = '\x131c4'

-- | The bent finger character, an ancient Egyptian character to denote /ten thousand/. This is hieroglyph B050.
bentFinger :: Char  -- ^ The ancient Egyptian character to denote /ten thousand/.
bentFinger = '\x130ad'

-- | The bent finger character, an ancient Egyptian character to denote /ten thousand/. This is hieroglyph B050.
bentFinger1 :: Char  -- ^ The ancient Egyptian character to denote /ten thousand/.
bentFinger1 = bentFinger

-- | The two bent fingers character, an ancient Egyptian character to denote /twenty thousand/. This is hieroglyph B050A.
bentFinger2 :: Char  -- ^ The ancient Egyptian character to denote /twenty thousand/.
bentFinger2 = '\x130ae'

-- | The three bent fingers character, an ancient Egyptian character to denote /thirty thousand/. This is hieroglyph B050B.
bentFinger3 :: Char  -- ^ The ancient Egyptian character to denote /thirty thousand/.
bentFinger3 = '\x130af'

-- | The four bent fingers character, an ancient Egyptian character to denote /fourty thousand/. This is hieroglyph B050C.
bentFinger4 :: Char  -- ^ The ancient Egyptian character to denote /fourty thousand/.
bentFinger4 = '\x130b0'

-- | The five bent fingers character, an ancient Egyptian character to denote /fifty thousand/. This is hieroglyph B050D.
bentFinger5 :: Char  -- ^ The ancient Egyptian character to denote /fifty thousand/.
bentFinger5 = '\x130b1'

-- | An alternative version of the five bent fingers character, an ancient Egyptian character
-- to denote /fifty thousand/. This is hieroglyph B050I.
bentFinger5' :: Char  -- ^ The ancient Egyptian character to denote /fifty thousand/.
bentFinger5' = '\x130b6'

-- | The six bent fingers character, an ancient Egyptian character to denote /sixty thousand/. This is hieroglyph B050E.
bentFinger6 :: Char  -- ^ The ancient Egyptian character to denote /sixty thousand/.
bentFinger6 = '\x130b2'

-- | The seven bent fingers character, an ancient Egyptian character to denote /seventy thousand/. This is hieroglyph B050F.
bentFinger7 :: Char  -- ^ The ancient Egyptian character to denote /seventy thousand/.
bentFinger7 = '\x130b3'

-- | The eight bent fingers character, an ancient Egyptian character to denote /eighty thousand/. This is hieroglyph B050G.
bentFinger8 :: Char  -- ^ The ancient Egyptian character to denote /eighty thousand/.
bentFinger8 = '\x130b4'

-- | The nine bent fingers character, an ancient Egyptian character to denote /ninety thousand/. This is hieroglyph B050H.
bentFinger9 :: Char  -- ^ The ancient Egyptian character to denote /ninety thousand/.
bentFinger9 = '\x130b5'

-- | The tadpole character, an ancient Egyptian character to denote /hundred thousand/. This is hieroglyph I008.
tadpole :: Char  -- ^ The ancient Egyptian character to denote /hundred thousand/.
tadpole = '\x13190'

-- | The heh character, an ancient Egyptian character to denote /one million/, or /many/. This is hieroglyph C011.
heh :: Char  -- ^ The ancient Egyptian character to denote /one million/, or /many/.
heh = '\x13068'

-- | The nfr character, an ancient Egyptian character to denote /zero/, it also
-- means /beautiful/. This is hieroglyph F035.
nfr :: Char  -- ^ The ancient Egyptian character to denote /zero/.
nfr = '\x13124'

_normalizeCount :: Int -> Int -> Int
_normalizeCount n | n < 6 = id
                  | otherwise = (*) (10^(n-6))

_toTranslate :: Int -> Char
_toTranslate 0 = singleStroke
_toTranslate 1 = cattleHobble
_toTranslate 2 = coilOfRope
_toTranslate 3 = waterLily
_toTranslate 4 = bentFinger 
_toTranslate 5 = tadpole
_toTranslate _ = heh

_ligateDigit :: Int -> Char -> Char
_ligateDigit k = chr . (+) (k-1) . ord

_noLigate :: Int -> Int -> Text
_noLigate _ 0 = empty
_noLigate k i = T.replicate (_normalizeCount k i) (singleton (_toTranslate k))

_withLigate :: Int -> Int -> Text
_withLigate _ 0 = empty
_withLigate 5 k = T.replicate k (singleton tadpole)
_withLigate n k | n < 5 = singleton ( _ligateDigit k (_toTranslate n))
                | otherwise = T.replicate (10^(n-6) *k) (singleton heh)

-- | Obtain 
digits :: Ligate -> Int -> Int -> Text
digits = splitLigate _withLigate _noLigate

-- | The hieroglyph used as a /plus/ sign. This character is used for /addition/. This is hieroglyph D055.
plus :: Char  -- ^ The ancient Egyptian character to denote /addition/.
plus = '\x130bd'

-- | The hieroglyph used as a /minus/ sign. This character is used for
-- /subtraction/. This is hieroglyph D054.
minus :: Char  -- ^ The ancient Egyptian character to denote /subtraction/.
minus = '\x130bb'

-- | Construct an ancient Egyptian numeral with the given 'Ligate' style and
-- 'PlusStyle' for the given number.
egyptianNumber :: Integral i
  => Ligate  -- ^ The given ligation style to use.
  -> PlusStyle  -- ^ The given 'PlusStyle' to use.
  -> i  -- ^ The given number to convert to an ancient Egyptian numeral.
  -> Text  -- ^ A sequence of characters that is the ancient Egytian equivalent of the given number.
egyptianNumber l = signValueSystem 10 (digits l) (singleton nfr) plus minus

-- | Construct an acient Egyptian ligated numeral with the given 'PlusStyle' for
-- the given number.
egyptianNumber' :: Integral i
  => PlusStyle  -- ^ The given 'PlusStyle' to use.
  -> i  -- ^ The given number to convert to an ancient Egyptian numeral.
  -> Text  -- ^ A sequence of characters that is the ancient Egytian equivalent of the given number.
egyptianNumber' = egyptianNumber def

-- | Construct an ancient Egyptian ligated numeral with the default 'PlusStyle.
egyptianNumber'' :: Integral i
  => i  -- ^ The given number to convert to an ancient Egyptian numeral.
  -> Text  -- ^ A sequence of characters that is the ancient Egytian equivalent of the given number.
egyptianNumber'' = egyptianNumber' def
