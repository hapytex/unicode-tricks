{-# LANGUAGE Safe #-}

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
  , tadpole1,      tadpole2,      tadpole3,      tadpole4,      tadpole5,      tadpole6,      tadpole7,      tadpole8,      tadpole9
  ) where

import Data.Char(chr, ord)
import Data.Char.Core(Ligate, PlusStyle, positionalNumberSystem', splitLigate)
import Data.Default(def)
import qualified Data.Text as T
import Data.Text(Text, singleton, empty)

-- | The single stroke character, an ancient Egyptian character to denote /one/.
singleStroke :: Char -- ^ The ancient Egyptian character to denote /one/.
singleStroke = '\x133fa'

-- | The single stroke character, an ancient Egyptian character to denote /one/.
singleStroke1 :: Char -- ^ The ancient Egyptian character to denote /one/.
singleStroke1 = singleStroke

-- | The two stroke character, an ancient Egyptian character to denote /two/.
singleStroke2 :: Char -- ^ The ancient Egyptian character to denote /two/.
singleStroke2 = '\x133fb'

-- | The three stroke character, an ancient Egyptian character to denote /three/.
singleStroke3 :: Char -- ^ The ancient Egyptian character to denote /three/.
singleStroke3 = '\x133fc'

-- | The four stroke character, an ancient Egyptian character to denote /four/.
singleStroke4 :: Char -- ^ The ancient Egyptian character to denote /four/.
singleStroke4 = '\x133fd'

-- | The five stroke character, an ancient Egyptian character to denote /five/.
singleStroke5 :: Char -- ^ The ancient Egyptian character to denote /five/.
singleStroke5 = '\x133fe'

-- | An alternative version of the five stroke character, an ancient Egyptian character
-- to denote /five/.
singleStroke5' :: Char -- ^ The ancient Egyptian character to denote /five/.
singleStroke5' = '\x13403'

-- | The six stroke character, an ancient Egyptian character to denote /six/.
singleStroke6 :: Char -- ^ The ancient Egyptian character to denote /six/.
singleStroke6 = '\x133ff'

-- | The seven stroke character, an ancient Egyptian character to denote /seven/.
singleStroke7 :: Char -- ^ The ancient Egyptian character to denote /seven/.
singleStroke7 = '\x13400'

-- | The eight stroke character, an ancient Egyptian character to denote /eight/.
singleStroke8 :: Char -- ^ The ancient Egyptian character to denote /eight/.
singleStroke8 = '\x13401'

-- | The nine stroke character, an ancient Egyptian character to denote /nine/.
singleStroke9 :: Char -- ^ The ancient Egyptian character to denote /nine/.
singleStroke9 = '\x13402'

-- | The cattle hobble character, an ancient Egyptian character to denote /ten/.
cattleHobble :: Char -- ^ The ancient Egyptian character to denote /ten/.
cattleHobble = '\x13386'

-- | The cattle hobble character, an ancient Egyptian character to denote /ten/.
cattleHobble1 :: Char -- ^ The ancient Egyptian character to denote /ten/.
cattleHobble1 = cattleHobble

-- | The two cattle hobble character, an ancient Egyptian character to denote /twenty/.
cattleHobble2 :: Char -- ^ The ancient Egyptian character to denote /twenty/.
cattleHobble2 = '\x13387'

-- | An alternative version of the two cattle hobble character, an ancient Egyptian character
-- to denote /twenty/.
cattleHobble2' :: Char -- ^ The ancient Egyptian character to denote /twenty/.
cattleHobble2' = '\x1338f'

-- | The three cattle hobble character, an ancient Egyptian character to denote /thirty/.
cattleHobble3 :: Char -- ^ The ancient Egyptian character to denote /thirty/.
cattleHobble3 = '\x13388'

-- | An alternative version of the three cattle hobble character, an ancient Egyptian character
-- to denote /thirty/.
cattleHobble3' :: Char -- ^ The ancient Egyptian character to denote /thirty/.
cattleHobble3' = '\x13390'

-- | The four cattle hobble character, an ancient Egyptian character to denote /forty/.
cattleHobble4 :: Char -- ^ The ancient Egyptian character to denote /forty/.
cattleHobble4 = '\x13389'

-- | An alternative version of the four cattle hobble character, an ancient Egyptian character
-- to denote /fourty/.
cattleHobble4' :: Char -- ^ The ancient Egyptian character to denote /fourty/.
cattleHobble4' = '\x13391'

-- | The five cattle hobble character, an ancient Egyptian character to denote /fifty/.
cattleHobble5 :: Char -- ^ The ancient Egyptian character to denote /fifty/.
cattleHobble5 = '\x1338a'

-- | An alternative version of the five cattle hobble character, an ancient Egyptian character
-- to denote /fifty/.
cattleHobble5' :: Char -- ^ The ancient Egyptian character to denote /fifty/.
cattleHobble5' = '\x13392'

-- | The six cattle hobble character, an ancient Egyptian character to denote /sixty/.
cattleHobble6 :: Char -- ^ The ancient Egyptian character to denote /sixty/.
cattleHobble6 = '\x1338b'

-- | The seven cattle hobble character, an ancient Egyptian character to denote /seventy/.
cattleHobble7 :: Char -- ^ The ancient Egyptian character to denote /seventy/.
cattleHobble7 = '\x1338c'

-- | The eight cattle hobble character, an ancient Egyptian character to denote /eighty/.
cattleHobble8 :: Char -- ^ The ancient Egyptian character to denote /eighty/.
cattleHobble8 = '\x1338d'

-- | The nine cattle hobble character, an ancient Egyptian character to denote /ninety/.
cattleHobble9 :: Char -- ^ The ancient Egyptian character to denote /ninety/.
cattleHobble9 = '\x1338e'

-- | The coil of rope character, an ancient Egyptian character to denote /hundred/.
coilOfRope :: Char -- ^ The ancient Egyptian character to denote /hundred/.
coilOfRope = '\x13362'

-- | The coil of rope character, an ancient Egyptian character to denote /hundred/.
coilOfRope1 :: Char -- ^ The ancient Egyptian character to denote /hundred/.
coilOfRope1 = coilOfRope1

-- | The two coil of rope character, an ancient Egyptian character to denote /two hundred/.
coilOfRope2 :: Char -- ^ The ancient Egyptian character to denote /two hundred/.
coilOfRope2 = '\x13363'

-- | The three coil of rope character, an ancient Egyptian character to denote /three hundred/.
coilOfRope3 :: Char -- ^ The ancient Egyptian character to denote /three hundred/.
coilOfRope3 = '\x13364'

-- | The four coil of rope character, an ancient Egyptian character to denote /four hundred/.
coilOfRope4 :: Char -- ^ The ancient Egyptian character to denote /four hundred/.
coilOfRope4 = '\x13365'

-- | The five coil of rope character, an ancient Egyptian character to denote /five hundred/.
coilOfRope5 :: Char -- ^ The ancient Egyptian character to denote /five hundred/.
coilOfRope5 = '\x13366'

-- | An alternative version of the five coil of rope character, an ancient Egyptian character
-- to denote /five hundred/.
coilOfRope5' :: Char -- ^ The ancient Egyptian character to denote /five hundred/.
coilOfRope5' = '\x1336b'

-- | The six coil of rope character, an ancient Egyptian character to denote /six hundred/.
coilOfRope6 :: Char -- ^ The ancient Egyptian character to denote /six hundred/.
coilOfRope6 = '\x13367'

-- | The seven coil of rope character, an ancient Egyptian character to denote /seven hundred/.
coilOfRope7 :: Char -- ^ The ancient Egyptian character to denote /seven hundred/.
coilOfRope7 = '\x13368'

-- | The eight coil of rope character, an ancient Egyptian character to denote /eight hundred/.
coilOfRope8 :: Char -- ^ The ancient Egyptian character to denote /eight hundred/.
coilOfRope8 = '\x13369'

-- | The nine coil of rope character, an ancient Egyptian character to denote /nine hundred/.
coilOfRope9 :: Char -- ^ The ancient Egyptian character to denote /nine hundred/.
coilOfRope9 = '\x1336a'

-- | The water lily character, an ancient Egyptian character to denote /thousand/.
waterLily :: Char -- ^ The ancient Egyptian character to denote /thousand/.
waterLily = '\x131bc'

-- | The water lily character, an ancient Egyptian character to denote /thousand/.
waterLily1 :: Char -- ^ The ancient Egyptian character to denote /thousand/.
waterLily1 = waterLily

-- | The two water lily character, an ancient Egyptian character to denote /two thousand/.
waterLily2 :: Char -- ^ The ancient Egyptian character to denote /two thousand/.
waterLily2 = '\x131bd'

-- | The three water lily character, an ancient Egyptian character to denote /three thousand/.
waterLily3 :: Char -- ^ The ancient Egyptian character to denote /three thousand/.
waterLily3 = '\x131be'

-- | The four water lily character, an ancient Egyptian character to denote /four thousand/.
waterLily4 :: Char -- ^ The ancient Egyptian character to denote /four thousand/.
waterLily4 = '\x131bf'

-- | The five water lily character, an ancient Egyptian character to denote /five thousand/.
waterLily5 :: Char -- ^ The ancient Egyptian character to denote /five thousand/.
waterLily5 = '\x131c0'

-- | The six water lily character, an ancient Egyptian character to denote /six thousand/.
waterLily6 :: Char -- ^ The ancient Egyptian character to denote /six thousand/.
waterLily6 = '\x131c1'

-- | The seven water lily character, an ancient Egyptian character to denote /seven thousand/.
waterLily7 :: Char -- ^ The ancient Egyptian character to denote /seven thousand/.
waterLily7 = '\x131c2'

-- | The eight water lily character, an ancient Egyptian character to denote /eight thousand/.
waterLily8 :: Char -- ^ The ancient Egyptian character to denote /eight thousand/.
waterLily8 = '\x131c3'

-- | The nine water lily character, an ancient Egyptian character to denote /nine thousand/.
waterLily9 :: Char -- ^ The ancient Egyptian character to denote /nine thousand/.
waterLily9 = '\x131c4'

-- | The bent finger character, an ancient Egyptian character to denote /ten thousand/.
bentFinger :: Char -- ^ The ancient Egyptian character to denote /ten thousand/.
bentFinger = '\x130ad'

-- | The bent finger character, an ancient Egyptian character to denote /ten thousand/.
bentFinger1 :: Char -- ^ The ancient Egyptian character to denote /ten thousand/.
bentFinger1 = bentFinger

-- | The two bent finger character, an ancient Egyptian character to denote /twenty thousand/.
bentFinger2 :: Char -- ^ The ancient Egyptian character to denote /twenty thousand/.
bentFinger2 = '\x130ae'

-- | The three bent finger character, an ancient Egyptian character to denote /thirty thousand/.
bentFinger3 :: Char -- ^ The ancient Egyptian character to denote /thirty thousand/.
bentFinger3 = '\x130af'

-- | The four bent finger character, an ancient Egyptian character to denote /fourty thousand/.
bentFinger4 :: Char -- ^ The ancient Egyptian character to denote /fourty thousand/.
bentFinger4 = '\x130b0'

-- | The five bent finger character, an ancient Egyptian character to denote /fifty thousand/.
bentFinger5 :: Char -- ^ The ancient Egyptian character to denote /fifty thousand/.
bentFinger5 = '\x130b1'

-- | An alternative version of the five bent finger character, an ancient Egyptian character
-- to denote /fifty thousand/.
bentFinger5' :: Char -- ^ The ancient Egyptian character to denote /fifty thousand/.
bentFinger5' = '\x130b6'


-- | The six bent finger character, an ancient Egyptian character to denote /sixty thousand/.
bentFinger6 :: Char -- ^ The ancient Egyptian character to denote /sixty thousand/.
bentFinger6 = '\x130b2'

-- | The seven bent finger character, an ancient Egyptian character to denote /seventy thousand/.
bentFinger7 :: Char -- ^ The ancient Egyptian character to denote /seventy thousand/.
bentFinger7 = '\x130b3'

-- | The eight bent finger character, an ancient Egyptian character to denote /eighty thousand/.
bentFinger8 :: Char -- ^ The ancient Egyptian character to denote /eighty thousand/.
bentFinger8 = '\x130b4'

-- | The nine bent finger character, an ancient Egyptian character to denote /ninety thousand/.
bentFinger9 :: Char -- ^ The ancient Egyptian character to denote /ninety thousand/.
bentFinger9 = '\x130b5'

-- | The tadpole character, an ancient Egyptian character to denote /hundred thousand/.
tadpole :: Char -- ^ The ancient Egyptian character to denote /hundred thousand/.
tadpole = '\x13190'

-- | The tadpole character, an ancient Egyptian character to denote /hundred thousand/.
tadpole1 :: Char -- ^ The ancient Egyptian character to denote /hundred thousand/.
tadpole1 = tadpole

-- | The two tadpole character, an ancient Egyptian character to denote /two hundred thousand/.
tadpole2 :: Char -- ^ The ancient Egyptian character to denote /two hundred thousand/.
tadpole2 = '\x13191'

-- | The three tadpole character, an ancient Egyptian character to denote /three hundred thousand/.
tadpole3 :: Char -- ^ The ancient Egyptian character to denote /three hundred thousand/.
tadpole3 = '\x13192'

-- | The four tadpole character, an ancient Egyptian character to denote /four hundred thousand/.
tadpole4 :: Char -- ^ The ancient Egyptian character to denote /four hundred thousand/.
tadpole4 = '\x13193'

-- | The five tadpole character, an ancient Egyptian character to denote /five hundred thousand/.
tadpole5 :: Char -- ^ The ancient Egyptian character to denote /five hundred thousand/.
tadpole5 = '\x13194'

-- | The six tadpole character, an ancient Egyptian character to denote /six hundred thousand/.
tadpole6 :: Char -- ^ The ancient Egyptian character to denote /six hundred thousand/.
tadpole6 = '\x13195'

-- | The seven tadpole character, an ancient Egyptian character to denote /seven hundred thousand/.
tadpole7 :: Char -- ^ The ancient Egyptian character to denote /seven hundred thousand/.
tadpole7 = '\x13196'

-- | The eight tadpole character, an ancient Egyptian character to denote /eight hundred thousand/.
tadpole8 :: Char -- ^ The ancient Egyptian character to denote /eight hundred thousand/.
tadpole8 = '\x13197'

-- | The nine tadpole character, an ancient Egyptian character to denote /nine hundred thousand/.
tadpole9 :: Char -- ^ The ancient Egyptian character to denote /nine hundred thousand/.
tadpole9 = '\x13198'

-- | The heh character, an ancient Egyptian character to denote /one million/, or /many/.
heh :: Char -- ^ The ancient Egyptian character to denote /one million/, or /many/.
heh = '\x13068'

-- | The nfr character, an ancient Egyptian character to denote /zero/.
nfr :: Char -- ^ The ancient Egyptian character to denote /zero/.
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
_withLigate n k | n < 6 = singleton ( _ligateDigit k (_toTranslate n))
                | otherwise = T.replicate (10^(n-6) *k) (singleton heh)

digits :: Ligate -> Int -> Int -> Text
digits = splitLigate _withLigate _noLigate

-- _wrapSingleton :: Ligate -> (Int -> Int -> Int) -> Int -> Int -> Text
-- _wrapSingleton = splitLigate f g

-- _wrapSingleton :: Int -> Int -> Text
-- _wrapSingleton o = singleton . chr . (o +)

-- ligateDigits :: Int -> Int -> Text
-- ligateDigits 0 = _wrapSingleton 0x133f9
-- ligateDigits 1 = _wrapSingleton 0x13385
-- ligateDigits 2 = _wrapSingleton 0x13361
-- ligateDigits 3 = _wrapSingleton 0x131bb
-- ligateDigits 4 = _wrapSingleton 0x130ac
-- ligateDigits 5 = (`T.replicate` singleton tadpole)
-- ligateDigits n = (`T.replicate` singleton heh) . (10^(n-6) *)

plus :: Char
plus = '\x130bd'

minus :: Char
minus = '\x130bb'

egyptianNumber :: Integral i => Ligate -> PlusStyle-> i -> Text
egyptianNumber l = positionalNumberSystem' 10 (digits l) (singleton nfr) plus minus

egyptianNumber' :: Integral i => PlusStyle -> i -> Text
egyptianNumber' = egyptianNumber def

egyptianNumber'' :: Integral i => i -> Text
egyptianNumber'' = egyptianNumber' def
