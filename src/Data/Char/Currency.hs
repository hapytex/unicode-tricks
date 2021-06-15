{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, Safe #-}

{-|
Module      : Data.Char.Currency
Description : The module exposes a type that defines the different currencies for which there is a Unicode equivalent.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode has multiple code blocks where it defines currencies. This module aims to expose a data structure that makes
it more convenient to work with currency characters.
-}

module Data.Char.Currency (
    -- * Defining currencies
    Currency(
                     Dollar,                Cent,               Pound,            Currency,                 Yen,         ArmenianDram,             Afghani
      ,           NkoDorome,            NkoTaman,    BengaliRupeeMark,        BengaliRupee,    BengaliGandaMark,        GujaratiRupee,          TamilRupee
      ,      ThaiSymbolBaht,     KhmerSymbolRiel,        EuroCurrency,               Colon,            Cruzeiro,          FrenchFranc,                Lira
      ,                Mill,               Naira,              Peseta,               Rupee,                 Won,            NewSheqel,                Dong
      ,                Euro,                 Kip,              Tugrik,             Drachma,         GermanPenny,                 Peso,             Guarani
      ,             Austral,             Hryvnia,                Cedi,       LivreTournois,            Spesmilo,                Tenge,         IndianRupee
      ,         TurkishLira,          NordicMark,               Manat,               Ruble,                Lari,              Bitcoin, NorthIndicRupeeMark
      ,                Rial,         SmallDollar,     FullwidthDollar,       FullwidthCent,      FullwidthPound,         FullwidthYen,        FullwidthWon
      ,          TamilKaacu,          TamilPanam,            TamilPon,       TamilVaraakan,          WanchoNgun,  IndicSiyaqRupeeMark
    )
    -- * Currencies as 'Char' objects
  ,              dollar,                cent,               pound,            currency,                 yen,        armenianDram,             afghani,           nkoDorome
  ,            nkoTaman,    bengaliRupeeMark,        bengaliRupee,    bengaliGandaMark,       gujaratiRupee,          tamilRupee,      thaiSymbolBaht,     khmerSymbolRiel
  ,        euroCurrency,               colon,            cruzeiro,         frenchFranc,                lira,                mill,               naira,              peseta
  ,               rupee,                 won,           newSheqel,                dong,                euro,                 kip,              tugrik,             drachma
  ,         germanPenny,                peso,             guarani,             austral,             hryvnia,                cedi,       livreTournois,            spesmilo
  ,               tenge,         indianRupee,         turkishLira,          nordicMark,               manat,               ruble,                lari,             bitcoin
  , northIndicRupeeMark,                rial,         smallDollar,     fullwidthDollar,       fullwidthCent,      fullwidthPound,        fullwidthYen,        fullwidthWon
  ,          tamilKaacu,          tamilPanam,            tamilPon,       tamilVaraakan,          wanchoNgun, indicSiyaqRupeeMark
    -- * Check if a character is a currency
  , isCurrency
  ) where

import Control.DeepSeq(NFData)

import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar), UnicodeText)
import Data.Data(Data)
import Data.Hashable(Hashable)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

-- | A datatype to present the currencies that have a unicode character.
data Currency
  = Dollar  -- ^ A currency that is rendered as @$@.
  | Cent  -- ^ A currency that is rendered as @¢@.
  | Pound  -- ^ A currency that is rendered as @£@.
  | Currency  -- ^ A currency that is rendered as @¤@.
  | Yen  -- ^ A currency that is rendered as @¥@.
  | ArmenianDram  -- ^ A currency that is rendered as @֏@.
  | Afghani  -- ^ A currency that is rendered as @؋@.
  | NkoDorome  -- ^ A currency that is rendered as @߾@.
  | NkoTaman  -- ^ A currency that is rendered as @߿@.
  | BengaliRupeeMark  -- ^ A currency that is rendered as @৲@.
  | BengaliRupee  -- ^ A currency that is rendered as @৳@.
  | BengaliGandaMark  -- ^ A currency that is rendered as @৻@.
  | GujaratiRupee  -- ^ A currency that is rendered as @૱@.
  | TamilRupee  -- ^ A currency that is rendered as @௹@.
  | ThaiSymbolBaht  -- ^ A currency that is rendered as @฿@.
  | KhmerSymbolRiel  -- ^ A currency that is rendered as @៛@.
  | EuroCurrency  -- ^ A currency that is rendered as @₠@.
  | Colon  -- ^ A currency that is rendered as @₡@.
  | Cruzeiro  -- ^ A currency that is rendered as @₢@.
  | FrenchFranc  -- ^ A currency that is rendered as @₣@.
  | Lira  -- ^ A currency that is rendered as @₤@.
  | Mill  -- ^ A currency that is rendered as @₥@.
  | Naira  -- ^ A currency that is rendered as @₦@.
  | Peseta  -- ^ A currency that is rendered as @₧@.
  | Rupee  -- ^ A currency that is rendered as @₨@.
  | Won  -- ^ A currency that is rendered as @₩@.
  | NewSheqel  -- ^ A currency that is rendered as @₪@.
  | Dong  -- ^ A currency that is rendered as @₫@.
  | Euro  -- ^ A currency that is rendered as @€@.
  | Kip  -- ^ A currency that is rendered as @₭@.
  | Tugrik  -- ^ A currency that is rendered as @₮@.
  | Drachma  -- ^ A currency that is rendered as @₯@.
  | GermanPenny  -- ^ A currency that is rendered as @₰@.
  | Peso  -- ^ A currency that is rendered as @₱@.
  | Guarani  -- ^ A currency that is rendered as @₲@.
  | Austral  -- ^ A currency that is rendered as @₳@.
  | Hryvnia  -- ^ A currency that is rendered as @₴@.
  | Cedi  -- ^ A currency that is rendered as @₵@.
  | LivreTournois  -- ^ A currency that is rendered as @₶@.
  | Spesmilo  -- ^ A currency that is rendered as @₷@.
  | Tenge  -- ^ A currency that is rendered as @₸@.
  | IndianRupee  -- ^ A currency that is rendered as @₹@.
  | TurkishLira  -- ^ A currency that is rendered as @₺@.
  | NordicMark  -- ^ A currency that is rendered as @₻@.
  | Manat  -- ^ A currency that is rendered as @₼@.
  | Ruble  -- ^ A currency that is rendered as @₽@.
  | Lari  -- ^ A currency that is rendered as @₾@.
  | Bitcoin  -- ^ A currency that is rendered as @₿@.
  | NorthIndicRupeeMark  -- ^ A currency that is rendered as @꠸@.
  | Rial  -- ^ A currency that is rendered as @﷼@.
  | SmallDollar  -- ^ A currency that is rendered as @﹩@.
  | FullwidthDollar  -- ^ A currency that is rendered as @＄@.
  | FullwidthCent  -- ^ A currency that is rendered as @￠@.
  | FullwidthPound  -- ^ A currency that is rendered as @￡@.
  | FullwidthYen  -- ^ A currency that is rendered as @￥@.
  | FullwidthWon  -- ^ A currency that is rendered as @￦@.
  | TamilKaacu  -- ^ A currency that is rendered as @𑿝@.
  | TamilPanam  -- ^ A currency that is rendered as @𑿞@.
  | TamilPon  -- ^ A currency that is rendered as @𑿟@.
  | TamilVaraakan  -- ^ A currency that is rendered as @𑿠@.
  | WanchoNgun  -- ^ A currency that is rendered as @𞋿@.
  | IndicSiyaqRupeeMark  -- ^ A currency that is rendered as @𞲰@.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary Currency where
  arbitrary = arbitraryBoundedEnum

instance Hashable Currency

instance NFData Currency

instance UnicodeCharacter Currency where
  toUnicodeChar Dollar = dollar
  toUnicodeChar Cent = cent
  toUnicodeChar Pound = pound
  toUnicodeChar Currency = currency
  toUnicodeChar Yen = yen
  toUnicodeChar ArmenianDram = armenianDram
  toUnicodeChar Afghani = afghani
  toUnicodeChar NkoDorome = nkoDorome
  toUnicodeChar NkoTaman = nkoTaman
  toUnicodeChar BengaliRupeeMark = bengaliRupeeMark
  toUnicodeChar BengaliRupee = bengaliRupee
  toUnicodeChar BengaliGandaMark = bengaliGandaMark
  toUnicodeChar GujaratiRupee = gujaratiRupee
  toUnicodeChar TamilRupee = tamilRupee
  toUnicodeChar ThaiSymbolBaht = thaiSymbolBaht
  toUnicodeChar KhmerSymbolRiel = khmerSymbolRiel
  toUnicodeChar EuroCurrency = euroCurrency
  toUnicodeChar Colon = colon
  toUnicodeChar Cruzeiro = cruzeiro
  toUnicodeChar FrenchFranc = frenchFranc
  toUnicodeChar Lira = lira
  toUnicodeChar Mill = mill
  toUnicodeChar Naira = naira
  toUnicodeChar Peseta = peseta
  toUnicodeChar Rupee = rupee
  toUnicodeChar Won = won
  toUnicodeChar NewSheqel = newSheqel
  toUnicodeChar Dong = dong
  toUnicodeChar Euro = euro
  toUnicodeChar Kip = kip
  toUnicodeChar Tugrik = tugrik
  toUnicodeChar Drachma = drachma
  toUnicodeChar GermanPenny = germanPenny
  toUnicodeChar Peso = peso
  toUnicodeChar Guarani = guarani
  toUnicodeChar Austral = austral
  toUnicodeChar Hryvnia = hryvnia
  toUnicodeChar Cedi = cedi
  toUnicodeChar LivreTournois = livreTournois
  toUnicodeChar Spesmilo = spesmilo
  toUnicodeChar Tenge = tenge
  toUnicodeChar IndianRupee = indianRupee
  toUnicodeChar TurkishLira = turkishLira
  toUnicodeChar NordicMark = nordicMark
  toUnicodeChar Manat = manat
  toUnicodeChar Ruble = ruble
  toUnicodeChar Lari = lari
  toUnicodeChar Bitcoin = bitcoin
  toUnicodeChar NorthIndicRupeeMark = northIndicRupeeMark
  toUnicodeChar Rial = rial
  toUnicodeChar SmallDollar = smallDollar
  toUnicodeChar FullwidthDollar = fullwidthDollar
  toUnicodeChar FullwidthCent = fullwidthCent
  toUnicodeChar FullwidthPound = fullwidthPound
  toUnicodeChar FullwidthYen = fullwidthYen
  toUnicodeChar FullwidthWon = fullwidthWon
  toUnicodeChar TamilKaacu = tamilKaacu
  toUnicodeChar TamilPanam = tamilPanam
  toUnicodeChar TamilPon = tamilPon
  toUnicodeChar TamilVaraakan = tamilVaraakan
  toUnicodeChar WanchoNgun = wanchoNgun
  toUnicodeChar IndicSiyaqRupeeMark = indicSiyaqRupeeMark
  fromUnicodeChar '\x24' = Just Dollar
  fromUnicodeChar '\xa2' = Just Cent
  fromUnicodeChar '\xa3' = Just Pound
  fromUnicodeChar '\xa4' = Just Currency
  fromUnicodeChar '\xa5' = Just Yen
  fromUnicodeChar '\x58f' = Just ArmenianDram
  fromUnicodeChar '\x60b' = Just Afghani
  fromUnicodeChar '\x7fe' = Just NkoDorome
  fromUnicodeChar '\x7ff' = Just NkoTaman
  fromUnicodeChar '\x9f2' = Just BengaliRupeeMark
  fromUnicodeChar '\x9f3' = Just BengaliRupee
  fromUnicodeChar '\x9fb' = Just BengaliGandaMark
  fromUnicodeChar '\xaf1' = Just GujaratiRupee
  fromUnicodeChar '\xbf9' = Just TamilRupee
  fromUnicodeChar '\xe3f' = Just ThaiSymbolBaht
  fromUnicodeChar '\x17db' = Just KhmerSymbolRiel
  fromUnicodeChar '\x20a0' = Just EuroCurrency
  fromUnicodeChar '\x20a1' = Just Colon
  fromUnicodeChar '\x20a2' = Just Cruzeiro
  fromUnicodeChar '\x20a3' = Just FrenchFranc
  fromUnicodeChar '\x20a4' = Just Lira
  fromUnicodeChar '\x20a5' = Just Mill
  fromUnicodeChar '\x20a6' = Just Naira
  fromUnicodeChar '\x20a7' = Just Peseta
  fromUnicodeChar '\x20a8' = Just Rupee
  fromUnicodeChar '\x20a9' = Just Won
  fromUnicodeChar '\x20aa' = Just NewSheqel
  fromUnicodeChar '\x20ab' = Just Dong
  fromUnicodeChar '\x20ac' = Just Euro
  fromUnicodeChar '\x20ad' = Just Kip
  fromUnicodeChar '\x20ae' = Just Tugrik
  fromUnicodeChar '\x20af' = Just Drachma
  fromUnicodeChar '\x20b0' = Just GermanPenny
  fromUnicodeChar '\x20b1' = Just Peso
  fromUnicodeChar '\x20b2' = Just Guarani
  fromUnicodeChar '\x20b3' = Just Austral
  fromUnicodeChar '\x20b4' = Just Hryvnia
  fromUnicodeChar '\x20b5' = Just Cedi
  fromUnicodeChar '\x20b6' = Just LivreTournois
  fromUnicodeChar '\x20b7' = Just Spesmilo
  fromUnicodeChar '\x20b8' = Just Tenge
  fromUnicodeChar '\x20b9' = Just IndianRupee
  fromUnicodeChar '\x20ba' = Just TurkishLira
  fromUnicodeChar '\x20bb' = Just NordicMark
  fromUnicodeChar '\x20bc' = Just Manat
  fromUnicodeChar '\x20bd' = Just Ruble
  fromUnicodeChar '\x20be' = Just Lari
  fromUnicodeChar '\x20bf' = Just Bitcoin
  fromUnicodeChar '\xa838' = Just NorthIndicRupeeMark
  fromUnicodeChar '\xfdfc' = Just Rial
  fromUnicodeChar '\xfe69' = Just SmallDollar
  fromUnicodeChar '\xff04' = Just FullwidthDollar
  fromUnicodeChar '\xffe0' = Just FullwidthCent
  fromUnicodeChar '\xffe1' = Just FullwidthPound
  fromUnicodeChar '\xffe5' = Just FullwidthYen
  fromUnicodeChar '\xffe6' = Just FullwidthWon
  fromUnicodeChar '\x11fdd' = Just TamilKaacu
  fromUnicodeChar '\x11fde' = Just TamilPanam
  fromUnicodeChar '\x11fdf' = Just TamilPon
  fromUnicodeChar '\x11fe0' = Just TamilVaraakan
  fromUnicodeChar '\x1e2ff' = Just WanchoNgun
  fromUnicodeChar '\x1ecb0' = Just IndicSiyaqRupeeMark
  fromUnicodeChar _ = Nothing

instance UnicodeText Currency

-- | Check if the given 'Char'acter is a currency character.
isCurrency
  :: Char  -- ^ The given character to test.
  -> Bool  -- ^ 'True' if the given character is a currency character; 'False' otherwise.
isCurrency x
  | '\x20a0' <= x && x <= '\x20bf' = True
  | '\xa2' <= x && x <= '\xa5' = True
  | '\x11fdd' <= x && x <= '\x11fe0' = True
  | '\x7fe' <= x && x <= '\x7ff' = True
  | '\x9f2' <= x && x <= '\x9f3' = True
  | '\xffe0' <= x && x <= '\xffe1' = True
  | '\xffe5' <= x && x <= '\xffe6' = True
isCurrency '\x24' = True
isCurrency '\x58f' = True
isCurrency '\x60b' = True
isCurrency '\x9fb' = True
isCurrency '\xaf1' = True
isCurrency '\xbf9' = True
isCurrency '\xe3f' = True
isCurrency '\x17db' = True
isCurrency '\xa838' = True
isCurrency '\xfdfc' = True
isCurrency '\xfe69' = True
isCurrency '\xff04' = True
isCurrency '\x1e2ff' = True
isCurrency _ = False

-- | The character used to render a /dollar sign/ presented as @$@.
dollar
  :: Char  -- ^ A character that corresponds with the /dollar sign/.
dollar = '\x24'

-- | The character used to render a /cent sign/ presented as @¢@.
cent
  :: Char  -- ^ A character that corresponds with the /cent sign/.
cent = '\xa2'

-- | The character used to render a /pound sign/ presented as @£@.
pound
  :: Char  -- ^ A character that corresponds with the /pound sign/.
pound = '\xa3'

-- | The character used to render a /currency sign/ presented as @¤@.
currency  :: Char  -- ^ A character that corresponds with the /currency sign/.
currency = '\xa4'

-- | The character used to render a /yen sign/ presented as @¥@.
yen
  :: Char  -- ^ A character that corresponds with the /yen sign/.
yen = '\xa5'

-- | The character used to render a /armenian dram sign/ presented as @֏@.
armenianDram
  :: Char  -- ^ A character that corresponds with the /armenian dram sign/.
armenianDram = '\x58f'

-- | The character used to render a /afghani sign/ presented as @؋@.
afghani
  :: Char  -- ^ A character that corresponds with the /afghani sign/.
afghani = '\x60b'

-- | The character used to render a /nko dorome sign/ presented as @߾@.
nkoDorome
  :: Char  -- ^ A character that corresponds with the /nko dorome sign/.
nkoDorome = '\x7fe'

-- | The character used to render a /nko taman sign/ presented as @߿@.
nkoTaman
  :: Char  -- ^ A character that corresponds with the /nko taman sign/.
nkoTaman = '\x7ff'

-- | The character used to render a /bengali rupee mark/ presented as @৲@.
bengaliRupeeMark
  :: Char  -- ^ A character that corresponds with the /bengali rupee mark/.
bengaliRupeeMark = '\x9f2'

-- | The character used to render a /bengali rupee sign/ presented as @৳@.
bengaliRupee
  :: Char  -- ^ A character that corresponds with the /bengali rupee sign/.
bengaliRupee = '\x9f3'

-- | The character used to render a /bengali ganda mark/ presented as @৻@.
bengaliGandaMark
  :: Char  -- ^ A character that corresponds with the /bengali ganda mark/.
bengaliGandaMark = '\x9fb'

-- | The character used to render a /gujarati rupee sign/ presented as @૱@.
gujaratiRupee
  :: Char  -- ^ A character that corresponds with the /gujarati rupee sign/.
gujaratiRupee = '\xaf1'

-- | The character used to render a /tamil rupee sign/ presented as @௹@.
tamilRupee
  :: Char  -- ^ A character that corresponds with the /tamil rupee sign/.
tamilRupee = '\xbf9'

-- | The character used to render a /thai currency symbol baht/ presented as @฿@.
thaiSymbolBaht
  :: Char  -- ^ A character that corresponds with the /thai currency symbol baht/.
thaiSymbolBaht = '\xe3f'

-- | The character used to render a /khmer currency symbol riel/ presented as @៛@.
khmerSymbolRiel
  :: Char  -- ^ A character that corresponds with the /khmer currency symbol riel/.
khmerSymbolRiel = '\x17db'

-- | The character used to render a /euro-currency sign/ presented as @₠@.
euroCurrency
  :: Char  -- ^ A character that corresponds with the /euro-currency sign/.
euroCurrency = '\x20a0'

-- | The character used to render a /colon sign/ presented as @₡@.
colon
  :: Char  -- ^ A character that corresponds with the /colon sign/.
colon = '\x20a1'

-- | The character used to render a /cruzeiro sign/ presented as @₢@.
cruzeiro
  :: Char  -- ^ A character that corresponds with the /cruzeiro sign/.
cruzeiro = '\x20a2'

-- | The character used to render a /french franc sign/ presented as @₣@.
frenchFranc
  :: Char  -- ^ A character that corresponds with the /french franc sign/.
frenchFranc = '\x20a3'

-- | The character used to render a /lira sign/ presented as @₤@.
lira
  :: Char  -- ^ A character that corresponds with the /lira sign/.
lira = '\x20a4'

-- | The character used to render a /mill sign/ presented as @₥@.
mill
  :: Char  -- ^ A character that corresponds with the /mill sign/.
mill = '\x20a5'

-- | The character used to render a /naira sign/ presented as @₦@.
naira
  :: Char  -- ^ A character that corresponds with the /naira sign/.
naira = '\x20a6'

-- | The character used to render a /peseta sign/ presented as @₧@.
peseta
  :: Char  -- ^ A character that corresponds with the /peseta sign/.
peseta = '\x20a7'

-- | The character used to render a /rupee sign/ presented as @₨@.
rupee
  :: Char  -- ^ A character that corresponds with the /rupee sign/.
rupee = '\x20a8'

-- | The character used to render a /won sign/ presented as @₩@.
won
  :: Char  -- ^ A character that corresponds with the /won sign/.
won = '\x20a9'

-- | The character used to render a /new sheqel sign/ presented as @₪@.
newSheqel
  :: Char  -- ^ A character that corresponds with the /new sheqel sign/.
newSheqel = '\x20aa'

-- | The character used to render a /dong sign/ presented as @₫@.
dong
  :: Char  -- ^ A character that corresponds with the /dong sign/.
dong = '\x20ab'

-- | The character used to render a /euro sign/ presented as @€@.
euro
  :: Char  -- ^ A character that corresponds with the /euro sign/.
euro = '\x20ac'

-- | The character used to render a /kip sign/ presented as @₭@.
kip
  :: Char  -- ^ A character that corresponds with the /kip sign/.
kip = '\x20ad'

-- | The character used to render a /tugrik sign/ presented as @₮@.
tugrik
  :: Char  -- ^ A character that corresponds with the /tugrik sign/.
tugrik = '\x20ae'

-- | The character used to render a /drachma sign/ presented as @₯@.
drachma
  :: Char  -- ^ A character that corresponds with the /drachma sign/.
drachma = '\x20af'

-- | The character used to render a /german penny sign/ presented as @₰@.
germanPenny
  :: Char  -- ^ A character that corresponds with the /german penny sign/.
germanPenny = '\x20b0'

-- | The character used to render a /peso sign/ presented as @₱@.
peso
  :: Char  -- ^ A character that corresponds with the /peso sign/.
peso = '\x20b1'

-- | The character used to render a /guarani sign/ presented as @₲@.
guarani
  :: Char  -- ^ A character that corresponds with the /guarani sign/.
guarani = '\x20b2'

-- | The character used to render a /austral sign/ presented as @₳@.
austral
  :: Char  -- ^ A character that corresponds with the /austral sign/.
austral = '\x20b3'

-- | The character used to render a /hryvnia sign/ presented as @₴@.
hryvnia
  :: Char  -- ^ A character that corresponds with the /hryvnia sign/.
hryvnia = '\x20b4'

-- | The character used to render a /cedi sign/ presented as @₵@.
cedi
  :: Char  -- ^ A character that corresponds with the /cedi sign/.
cedi = '\x20b5'

-- | The character used to render a /livre tournois sign/ presented as @₶@.
livreTournois
  :: Char  -- ^ A character that corresponds with the /livre tournois sign/.
livreTournois = '\x20b6'

-- | The character used to render a /spesmilo sign/ presented as @₷@.
spesmilo
  :: Char  -- ^ A character that corresponds with the /spesmilo sign/.
spesmilo = '\x20b7'

-- | The character used to render a /tenge sign/ presented as @₸@.
tenge
  :: Char  -- ^ A character that corresponds with the /tenge sign/.
tenge = '\x20b8'

-- | The character used to render a /indian rupee sign/ presented as @₹@.
indianRupee
  :: Char  -- ^ A character that corresponds with the /indian rupee sign/.
indianRupee = '\x20b9'

-- | The character used to render a /turkish lira sign/ presented as @₺@.
turkishLira
  :: Char  -- ^ A character that corresponds with the /turkish lira sign/.
turkishLira = '\x20ba'

-- | The character used to render a /nordic mark sign/ presented as @₻@.
nordicMark
  :: Char  -- ^ A character that corresponds with the /nordic mark sign/.
nordicMark = '\x20bb'

-- | The character used to render a /manat sign/ presented as @₼@.
manat
  :: Char  -- ^ A character that corresponds with the /manat sign/.
manat = '\x20bc'

-- | The character used to render a /ruble sign/ presented as @₽@.
ruble
  :: Char  -- ^ A character that corresponds with the /ruble sign/.
ruble = '\x20bd'

-- | The character used to render a /lari sign/ presented as @₾@.
lari
  :: Char  -- ^ A character that corresponds with the /lari sign/.
lari = '\x20be'

-- | The character used to render a /bitcoin sign/ presented as @₿@.
bitcoin
  :: Char  -- ^ A character that corresponds with the /bitcoin sign/.
bitcoin = '\x20bf'

-- | The character used to render a /north indic rupee mark/ presented as @꠸@.
northIndicRupeeMark
  :: Char  -- ^ A character that corresponds with the /north indic rupee mark/.
northIndicRupeeMark = '\xa838'

-- | The character used to render a /rial sign/ presented as @﷼@.
rial
  :: Char  -- ^ A character that corresponds with the /rial sign/.
rial = '\xfdfc'

-- | The character used to render a /small dollar sign/ presented as @﹩@.
smallDollar
  :: Char  -- ^ A character that corresponds with the /small dollar sign/.
smallDollar = '\xfe69'

-- | The character used to render a /fullwidth dollar sign/ presented as @＄@.
fullwidthDollar
  :: Char  -- ^ A character that corresponds with the /fullwidth dollar sign/.
fullwidthDollar = '\xff04'

-- | The character used to render a /fullwidth cent sign/ presented as @￠@.
fullwidthCent
  :: Char  -- ^ A character that corresponds with the /fullwidth cent sign/.
fullwidthCent = '\xffe0'

-- | The character used to render a /fullwidth pound sign/ presented as @￡@.
fullwidthPound
  :: Char  -- ^ A character that corresponds with the /fullwidth pound sign/.
fullwidthPound = '\xffe1'

-- | The character used to render a /fullwidth yen sign/ presented as @￥@.
fullwidthYen
  :: Char  -- ^ A character that corresponds with the /fullwidth yen sign/.
fullwidthYen = '\xffe5'

-- | The character used to render a /fullwidth won sign/ presented as @￦@.
fullwidthWon
  :: Char  -- ^ A character that corresponds with the /fullwidth won sign/.
fullwidthWon = '\xffe6'

-- | The character used to render a /tamil sign kaacu/ presented as @𑿝@.
tamilKaacu
  :: Char  -- ^ A character that corresponds with the /tamil sign kaacu/.
tamilKaacu = '\x11fdd'

-- | The character used to render a /tamil sign panam/ presented as @𑿞@.
tamilPanam
  :: Char  -- ^ A character that corresponds with the /tamil sign panam/.
tamilPanam = '\x11fde'

-- | The character used to render a /tamil sign pon/ presented as @𑿟@.
tamilPon
  :: Char  -- ^ A character that corresponds with the /tamil sign pon/.
tamilPon = '\x11fdf'

-- | The character used to render a /tamil sign varaakan/ presented as @𑿠@.
tamilVaraakan
  :: Char  -- ^ A character that corresponds with the /tamil sign varaakan/.
tamilVaraakan = '\x11fe0'

-- | The character used to render a /wancho ngun sign/ presented as @𞋿@.
wanchoNgun
  :: Char  -- ^ A character that corresponds with the /wancho ngun sign/.
wanchoNgun = '\x1e2ff'

-- | The character used to render a /indic siyaq rupee mark/ presented as @𞲰@.
indicSiyaqRupeeMark
  :: Char  -- ^ A character that corresponds with the /indic siyaq rupee mark/.
indicSiyaqRupeeMark = '\x1ecb0'
