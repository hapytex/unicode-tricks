{-# LANGUAGE PatternSynonyms, Safe #-}

{-|
Module      : Data.Char.Emoji
Description : A module that defines emoji and ways to render and modify emoji.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode defines 2182 emoji characters, this module aims to make working with emoji characters more convenient.
-}

module Data.Char.Emoji (
    -- * Flag emoji
    Flag
  , iso3166Alpha2ToFlag, iso3166Alpha2ToFlag', validFlagEmoji
    -- * Zodiac emoji
  , Zodiac(Aries, Taurus, Gemini, Cancer, Leo, Virgo, Libra, Scorpio, Sagittarius, Capricorn, Aquarius, Pisces)
    -- * Skin color modifier
  , SkinColorModifier(Light, MediumLight, Medium, MediumDark, Dark), fromFitzPatrick
    -- * Pattern symbols for Flags
  , pattern AC, pattern AD, pattern AE, pattern AF, pattern AG, pattern AI, pattern AL, pattern AM, pattern AO, pattern AQ, pattern AR
  , pattern AS, pattern AT, pattern AU, pattern AW, pattern AX, pattern AZ, pattern BA, pattern BB, pattern BD, pattern BE, pattern BF
  , pattern BG, pattern BH, pattern BI, pattern BJ, pattern BL, pattern BM, pattern BN, pattern BO, pattern BQ, pattern BR, pattern BS
  , pattern BT, pattern BV, pattern BW, pattern BY, pattern BZ, pattern CA, pattern CC, pattern CD, pattern CF, pattern CG, pattern CH
  , pattern CI, pattern CK, pattern CL, pattern CM, pattern CN, pattern CO, pattern CP, pattern CR, pattern CU, pattern CV, pattern CW
  , pattern CX, pattern CY, pattern CZ, pattern DE, pattern DG, pattern DJ, pattern DK, pattern DM, pattern DO, pattern DZ, pattern EA
  , pattern EC, pattern EE, pattern EG, pattern EH, pattern ER, pattern ES, pattern ET, pattern EU, pattern FI, pattern FJ, pattern FK
  , pattern FM, pattern FO, pattern FR, pattern GA, pattern GB, pattern GD, pattern GE, pattern GF, pattern GG, pattern GH, pattern GI
  , pattern GL, pattern GM, pattern GN, pattern GP, pattern GQ, pattern GR, pattern GS, pattern GT, pattern GU, pattern GW, pattern GY
  , pattern HK, pattern HM, pattern HN, pattern HR, pattern HT, pattern HU, pattern IC, pattern ID, pattern IE, pattern IL, pattern IM
  , pattern IN, pattern IO, pattern IQ, pattern IR, pattern IS, pattern IT, pattern JE, pattern JM, pattern JO, pattern JP, pattern KE
  , pattern KG, pattern KH, pattern KI, pattern KM, pattern KN, pattern KP, pattern KR, pattern KW, pattern KY, pattern KZ, pattern LA
  , pattern LB, pattern LC, pattern LI, pattern LK, pattern LR, pattern LS, pattern LT, pattern LU, pattern LV, pattern LY, pattern MA
  , pattern MC, pattern MD, pattern ME, pattern MF, pattern MG, pattern MH, pattern MK, pattern ML, pattern MM, pattern MN, pattern MO
  , pattern MP, pattern MQ, pattern MR, pattern MS, pattern MT, pattern MU, pattern MV, pattern MW, pattern MX, pattern MY, pattern MZ
  , pattern NA, pattern NC, pattern NE, pattern NF, pattern NG, pattern NI, pattern NL, pattern NO, pattern NP, pattern NR, pattern NU
  , pattern NZ, pattern OM, pattern PA, pattern PE, pattern PF, pattern PG, pattern PH, pattern PK, pattern PL, pattern PM, pattern PN
  , pattern PR, pattern PS, pattern PT, pattern PW, pattern PY, pattern QA, pattern RE, pattern RO, pattern RS, pattern RU, pattern RW
  , pattern SA, pattern SB, pattern SC, pattern SD, pattern SE, pattern SG, pattern SH, pattern SI, pattern SJ, pattern SK, pattern SL
  , pattern SM, pattern SN, pattern SO, pattern SR, pattern SS, pattern ST, pattern SV, pattern SX, pattern SY, pattern SZ, pattern TA
  , pattern TC, pattern TD, pattern TF, pattern TG, pattern TH, pattern TJ, pattern TK, pattern TL, pattern TM, pattern TN, pattern TO
  , pattern TR, pattern TT, pattern TV, pattern TW, pattern TZ, pattern UA, pattern UG, pattern UM, pattern UN, pattern US, pattern UY
  , pattern UZ, pattern VA, pattern VC, pattern VE, pattern VG, pattern VI, pattern VN, pattern VU, pattern WF, pattern WS, pattern XK
  , pattern YE, pattern YT, pattern ZA, pattern ZM, pattern ZW
    -- * Pattern synonyms for emoji elements
  , pattern Ram, pattern Bull, pattern Twins, pattern Crab, pattern Lion, pattern Maiden, pattern Scales, pattern Scorpius, pattern Scorpion
  , pattern Centaur, pattern Archer, pattern Capricornus, pattern MountainGoat, pattern GoatHorned, pattern SeaGoat, pattern WaterBearer
  , pattern Fish
  , pattern FitzPatrickI, pattern FitzPatrickII, pattern FitzPatrickIII, pattern FitzPatrickIV, pattern FitzPatrickV, pattern FitzPatrickVI
  ) where

import Prelude hiding (LT, GT)
import Data.Char(toUpper)
import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar'), mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Function(on)
import Data.Text(Text, pack)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

_skinColorOffset :: Int
_skinColorOffset = 0x1f3fb

_zodiacOffset :: Int
_zodiacOffset = 0x2648

-- | A data type that stores a (country) flag by the two characters of the ISO
-- 3166 Alpa-2 standard. The data constructor is hidden to prevent making flags
-- with a combination of characters that is invalid.
-- Besides the countries defined in the ISO-3166 Alpha-2 standard, only the
-- Antarctica (AQ), the European Union (EU) and the United Nations (UN) have a
-- flag. Deprecated territories like the Soviet Union (SU) and Yugoslavia (YU)
-- have no corresponding flag.
data Flag = Flag Char Char deriving (Eq, Ord, Show)

instance Bounded Flag where
    minBound = AC
    maxBound = ZW

-- | The 'Flag' pattern used for /Ascension Island/ denoted with /AC/.
pattern AC :: Flag
pattern AC = Flag 'A' 'C'

-- | The 'Flag' pattern used for /Andorra/ denoted with /AD/.
pattern AD :: Flag
pattern AD = Flag 'A' 'D'

-- | The 'Flag' pattern used for the /United Arab Emirates/ denoted with /AE/.
pattern AE :: Flag
pattern AE = Flag 'A' 'E'

-- | The 'Flag' pattern used for /Afghanistan/ denoted with /AF/.
pattern AF :: Flag
pattern AF = Flag 'A' 'F'

-- | The 'Flag' pattern used for /Antigua & Barbuda/ denoted with /AG/.
pattern AG :: Flag
pattern AG = Flag 'A' 'G'

-- | The 'Flag' pattern used for /Anguilla/ denoted with /AI/.
pattern AI :: Flag
pattern AI = Flag 'A' 'I'

-- | The 'Flag' pattern used for /Albania/ denoted with /AL/.
pattern AL :: Flag
pattern AL = Flag 'A' 'L'

-- | The 'Flag' pattern used for /Armenia/ denoted with /AM/.
pattern AM :: Flag
pattern AM = Flag 'A' 'M'

-- | The 'Flag' pattern used for /Angola/ denoted with /AO/.
pattern AO :: Flag
pattern AO = Flag 'A' 'O'

-- | The 'Flag' pattern used for /Antarctica/ denoted with /AQ/.
pattern AQ :: Flag
pattern AQ = Flag 'A' 'Q'

-- | The 'Flag' pattern used for /Argentina/ denoted with /AR/.
pattern AR :: Flag
pattern AR = Flag 'A' 'R'

-- | The 'Flag' pattern used for /American Samoa/ denoted with /AS/.
pattern AS :: Flag
pattern AS = Flag 'A' 'S'

-- | The 'Flag' pattern used for /Austria/ denoted with /AT/.
pattern AT :: Flag
pattern AT = Flag 'A' 'T'

-- | The 'Flag' pattern used for /Australia/ denoted with /AU/.
pattern AU :: Flag
pattern AU = Flag 'A' 'U'

-- | The 'Flag' pattern used for /Aruba/ denoted with /AW/.
pattern AW :: Flag
pattern AW = Flag 'A' 'W'

-- | The 'Flag' pattern used for the /Åland Islands/ denoted with /AX/.
pattern AX :: Flag
pattern AX = Flag 'A' 'X'

-- | The 'Flag' pattern used for /Azerbaijan/ denoted with /AZ/.
pattern AZ :: Flag
pattern AZ = Flag 'A' 'Z'

-- | The 'Flag' pattern used for /Bosnia & Herzegovina/ denoted with /BA/.
pattern BA :: Flag
pattern BA = Flag 'B' 'A'

-- | The 'Flag' pattern used for /Barbados/ denoted with /BB/.
pattern BB :: Flag
pattern BB = Flag 'B' 'B'

-- | The 'Flag' pattern used for /Bangladesh/ denoted with /BD/.
pattern BD :: Flag
pattern BD = Flag 'B' 'D'

-- | The 'Flag' pattern used for /Belgium/ denoted with /BE/.
pattern BE :: Flag
pattern BE = Flag 'B' 'E'

-- | The 'Flag' pattern used for /Burkina Faso/ denoted with /BF/.
pattern BF :: Flag
pattern BF = Flag 'B' 'F'

-- | The 'Flag' pattern used for /Bulgaria/ denoted with /BG/.
pattern BG :: Flag
pattern BG = Flag 'B' 'G'

-- | The 'Flag' pattern used for /Bahrain/ denoted with /BH/.
pattern BH :: Flag
pattern BH = Flag 'B' 'H'

-- | The 'Flag' pattern used for /Burundi/ denoted with /BI/.
pattern BI :: Flag
pattern BI = Flag 'B' 'I'

-- | The 'Flag' pattern used for /Benin/ denoted with /BJ/.
pattern BJ :: Flag
pattern BJ = Flag 'B' 'J'

-- | The 'Flag' pattern used for /St. Barthélemy/ denoted with /BL/.
pattern BL :: Flag
pattern BL = Flag 'B' 'L'

-- | The 'Flag' pattern used for /Bermuda/ denoted with /BM/.
pattern BM :: Flag
pattern BM = Flag 'B' 'M'

-- | The 'Flag' pattern used for /Brunei/ denoted with /BN/.
pattern BN :: Flag
pattern BN = Flag 'B' 'N'

-- | The 'Flag' pattern used for /Bolivia/ denoted with /BO/.
pattern BO :: Flag
pattern BO = Flag 'B' 'O'

-- | The 'Flag' pattern used for the /Caribbean Netherlands/ denoted with /BQ/.
pattern BQ :: Flag
pattern BQ = Flag 'B' 'Q'

-- | The 'Flag' pattern used for /Brazil/ denoted with /BR/.
pattern BR :: Flag
pattern BR = Flag 'B' 'R'

-- | The 'Flag' pattern used for the /Bahamas/ denoted with /BS/.
pattern BS :: Flag
pattern BS = Flag 'B' 'S'

-- | The 'Flag' pattern used for /Bhutan/ denoted with /BT/.
pattern BT :: Flag
pattern BT = Flag 'B' 'T'

-- | The 'Flag' pattern used for /Bouvet Island/ denoted with /BV/.
pattern BV :: Flag
pattern BV = Flag 'B' 'V'

-- | The 'Flag' pattern used for /Botswana/ denoted with /BW/.
pattern BW :: Flag
pattern BW = Flag 'B' 'W'

-- | The 'Flag' pattern used for /Belarus/ denoted with /BY/.
pattern BY :: Flag
pattern BY = Flag 'B' 'Y'

-- | The 'Flag' pattern used for /Belize/ denoted with /BZ/.
pattern BZ :: Flag
pattern BZ = Flag 'B' 'Z'

-- | The 'Flag' pattern used for /Canada/ denoted with /CA/.
pattern CA :: Flag
pattern CA = Flag 'C' 'A'

-- | The 'Flag' pattern used for the /Cocos (Keeling) Islands/ denoted with /CC/.
pattern CC :: Flag
pattern CC = Flag 'C' 'C'

-- | The 'Flag' pattern used for /Congo - Kinshasa/ denoted with /CD/.
pattern CD :: Flag
pattern CD = Flag 'C' 'D'

-- | The 'Flag' pattern used for /Central African Republic/ denoted with /CF/.
pattern CF :: Flag
pattern CF = Flag 'C' 'F'

-- | The 'Flag' pattern used for /Congo - Brazzaville/ denoted with /CG/.
pattern CG :: Flag
pattern CG = Flag 'C' 'G'

-- | The 'Flag' pattern used for /Switzerland/ denoted with /CH/.
pattern CH :: Flag
pattern CH = Flag 'C' 'H'

-- | The 'Flag' pattern used for /Côte d’Ivoire/ denoted with /CI/.
pattern CI :: Flag
pattern CI = Flag 'C' 'I'

-- | The 'Flag' pattern used for the /Cook Islands/ denoted with /CK/.
pattern CK :: Flag
pattern CK = Flag 'C' 'K'

-- | The 'Flag' pattern used for /Chile/ denoted with /CL/.
pattern CL :: Flag
pattern CL = Flag 'C' 'L'

-- | The 'Flag' pattern used for /Cameroon/ denoted with /CM/.
pattern CM :: Flag
pattern CM = Flag 'C' 'M'

-- | The 'Flag' pattern used for /China/ denoted with /CN/.
pattern CN :: Flag
pattern CN = Flag 'C' 'N'

-- | The 'Flag' pattern used for /Colombia/ denoted with /CO/.
pattern CO :: Flag
pattern CO = Flag 'C' 'O'

-- | The 'Flag' pattern used for /Clipperton Island/ denoted with /CP/.
pattern CP :: Flag
pattern CP = Flag 'C' 'P'

-- | The 'Flag' pattern used for /Costa Rica/ denoted with /CR/.
pattern CR :: Flag
pattern CR = Flag 'C' 'R'

-- | The 'Flag' pattern used for /Cuba/ denoted with /CU/.
pattern CU :: Flag
pattern CU = Flag 'C' 'U'

-- | The 'Flag' pattern used for /Cape Verde/ denoted with /CV/.
pattern CV :: Flag
pattern CV = Flag 'C' 'V'

-- | The 'Flag' pattern used for /Curaçao/ denoted with /CW/.
pattern CW :: Flag
pattern CW = Flag 'C' 'W'

-- | The 'Flag' pattern used for /Christmas Island/ denoted with /CX/.
pattern CX :: Flag
pattern CX = Flag 'C' 'X'

-- | The 'Flag' pattern used for /Cyprus/ denoted with /CY/.
pattern CY :: Flag
pattern CY = Flag 'C' 'Y'

-- | The 'Flag' pattern used for /Czechia/ denoted with /CZ/.
pattern CZ :: Flag
pattern CZ = Flag 'C' 'Z'

-- | The 'Flag' pattern used for /Germany/ denoted with /DE/.
pattern DE :: Flag
pattern DE = Flag 'D' 'E'

-- | The 'Flag' pattern used for /Diego Garcia/ denoted with /DG/.
pattern DG :: Flag
pattern DG = Flag 'D' 'G'

-- | The 'Flag' pattern used for /Djibouti/ denoted with /DJ/.
pattern DJ :: Flag
pattern DJ = Flag 'D' 'J'

-- | The 'Flag' pattern used for /Denmark/ denoted with /DK/.
pattern DK :: Flag
pattern DK = Flag 'D' 'K'

-- | The 'Flag' pattern used for /Dominica/ denoted with /DM/.
pattern DM :: Flag
pattern DM = Flag 'D' 'M'

-- | The 'Flag' pattern used for /Dominican Republic/ denoted with /DO/.
pattern DO :: Flag
pattern DO = Flag 'D' 'O'

-- | The 'Flag' pattern used for /Algeria/ denoted with /DZ/.
pattern DZ :: Flag
pattern DZ = Flag 'D' 'Z'

-- | The 'Flag' pattern used for /Ceuta & Melilla/ denoted with /EA/.
pattern EA :: Flag
pattern EA = Flag 'E' 'A'

-- | The 'Flag' pattern used for /Ecuador/ denoted with /EC/.
pattern EC :: Flag
pattern EC = Flag 'E' 'C'

-- | The 'Flag' pattern used for /Estonia/ denoted with /EE/.
pattern EE :: Flag
pattern EE = Flag 'E' 'E'

-- | The 'Flag' pattern used for /Egypt/ denoted with /EG/.
pattern EG :: Flag
pattern EG = Flag 'E' 'G'

-- | The 'Flag' pattern used for /Western Sahara/ denoted with /EH/.
pattern EH :: Flag
pattern EH = Flag 'E' 'H'

-- | The 'Flag' pattern used for /Eritrea/ denoted with /ER/.
pattern ER :: Flag
pattern ER = Flag 'E' 'R'

-- | The 'Flag' pattern used for /Spain/ denoted with /ES/.
pattern ES :: Flag
pattern ES = Flag 'E' 'S'

-- | The 'Flag' pattern used for /Ethiopia/ denoted with /ET/.
pattern ET :: Flag
pattern ET = Flag 'E' 'T'

-- | The 'Flag' pattern used for the /European Union/ denoted with /EU/.
pattern EU :: Flag
pattern EU = Flag 'E' 'U'

-- | The 'Flag' pattern used for /Finland/ denoted with /FI/.
pattern FI :: Flag
pattern FI = Flag 'F' 'I'

-- | The 'Flag' pattern used for /Fiji/ denoted with /FJ/.
pattern FJ :: Flag
pattern FJ = Flag 'F' 'J'

-- | The 'Flag' pattern used for the /Falkland Islands/ denoted with /FK/.
pattern FK :: Flag
pattern FK = Flag 'F' 'K'

-- | The 'Flag' pattern used for /Micronesia/ denoted with /FM/.
pattern FM :: Flag
pattern FM = Flag 'F' 'M'

-- | The 'Flag' pattern used for the /Faroe Islands/ denoted with /FO/.
pattern FO :: Flag
pattern FO = Flag 'F' 'O'

-- | The 'Flag' pattern used for /France/ denoted with /FR/.
pattern FR :: Flag
pattern FR = Flag 'F' 'R'

-- | The 'Flag' pattern used for /Gabon/ denoted with /GA/.
pattern GA :: Flag
pattern GA = Flag 'G' 'A'

-- | The 'Flag' pattern used for /United Kingdom/ denoted with /GB/.
pattern GB :: Flag
pattern GB = Flag 'G' 'B'

-- | The 'Flag' pattern used for /Grenada/ denoted with /GD/.
pattern GD :: Flag
pattern GD = Flag 'G' 'D'

-- | The 'Flag' pattern used for /Georgia/ denoted with /GE/.
pattern GE :: Flag
pattern GE = Flag 'G' 'E'

-- | The 'Flag' pattern used for /French Guiana/ denoted with /GF/.
pattern GF :: Flag
pattern GF = Flag 'G' 'F'

-- | The 'Flag' pattern used for /Guernsey/ denoted with /GG/.
pattern GG :: Flag
pattern GG = Flag 'G' 'G'

-- | The 'Flag' pattern used for /Ghana/ denoted with /GH/.
pattern GH :: Flag
pattern GH = Flag 'G' 'H'

-- | The 'Flag' pattern used for /Gibraltar/ denoted with /GI/.
pattern GI :: Flag
pattern GI = Flag 'G' 'I'

-- | The 'Flag' pattern used for /Greenland/ denoted with /GL/.
pattern GL :: Flag
pattern GL = Flag 'G' 'L'

-- | The 'Flag' pattern used for /Gambia/ denoted with /GM/.
pattern GM :: Flag
pattern GM = Flag 'G' 'M'

-- | The 'Flag' pattern used for /Guinea/ denoted with /GN/.
pattern GN :: Flag
pattern GN = Flag 'G' 'N'

-- | The 'Flag' pattern used for /Guadeloupe/ denoted with /GP/.
pattern GP :: Flag
pattern GP = Flag 'G' 'P'

-- | The 'Flag' pattern used for /Equatorial Guinea/ denoted with /GQ/.
pattern GQ :: Flag
pattern GQ = Flag 'G' 'Q'

-- | The 'Flag' pattern used for /Greece/ denoted with /GR/.
pattern GR :: Flag
pattern GR = Flag 'G' 'R'

-- | The 'Flag' pattern used for the /South Georgia & South Sandwich Islands/ denoted with /GS/.
pattern GS :: Flag
pattern GS = Flag 'G' 'S'

-- | The 'Flag' pattern used for /Guatemala/ denoted with /GT/.
pattern GT :: Flag
pattern GT = Flag 'G' 'T'

-- | The 'Flag' pattern used for /Guam/ denoted with /GU/.
pattern GU :: Flag
pattern GU = Flag 'G' 'U'

-- | The 'Flag' pattern used for /Guinea-Bissau/ denoted with /GW/.
pattern GW :: Flag
pattern GW = Flag 'G' 'W'

-- | The 'Flag' pattern used for /Guyana/ denoted with /GY/.
pattern GY :: Flag
pattern GY = Flag 'G' 'Y'

-- | The 'Flag' pattern used for /Hong Kong SAR China/ denoted with /HK/.
pattern HK :: Flag
pattern HK = Flag 'H' 'K'

-- | The 'Flag' pattern used for the /Heard & McDonald Islands/ denoted with /HM/.
pattern HM :: Flag
pattern HM = Flag 'H' 'M'

-- | The 'Flag' pattern used for /Honduras/ denoted with /HN/.
pattern HN :: Flag
pattern HN = Flag 'H' 'N'

-- | The 'Flag' pattern used for /Croatia/ denoted with /HR/.
pattern HR :: Flag
pattern HR = Flag 'H' 'R'

-- | The 'Flag' pattern used for /Haiti/ denoted with /HT/.
pattern HT :: Flag
pattern HT = Flag 'H' 'T'

-- | The 'Flag' pattern used for /Hungary/ denoted with /HU/.
pattern HU :: Flag
pattern HU = Flag 'H' 'U'

-- | The 'Flag' pattern used for the /Canary Islands/ denoted with /IC/.
pattern IC :: Flag
pattern IC = Flag 'I' 'C'

-- | The 'Flag' pattern used for /Indonesia/ denoted with /ID/.
pattern ID :: Flag
pattern ID = Flag 'I' 'D'

-- | The 'Flag' pattern used for /Ireland/ denoted with /IE/.
pattern IE :: Flag
pattern IE = Flag 'I' 'E'

-- | The 'Flag' pattern used for /Israel/ denoted with /IL/.
pattern IL :: Flag
pattern IL = Flag 'I' 'L'

-- | The 'Flag' pattern used for /Isle of Man/ denoted with /IM/.
pattern IM :: Flag
pattern IM = Flag 'I' 'M'

-- | The 'Flag' pattern used for /India/ denoted with /IN/.
pattern IN :: Flag
pattern IN = Flag 'I' 'N'

-- | The 'Flag' pattern used for /British Indian Ocean Territory/ denoted with /IO/.
pattern IO :: Flag
pattern IO = Flag 'I' 'O'

-- | The 'Flag' pattern used for /Iraq/ denoted with /IQ/.
pattern IQ :: Flag
pattern IQ = Flag 'I' 'Q'

-- | The 'Flag' pattern used for /Iran/ denoted with /IR/.
pattern IR :: Flag
pattern IR = Flag 'I' 'R'

-- | The 'Flag' pattern used for /Iceland/ denoted with /IS/.
pattern IS :: Flag
pattern IS = Flag 'I' 'S'

-- | The 'Flag' pattern used for /Italy/ denoted with /IT/.
pattern IT :: Flag
pattern IT = Flag 'I' 'T'

-- | The 'Flag' pattern used for /Jersey/ denoted with /JE/.
pattern JE :: Flag
pattern JE = Flag 'J' 'E'

-- | The 'Flag' pattern used for /Jamaica/ denoted with /JM/.
pattern JM :: Flag
pattern JM = Flag 'J' 'M'

-- | The 'Flag' pattern used for /Jordan/ denoted with /JO/.
pattern JO :: Flag
pattern JO = Flag 'J' 'O'

-- | The 'Flag' pattern used for /Japan/ denoted with /JP/.
pattern JP :: Flag
pattern JP = Flag 'J' 'P'

-- | The 'Flag' pattern used for /Kenya/ denoted with /KE/.
pattern KE :: Flag
pattern KE = Flag 'K' 'E'

-- | The 'Flag' pattern used for /Kyrgyzstan/ denoted with /KG/.
pattern KG :: Flag
pattern KG = Flag 'K' 'G'

-- | The 'Flag' pattern used for /Cambodia/ denoted with /KH/.
pattern KH :: Flag
pattern KH = Flag 'K' 'H'

-- | The 'Flag' pattern used for /Kiribati/ denoted with /KI/.
pattern KI :: Flag
pattern KI = Flag 'K' 'I'

-- | The 'Flag' pattern used for the /Comoros/ denoted with /KM/.
pattern KM :: Flag
pattern KM = Flag 'K' 'M'

-- | The 'Flag' pattern used for /St. Kitts & Nevis/ denoted with /KN/.
pattern KN :: Flag
pattern KN = Flag 'K' 'N'

-- | The 'Flag' pattern used for /North Korea/ denoted with /KP/.
pattern KP :: Flag
pattern KP = Flag 'K' 'P'

-- | The 'Flag' pattern used for /South Korea/ denoted with /KR/.
pattern KR :: Flag
pattern KR = Flag 'K' 'R'

-- | The 'Flag' pattern used for /Kuwait/ denoted with /KW/.
pattern KW :: Flag
pattern KW = Flag 'K' 'W'

-- | The 'Flag' pattern used for the /Cayman Islands/ denoted with /KY/.
pattern KY :: Flag
pattern KY = Flag 'K' 'Y'

-- | The 'Flag' pattern used for /Kazakhstan/ denoted with /KZ/.
pattern KZ :: Flag
pattern KZ = Flag 'K' 'Z'

-- | The 'Flag' pattern used for /Laos/ denoted with /LA/.
pattern LA :: Flag
pattern LA = Flag 'L' 'A'

-- | The 'Flag' pattern used for /Lebanon/ denoted with /LB/.
pattern LB :: Flag
pattern LB = Flag 'L' 'B'

-- | The 'Flag' pattern used for /St. Lucia/ denoted with /LC/.
pattern LC :: Flag
pattern LC = Flag 'L' 'C'

-- | The 'Flag' pattern used for /Liechtenstein/ denoted with /LI/.
pattern LI :: Flag
pattern LI = Flag 'L' 'I'

-- | The 'Flag' pattern used for /Sri Lanka/ denoted with /LK/.
pattern LK :: Flag
pattern LK = Flag 'L' 'K'

-- | The 'Flag' pattern used for /Liberia/ denoted with /LR/.
pattern LR :: Flag
pattern LR = Flag 'L' 'R'

-- | The 'Flag' pattern used for /Lesotho/ denoted with /LS/.
pattern LS :: Flag
pattern LS = Flag 'L' 'S'

-- | The 'Flag' pattern used for /Lithuania/ denoted with /LT/.
pattern LT :: Flag
pattern LT = Flag 'L' 'T'

-- | The 'Flag' pattern used for /Luxembourg/ denoted with /LU/.
pattern LU :: Flag
pattern LU = Flag 'L' 'U'

-- | The 'Flag' pattern used for /Latvia/ denoted with /LV/.
pattern LV :: Flag
pattern LV = Flag 'L' 'V'

-- | The 'Flag' pattern used for /Libya/ denoted with /LY/.
pattern LY :: Flag
pattern LY = Flag 'L' 'Y'

-- | The 'Flag' pattern used for /Morocco/ denoted with /MA/.
pattern MA :: Flag
pattern MA = Flag 'M' 'A'

-- | The 'Flag' pattern used for /Monaco/ denoted with /MC/.
pattern MC :: Flag
pattern MC = Flag 'M' 'C'

-- | The 'Flag' pattern used for /Moldova/ denoted with /MD/.
pattern MD :: Flag
pattern MD = Flag 'M' 'D'

-- | The 'Flag' pattern used for /Montenegro/ denoted with /ME/.
pattern ME :: Flag
pattern ME = Flag 'M' 'E'

-- | The 'Flag' pattern used for /St. Martin/ denoted with /MF/.
pattern MF :: Flag
pattern MF = Flag 'M' 'F'

-- | The 'Flag' pattern used for /Madagascar/ denoted with /MG/.
pattern MG :: Flag
pattern MG = Flag 'M' 'G'

-- | The 'Flag' pattern used for the /Marshall Islands/ denoted with /MH/.
pattern MH :: Flag
pattern MH = Flag 'M' 'H'

-- | The 'Flag' pattern used for /North Macedonia/ denoted with /MK/.
pattern MK :: Flag
pattern MK = Flag 'M' 'K'

-- | The 'Flag' pattern used for /Mali/ denoted with /ML/.
pattern ML :: Flag
pattern ML = Flag 'M' 'L'

-- | The 'Flag' pattern used for /Myanmar (Burma)/ denoted with /MM/.
pattern MM :: Flag
pattern MM = Flag 'M' 'M'

-- | The 'Flag' pattern used for /Mongolia/ denoted with /MN/.
pattern MN :: Flag
pattern MN = Flag 'M' 'N'

-- | The 'Flag' pattern used for /Macao SAR China/ denoted with /MO/.
pattern MO :: Flag
pattern MO = Flag 'M' 'O'

-- | The 'Flag' pattern used for the /Northern Mariana Islands/ denoted with /MP/.
pattern MP :: Flag
pattern MP = Flag 'M' 'P'

-- | The 'Flag' pattern used for /Martinique/ denoted with /MQ/.
pattern MQ :: Flag
pattern MQ = Flag 'M' 'Q'

-- | The 'Flag' pattern used for /Mauritania/ denoted with /MR/.
pattern MR :: Flag
pattern MR = Flag 'M' 'R'

-- | The 'Flag' pattern used for /Montserrat/ denoted with /MS/.
pattern MS :: Flag
pattern MS = Flag 'M' 'S'

-- | The 'Flag' pattern used for /Malta/ denoted with /MT/.
pattern MT :: Flag
pattern MT = Flag 'M' 'T'

-- | The 'Flag' pattern used for /Mauritius/ denoted with /MU/.
pattern MU :: Flag
pattern MU = Flag 'M' 'U'

-- | The 'Flag' pattern used for the /Maldives/ denoted with /MV/.
pattern MV :: Flag
pattern MV = Flag 'M' 'V'

-- | The 'Flag' pattern used for /Malawi/ denoted with /MW/.
pattern MW :: Flag
pattern MW = Flag 'M' 'W'

-- | The 'Flag' pattern used for /Mexico/ denoted with /MX/.
pattern MX :: Flag
pattern MX = Flag 'M' 'X'

-- | The 'Flag' pattern used for /Malaysia/ denoted with /MY/.
pattern MY :: Flag
pattern MY = Flag 'M' 'Y'

-- | The 'Flag' pattern used for /Mozambique/ denoted with /MZ/.
pattern MZ :: Flag
pattern MZ = Flag 'M' 'Z'

-- | The 'Flag' pattern used for /Namibia/ denoted with /NA/.
pattern NA :: Flag
pattern NA = Flag 'N' 'A'

-- | The 'Flag' pattern used for /New Caledonia/ denoted with /NC/.
pattern NC :: Flag
pattern NC = Flag 'N' 'C'

-- | The 'Flag' pattern used for /Niger/ denoted with /NE/.
pattern NE :: Flag
pattern NE = Flag 'N' 'E'

-- | The 'Flag' pattern used for /Norfolk Island/ denoted with /NF/.
pattern NF :: Flag
pattern NF = Flag 'N' 'F'

-- | The 'Flag' pattern used for /Nigeria/ denoted with /NG/.
pattern NG :: Flag
pattern NG = Flag 'N' 'G'

-- | The 'Flag' pattern used for /Nicaragua/ denoted with /NI/.
pattern NI :: Flag
pattern NI = Flag 'N' 'I'

-- | The 'Flag' pattern used for the /Netherlands/ denoted with /NL/.
pattern NL :: Flag
pattern NL = Flag 'N' 'L'

-- | The 'Flag' pattern used for /Norway/ denoted with /NO/.
pattern NO :: Flag
pattern NO = Flag 'N' 'O'

-- | The 'Flag' pattern used for /Nepal/ denoted with /NP/.
pattern NP :: Flag
pattern NP = Flag 'N' 'P'

-- | The 'Flag' pattern used for /Nauru/ denoted with /NR/.
pattern NR :: Flag
pattern NR = Flag 'N' 'R'

-- | The 'Flag' pattern used for /Niue/ denoted with /NU/.
pattern NU :: Flag
pattern NU = Flag 'N' 'U'

-- | The 'Flag' pattern used for /New Zealand/ denoted with /NZ/.
pattern NZ :: Flag
pattern NZ = Flag 'N' 'Z'

-- | The 'Flag' pattern used for /Oman/ denoted with /OM/.
pattern OM :: Flag
pattern OM = Flag 'O' 'M'

-- | The 'Flag' pattern used for /Panama/ denoted with /PA/.
pattern PA :: Flag
pattern PA = Flag 'P' 'A'

-- | The 'Flag' pattern used for /Peru/ denoted with /PE/.
pattern PE :: Flag
pattern PE = Flag 'P' 'E'

-- | The 'Flag' pattern used for /French Polynesia/ denoted with /PF/.
pattern PF :: Flag
pattern PF = Flag 'P' 'F'

-- | The 'Flag' pattern used for /Papua New Guinea/ denoted with /PG/.
pattern PG :: Flag
pattern PG = Flag 'P' 'G'

-- | The 'Flag' pattern used for the /Philippines/ denoted with /PH/.
pattern PH :: Flag
pattern PH = Flag 'P' 'H'

-- | The 'Flag' pattern used for /Pakistan/ denoted with /PK/.
pattern PK :: Flag
pattern PK = Flag 'P' 'K'

-- | The 'Flag' pattern used for /Poland/ denoted with /PL/.
pattern PL :: Flag
pattern PL = Flag 'P' 'L'

-- | The 'Flag' pattern used for /St. Pierre & Miquelon/ denoted with /PM/.
pattern PM :: Flag
pattern PM = Flag 'P' 'M'

-- | The 'Flag' pattern used for the /Pitcairn Islands/ denoted with /PN/.
pattern PN :: Flag
pattern PN = Flag 'P' 'N'

-- | The 'Flag' pattern used for /Puerto Rico/ denoted with /PR/.
pattern PR :: Flag
pattern PR = Flag 'P' 'R'

-- | The 'Flag' pattern used for the /Palestinian Territories/ denoted with /PS/.
pattern PS :: Flag
pattern PS = Flag 'P' 'S'

-- | The 'Flag' pattern used for /Portugal/ denoted with /PT/.
pattern PT :: Flag
pattern PT = Flag 'P' 'T'

-- | The 'Flag' pattern used for /Palau/ denoted with /PW/.
pattern PW :: Flag
pattern PW = Flag 'P' 'W'

-- | The 'Flag' pattern used for /Paraguay/ denoted with /PY/.
pattern PY :: Flag
pattern PY = Flag 'P' 'Y'

-- | The 'Flag' pattern used for /Qatar/ denoted with /QA/.
pattern QA :: Flag
pattern QA = Flag 'Q' 'A'

-- | The 'Flag' pattern used for /Réunion/ denoted with /RE/.
pattern RE :: Flag
pattern RE = Flag 'R' 'E'

-- | The 'Flag' pattern used for /Romania/ denoted with /RO/.
pattern RO :: Flag
pattern RO = Flag 'R' 'O'

-- | The 'Flag' pattern used for /Serbia/ denoted with /RS/.
pattern RS :: Flag
pattern RS = Flag 'R' 'S'

-- | The 'Flag' pattern used for /Russia/ denoted with /RU/.
pattern RU :: Flag
pattern RU = Flag 'R' 'U'

-- | The 'Flag' pattern used for /Rwanda/ denoted with /RW/.
pattern RW :: Flag
pattern RW = Flag 'R' 'W'

-- | The 'Flag' pattern used for /Saudi Arabia/ denoted with /SA/.
pattern SA :: Flag
pattern SA = Flag 'S' 'A'

-- | The 'Flag' pattern used for the /Solomon Islands/ denoted with /SB/.
pattern SB :: Flag
pattern SB = Flag 'S' 'B'

-- | The 'Flag' pattern used for /Seychelles/ denoted with /SC/.
pattern SC :: Flag
pattern SC = Flag 'S' 'C'

-- | The 'Flag' pattern used for /Sudan/ denoted with /SD/.
pattern SD :: Flag
pattern SD = Flag 'S' 'D'

-- | The 'Flag' pattern used for /Sweden/ denoted with /SE/.
pattern SE :: Flag
pattern SE = Flag 'S' 'E'

-- | The 'Flag' pattern used for /Singapore/ denoted with /SG/.
pattern SG :: Flag
pattern SG = Flag 'S' 'G'

-- | The 'Flag' pattern used for /St. Helena/ denoted with /SH/.
pattern SH :: Flag
pattern SH = Flag 'S' 'H'

-- | The 'Flag' pattern used for /Slovenia/ denoted with /SI/.
pattern SI :: Flag
pattern SI = Flag 'S' 'I'

-- | The 'Flag' pattern used for /Svalbard & Jan Mayen/ denoted with /SJ/.
pattern SJ :: Flag
pattern SJ = Flag 'S' 'J'

-- | The 'Flag' pattern used for /Slovakia/ denoted with /SK/.
pattern SK :: Flag
pattern SK = Flag 'S' 'K'

-- | The 'Flag' pattern used for /Sierra Leone/ denoted with /SL/.
pattern SL :: Flag
pattern SL = Flag 'S' 'L'

-- | The 'Flag' pattern used for /San Marino/ denoted with /SM/.
pattern SM :: Flag
pattern SM = Flag 'S' 'M'

-- | The 'Flag' pattern used for /Senegal/ denoted with /SN/.
pattern SN :: Flag
pattern SN = Flag 'S' 'N'

-- | The 'Flag' pattern used for /Somalia/ denoted with /SO/.
pattern SO :: Flag
pattern SO = Flag 'S' 'O'

-- | The 'Flag' pattern used for /Suriname/ denoted with /SR/.
pattern SR :: Flag
pattern SR = Flag 'S' 'R'

-- | The 'Flag' pattern used for /South Sudan/ denoted with /SS/.
pattern SS :: Flag
pattern SS = Flag 'S' 'S'

-- | The 'Flag' pattern used for /São Tomé & Príncipe/ denoted with /ST/.
pattern ST :: Flag
pattern ST = Flag 'S' 'T'

-- | The 'Flag' pattern used for /El Salvador/ denoted with /SV/.
pattern SV :: Flag
pattern SV = Flag 'S' 'V'

-- | The 'Flag' pattern used for /Sint Maarten/ denoted with /SX/.
pattern SX :: Flag
pattern SX = Flag 'S' 'X'

-- | The 'Flag' pattern used for /Syria/ denoted with /SY/.
pattern SY :: Flag
pattern SY = Flag 'S' 'Y'

-- | The 'Flag' pattern used for /Eswatini/ denoted with /SZ/.
pattern SZ :: Flag
pattern SZ = Flag 'S' 'Z'

-- | The 'Flag' pattern used for /Tristan da Cunha/ denoted with /TA/.
pattern TA :: Flag
pattern TA = Flag 'T' 'A'

-- | The 'Flag' pattern used for the /Turks & Caicos Islands/ denoted with /TC/.
pattern TC :: Flag
pattern TC = Flag 'T' 'C'

-- | The 'Flag' pattern used for /Chad/ denoted with /TD/.
pattern TD :: Flag
pattern TD = Flag 'T' 'D'

-- | The 'Flag' pattern used for the /French Southern Territories/ denoted with /TF/.
pattern TF :: Flag
pattern TF = Flag 'T' 'F'

-- | The 'Flag' pattern used for /Togo/ denoted with /TG/.
pattern TG :: Flag
pattern TG = Flag 'T' 'G'

-- | The 'Flag' pattern used for /Thailand/ denoted with /TH/.
pattern TH :: Flag
pattern TH = Flag 'T' 'H'

-- | The 'Flag' pattern used for /Tajikistan/ denoted with /TJ/.
pattern TJ :: Flag
pattern TJ = Flag 'T' 'J'

-- | The 'Flag' pattern used for /Tokelau/ denoted with /TK/.
pattern TK :: Flag
pattern TK = Flag 'T' 'K'

-- | The 'Flag' pattern used for /Timor-Leste/ denoted with /TL/.
pattern TL :: Flag
pattern TL = Flag 'T' 'L'

-- | The 'Flag' pattern used for /Turkmenistan/ denoted with /TM/.
pattern TM :: Flag
pattern TM = Flag 'T' 'M'

-- | The 'Flag' pattern used for /Tunisia/ denoted with /TN/.
pattern TN :: Flag
pattern TN = Flag 'T' 'N'

-- | The 'Flag' pattern used for /Tonga/ denoted with /TO/.
pattern TO :: Flag
pattern TO = Flag 'T' 'O'

-- | The 'Flag' pattern used for /Turkey/ denoted with /TR/.
pattern TR :: Flag
pattern TR = Flag 'T' 'R'

-- | The 'Flag' pattern used for /Trinidad & Tobago/ denoted with /TT/.
pattern TT :: Flag
pattern TT = Flag 'T' 'T'

-- | The 'Flag' pattern used for /Tuvalu/ denoted with /TV/.
pattern TV :: Flag
pattern TV = Flag 'T' 'V'

-- | The 'Flag' pattern used for /Taiwan/ denoted with /TW/.
pattern TW :: Flag
pattern TW = Flag 'T' 'W'

-- | The 'Flag' pattern used for /Tanzania/ denoted with /TZ/.
pattern TZ :: Flag
pattern TZ = Flag 'T' 'Z'

-- | The 'Flag' pattern used for /Ukraine/ denoted with /UA/.
pattern UA :: Flag
pattern UA = Flag 'U' 'A'

-- | The 'Flag' pattern used for /Uganda/ denoted with /UG/.
pattern UG :: Flag
pattern UG = Flag 'U' 'G'

-- | The 'Flag' pattern used for the /U.S. Outlying Islands/ denoted with /UM/.
pattern UM :: Flag
pattern UM = Flag 'U' 'M'

-- | The 'Flag' pattern used for the /United Nations/ denoted with /UN/.
pattern UN :: Flag
pattern UN = Flag 'U' 'N'

-- | The 'Flag' pattern used for the /United States/ denoted with /US/.
pattern US :: Flag
pattern US = Flag 'U' 'S'

-- | The 'Flag' pattern used for /Uruguay/ denoted with /UY/.
pattern UY :: Flag
pattern UY = Flag 'U' 'Y'

-- | The 'Flag' pattern used for /Uzbekistan/ denoted with /UZ/.
pattern UZ :: Flag
pattern UZ = Flag 'U' 'Z'

-- | The 'Flag' pattern used for /Vatican City/ denoted with /VA/.
pattern VA :: Flag
pattern VA = Flag 'V' 'A'

-- | The 'Flag' pattern used for /St. Vincent & Grenadines/ denoted with /VC/.
pattern VC :: Flag
pattern VC = Flag 'V' 'C'

-- | The 'Flag' pattern used for /Venezuela/ denoted with /VE/.
pattern VE :: Flag
pattern VE = Flag 'V' 'E'

-- | The 'Flag' pattern used for the /British Virgin Islands/ denoted with /VG/.
pattern VG :: Flag
pattern VG = Flag 'V' 'G'

-- | The 'Flag' pattern used for the /U.S. Virgin Islands/ denoted with /VI/.
pattern VI :: Flag
pattern VI = Flag 'V' 'I'

-- | The 'Flag' pattern used for /Vietnam/ denoted with /VN/.
pattern VN :: Flag
pattern VN = Flag 'V' 'N'

-- | The 'Flag' pattern used for /Vanuatu/ denoted with /VU/.
pattern VU :: Flag
pattern VU = Flag 'V' 'U'

-- | The 'Flag' pattern used for /Wallis & Futuna/ denoted with /WF/.
pattern WF :: Flag
pattern WF = Flag 'W' 'F'

-- | The 'Flag' pattern used for /Samoa/ denoted with /WS/.
pattern WS :: Flag
pattern WS = Flag 'W' 'S'

-- | The 'Flag' pattern used for /Kosovo/ denoted with /XK/.
pattern XK :: Flag
pattern XK = Flag 'X' 'K'

-- | The 'Flag' pattern used for /Yemen/ denoted with /YE/.
pattern YE :: Flag
pattern YE = Flag 'Y' 'E'

-- | The 'Flag' pattern used for /Mayotte/ denoted with /YT/.
pattern YT :: Flag
pattern YT = Flag 'Y' 'T'

-- | The 'Flag' pattern used for /South Africa/ denoted with /ZA/.
pattern ZA :: Flag
pattern ZA = Flag 'Z' 'A'

-- | The 'Flag' pattern used for /Zambia/ denoted with /ZM/.
pattern ZM :: Flag
pattern ZM = Flag 'Z' 'M'

-- | The 'Flag' pattern used for /Zimbabwe/ denoted with /ZW/.
pattern ZW :: Flag
pattern ZW = Flag 'Z' 'W'

-- | Some emoji deal with people. One can change the color of the skin with the
-- 'SkinColorModifier'. For the skin color, the <https://en.wikipedia.org/wiki/Fitzpatrick_scale /Fitzpatrick scale/> is used.
-- A numerical classification system for skin types.
data SkinColorModifier
  = Light  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ one or two to the Emoji.
  | MediumLight  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ three to the Emoji.
  | Medium  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ four to the Emoji.
  | MediumDark  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ five to the Emoji.
  | Dark  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ six to the Emoji.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | A data type to deal with the /zodiac sign/ emoji. The data type lists the
-- different zodiac signs as data constructors, and the instance of the
-- 'UnicodeCharacter' allows to convert it from and to a 'Char'acter.
data Zodiac
  = Aries  -- ^ The /aries/ zodiac sign, /ram/ in English, is denoted as ♈.
  | Taurus  -- ^ The /taurus/ zodiac sign, /bull/ in English, is denoted as ♉.
  | Gemini  -- ^ The /gemini/ zodiac sign, /twins/ in English, is denoted as ♊.
  | Cancer  -- ^ The /cancer/ zodiac sign, /crab/ in English, is denoted as ♋.
  | Leo  -- ^ The /leo/ zodiac sign, /lion/ in English, is denoted as ♌.
  | Virgo  -- ^ The /virgo/ zodiac sign, /maiden/ in English, is denoted as ♍.
  | Libra  -- ^ The /libra/ zodiac sign, /scales/ in English, is denoted as ♎.
  | Scorpio  -- ^ The /scorpio/ zodiac sign, /scorpion/ in English, is denoted as ♏.
  | Sagittarius  -- ^ The /saggitarius/ zodiac sign, /archer/ in English, is denoted as ♐.
  | Capricorn  -- ^ The /capricorn/ zodiac sign, /sea-goat/ in English, is denoted as ♑.
  | Aquarius  -- ^ The /aquarius/ zodiac sign, /water-bearer/ in English, is denoted as ♒.
  | Pisces  -- ^ The /pices/ zodiac sign, /fish/ in English, is denoted as ♓.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | The 'SkinColorModifier' that corresponds to type one of the /Fitzpatrick
-- scale/.
pattern FitzPatrickI :: SkinColorModifier
pattern FitzPatrickI = Light

-- | The 'SkinColorModifier' that corresponds to type two of the /Fitzpatrick
-- scale/.
pattern FitzPatrickII :: SkinColorModifier
pattern FitzPatrickII = Light

-- | The 'SkinColorModifier' that corresponds to type three of the /Fitzpatrick
-- scale/.
pattern FitzPatrickIII :: SkinColorModifier
pattern FitzPatrickIII = MediumLight

-- | The 'SkinColorModifier' that corresponds to type four of the /Fitzpatrick
-- scale/.
pattern FitzPatrickIV :: SkinColorModifier
pattern FitzPatrickIV = Medium

-- | The 'SkinColorModifier' that corresponds to type five of the /Fitzpatrick
-- scale/.
pattern FitzPatrickV :: SkinColorModifier
pattern FitzPatrickV = MediumDark

-- | The 'SkinColorModifier' that corresponds to type six of the /Fitzpatrick
-- scale/.
pattern FitzPatrickVI :: SkinColorModifier
pattern FitzPatrickVI = Dark

-- | Convert the given /Fitzpatrick skin type/ to the corresponding
-- 'SkinColorModifier' wrapped in a 'Just', if no such 'SkinColorModifier'
-- exists, 'Nothing' is returned.
fromFitzPatrick :: Integral i
  => i  -- ^ The given /Fitzpatrick skin type/.
  -> Maybe SkinColorModifier  -- ^ The corresponding 'SkinColorModifier' wrapped in a 'Just'; 'Nothing' if no such modifier exists.
fromFitzPatrick 1 = Just Light
fromFitzPatrick 2 = Just Light
fromFitzPatrick 3 = Just MediumLight
fromFitzPatrick 4 = Just Medium
fromFitzPatrick 5 = Just MediumDark
fromFitzPatrick 6 = Just Dark
fromFitzPatrick _ = Nothing

-- | The English name for the 'Aries' zodiac sign.
pattern Ram :: Zodiac
pattern Ram = Aries

-- | The English name for the 'Taurus' zodiac sign.
pattern Bull :: Zodiac
pattern Bull = Taurus

-- | The English name for the 'Gemini' zodiac sign.
pattern Twins :: Zodiac
pattern Twins = Gemini

-- | The English name for the 'Cancer' zodiac sign.
pattern Crab :: Zodiac
pattern Crab = Cancer

-- | The English name for the 'Leo' zodiac sign.
pattern Lion :: Zodiac
pattern Lion = Leo

-- | The English name for the 'Virgo' zodiac sign.
pattern Maiden :: Zodiac
pattern Maiden = Virgo

-- | The English name for the 'Libra' zodiac sign.
pattern Scales :: Zodiac
pattern Scales = Libra

-- | The name of the constellation of the 'Scorpio' zodiac sign.
pattern Scorpius :: Zodiac
pattern Scorpius = Scorpio

-- | The English name for the 'Scorpio' zodiac sign.
pattern Scorpion :: Zodiac
pattern Scorpion = Scorpio

-- | An English name for the 'Sagittarius' zodiac sign.
pattern Centaur :: Zodiac
pattern Centaur = Sagittarius

-- | An English name for the 'Sagittarius' zodiac sign.
pattern Archer :: Zodiac
pattern Archer = Sagittarius

-- | The name of the constellation of the 'Capricorn' zodiac sign.
pattern Capricornus :: Zodiac
pattern Capricornus = Capricorn

-- | An English name for the 'Capricorn' zodiac sign.
pattern MountainGoat :: Zodiac
pattern MountainGoat = Capricorn

-- | An English name for the 'Capricorn' zodiac sign.
pattern GoatHorned :: Zodiac
pattern GoatHorned = Capricorn

-- | An English name for the 'Capricorn' zodiac sign.
pattern SeaGoat :: Zodiac
pattern SeaGoat = Capricorn

-- | The English name for the 'Aquarius' zodiac sign.
pattern WaterBearer :: Zodiac
pattern WaterBearer = Aquarius

-- | The English name for the 'Pisces' zodiac sign.
pattern Fish :: Zodiac
pattern Fish = Pisces

-- | Convert the given two 'Char'acters of the ISO3166-1 Alpha-2 standard to an
-- Emoji that renders the flag of the corresponding country or terroitory.
-- Deprecated regions, such as SU (Soviet Union) and YU (Yugoslavia) have no
-- flag. The European Union (EU), Antarctica (AQ) and United Nations (UN)
-- are included as marcoregion flags. This function does not check if the
-- two characters map to a valid flag token.
iso3166Alpha2ToFlag'
  :: Char  -- ^ The first 'Char'acter of the ISO3166 Alpha-2 code.
  -> Char  -- ^ The second 'Char'acter of the ISO3166 Alpha-2 code.
  -> Text  -- ^ A 'Text' object that consists of two characters, where the two characters form a flag emoji, if the given flag exists.
iso3166Alpha2ToFlag' ca cb = pack (map (mapFromEnum 0x1f1a5 . toUpper) [ca, cb])

-- | Convert the given two 'Char'acters of the ISO3166-1 Alpha-2 standard to an
-- Emoji that renders the flag of the corresponding country or terroitory
-- wrapped in a 'Just' data constructor. Deprecated regions, such as SU
-- (Soviet Union) and YU (Yugoslavia) have no flag. The European Union (EU),
-- Antarctica (AQ) and United Nations (UN) are included as marcoregion flags.
-- If the flag does not exists, 'Nothing' is returned.
iso3166Alpha2ToFlag
  :: Char  -- ^ The first 'Char'acter of the ISO3166 Alpha-2 code.
  -> Char  -- ^ The second 'Char'acter of the ISO3166 Alpha-2 code.
  -> Maybe Text  -- ^ A 'Text' object that consists of two characters, where the two characters form a flag emoji wrapped in a 'Just', if the given flag exists; 'Nothing' otherwise.
iso3166Alpha2ToFlag ca cb
  | validFlagEmoji ca cb = Just (iso3166Alpha2ToFlag' ca cb)
  | otherwise = Nothing

-- | Check if for the given two 'Char'acters, a flag emoji exists. The two
-- character combinations for which a flag exist are defined in the ISO3166-1
-- Alpha-2 standard where deprecated reagions, such as SU and YU have no flag,
-- and the European Union (EU), Antarctica (AQ), and the United Nations (UN)
-- have a flag.
validFlagEmoji
  :: Char  -- ^ The first 'Char'acter of the ISO3166 Alpha-2 code.
  -> Char  -- ^ The second 'Char'acter of the ISO3166 Alpha-2 code.
  -> Bool  -- ^ 'True' if a flag emoji exists for the given characters; 'False' otherwise.
validFlagEmoji = on _validFlagEmoji toUpper

_validFlagEmoji :: Char -> Char -> Bool
_validFlagEmoji 'A' 'C' = True
_validFlagEmoji 'A' 'D' = True
_validFlagEmoji 'A' 'E' = True
_validFlagEmoji 'A' 'F' = True
_validFlagEmoji 'A' 'G' = True
_validFlagEmoji 'A' 'I' = True
_validFlagEmoji 'A' 'L' = True
_validFlagEmoji 'A' 'M' = True
_validFlagEmoji 'A' 'O' = True
_validFlagEmoji 'A' 'Q' = True
_validFlagEmoji 'A' 'R' = True
_validFlagEmoji 'A' 'S' = True
_validFlagEmoji 'A' 'T' = True
_validFlagEmoji 'A' 'U' = True
_validFlagEmoji 'A' 'W' = True
_validFlagEmoji 'A' 'X' = True
_validFlagEmoji 'A' 'Z' = True
_validFlagEmoji 'B' 'A' = True
_validFlagEmoji 'B' 'B' = True
_validFlagEmoji 'B' 'D' = True
_validFlagEmoji 'B' 'E' = True
_validFlagEmoji 'B' 'F' = True
_validFlagEmoji 'B' 'G' = True
_validFlagEmoji 'B' 'H' = True
_validFlagEmoji 'B' 'I' = True
_validFlagEmoji 'B' 'J' = True
_validFlagEmoji 'B' 'L' = True
_validFlagEmoji 'B' 'M' = True
_validFlagEmoji 'B' 'N' = True
_validFlagEmoji 'B' 'O' = True
_validFlagEmoji 'B' 'Q' = True
_validFlagEmoji 'B' 'R' = True
_validFlagEmoji 'B' 'S' = True
_validFlagEmoji 'B' 'T' = True
_validFlagEmoji 'B' 'V' = True
_validFlagEmoji 'B' 'W' = True
_validFlagEmoji 'B' 'Y' = True
_validFlagEmoji 'B' 'Z' = True
_validFlagEmoji 'C' 'A' = True
_validFlagEmoji 'C' 'C' = True
_validFlagEmoji 'C' 'D' = True
_validFlagEmoji 'C' 'F' = True
_validFlagEmoji 'C' 'G' = True
_validFlagEmoji 'C' 'H' = True
_validFlagEmoji 'C' 'I' = True
_validFlagEmoji 'C' 'K' = True
_validFlagEmoji 'C' 'L' = True
_validFlagEmoji 'C' 'M' = True
_validFlagEmoji 'C' 'N' = True
_validFlagEmoji 'C' 'O' = True
_validFlagEmoji 'C' 'P' = True
_validFlagEmoji 'C' 'R' = True
_validFlagEmoji 'C' 'U' = True
_validFlagEmoji 'C' 'V' = True
_validFlagEmoji 'C' 'W' = True
_validFlagEmoji 'C' 'X' = True
_validFlagEmoji 'C' 'Y' = True
_validFlagEmoji 'C' 'Z' = True
_validFlagEmoji 'D' 'E' = True
_validFlagEmoji 'D' 'G' = True
_validFlagEmoji 'D' 'J' = True
_validFlagEmoji 'D' 'K' = True
_validFlagEmoji 'D' 'M' = True
_validFlagEmoji 'D' 'O' = True
_validFlagEmoji 'D' 'Z' = True
_validFlagEmoji 'E' 'A' = True
_validFlagEmoji 'E' 'C' = True
_validFlagEmoji 'E' 'E' = True
_validFlagEmoji 'E' 'G' = True
_validFlagEmoji 'E' 'H' = True
_validFlagEmoji 'E' 'R' = True
_validFlagEmoji 'E' 'S' = True
_validFlagEmoji 'E' 'T' = True
_validFlagEmoji 'E' 'U' = True
_validFlagEmoji 'F' 'I' = True
_validFlagEmoji 'F' 'J' = True
_validFlagEmoji 'F' 'K' = True
_validFlagEmoji 'F' 'M' = True
_validFlagEmoji 'F' 'O' = True
_validFlagEmoji 'F' 'R' = True
_validFlagEmoji 'G' 'A' = True
_validFlagEmoji 'G' 'B' = True
_validFlagEmoji 'G' 'D' = True
_validFlagEmoji 'G' 'E' = True
_validFlagEmoji 'G' 'F' = True
_validFlagEmoji 'G' 'G' = True
_validFlagEmoji 'G' 'H' = True
_validFlagEmoji 'G' 'I' = True
_validFlagEmoji 'G' 'L' = True
_validFlagEmoji 'G' 'M' = True
_validFlagEmoji 'G' 'N' = True
_validFlagEmoji 'G' 'P' = True
_validFlagEmoji 'G' 'Q' = True
_validFlagEmoji 'G' 'R' = True
_validFlagEmoji 'G' 'S' = True
_validFlagEmoji 'G' 'T' = True
_validFlagEmoji 'G' 'U' = True
_validFlagEmoji 'G' 'W' = True
_validFlagEmoji 'G' 'Y' = True
_validFlagEmoji 'H' 'K' = True
_validFlagEmoji 'H' 'M' = True
_validFlagEmoji 'H' 'N' = True
_validFlagEmoji 'H' 'R' = True
_validFlagEmoji 'H' 'T' = True
_validFlagEmoji 'H' 'U' = True
_validFlagEmoji 'I' 'C' = True
_validFlagEmoji 'I' 'D' = True
_validFlagEmoji 'I' 'E' = True
_validFlagEmoji 'I' 'L' = True
_validFlagEmoji 'I' 'M' = True
_validFlagEmoji 'I' 'N' = True
_validFlagEmoji 'I' 'O' = True
_validFlagEmoji 'I' 'Q' = True
_validFlagEmoji 'I' 'R' = True
_validFlagEmoji 'I' 'S' = True
_validFlagEmoji 'I' 'T' = True
_validFlagEmoji 'J' 'E' = True
_validFlagEmoji 'J' 'M' = True
_validFlagEmoji 'J' 'O' = True
_validFlagEmoji 'J' 'P' = True
_validFlagEmoji 'K' 'E' = True
_validFlagEmoji 'K' 'G' = True
_validFlagEmoji 'K' 'H' = True
_validFlagEmoji 'K' 'I' = True
_validFlagEmoji 'K' 'M' = True
_validFlagEmoji 'K' 'N' = True
_validFlagEmoji 'K' 'P' = True
_validFlagEmoji 'K' 'R' = True
_validFlagEmoji 'K' 'W' = True
_validFlagEmoji 'K' 'Y' = True
_validFlagEmoji 'K' 'Z' = True
_validFlagEmoji 'L' 'A' = True
_validFlagEmoji 'L' 'B' = True
_validFlagEmoji 'L' 'C' = True
_validFlagEmoji 'L' 'I' = True
_validFlagEmoji 'L' 'K' = True
_validFlagEmoji 'L' 'R' = True
_validFlagEmoji 'L' 'S' = True
_validFlagEmoji 'L' 'T' = True
_validFlagEmoji 'L' 'U' = True
_validFlagEmoji 'L' 'V' = True
_validFlagEmoji 'L' 'Y' = True
_validFlagEmoji 'M' 'A' = True
_validFlagEmoji 'M' 'C' = True
_validFlagEmoji 'M' 'D' = True
_validFlagEmoji 'M' 'E' = True
_validFlagEmoji 'M' 'F' = True
_validFlagEmoji 'M' 'G' = True
_validFlagEmoji 'M' 'H' = True
_validFlagEmoji 'M' 'K' = True
_validFlagEmoji 'M' 'L' = True
_validFlagEmoji 'M' 'M' = True
_validFlagEmoji 'M' 'N' = True
_validFlagEmoji 'M' 'O' = True
_validFlagEmoji 'M' 'P' = True
_validFlagEmoji 'M' 'Q' = True
_validFlagEmoji 'M' 'R' = True
_validFlagEmoji 'M' 'S' = True
_validFlagEmoji 'M' 'T' = True
_validFlagEmoji 'M' 'U' = True
_validFlagEmoji 'M' 'V' = True
_validFlagEmoji 'M' 'W' = True
_validFlagEmoji 'M' 'X' = True
_validFlagEmoji 'M' 'Y' = True
_validFlagEmoji 'M' 'Z' = True
_validFlagEmoji 'N' 'A' = True
_validFlagEmoji 'N' 'C' = True
_validFlagEmoji 'N' 'E' = True
_validFlagEmoji 'N' 'F' = True
_validFlagEmoji 'N' 'G' = True
_validFlagEmoji 'N' 'I' = True
_validFlagEmoji 'N' 'L' = True
_validFlagEmoji 'N' 'O' = True
_validFlagEmoji 'N' 'P' = True
_validFlagEmoji 'N' 'R' = True
_validFlagEmoji 'N' 'U' = True
_validFlagEmoji 'N' 'Z' = True
_validFlagEmoji 'O' 'M' = True
_validFlagEmoji 'P' 'A' = True
_validFlagEmoji 'P' 'E' = True
_validFlagEmoji 'P' 'F' = True
_validFlagEmoji 'P' 'G' = True
_validFlagEmoji 'P' 'H' = True
_validFlagEmoji 'P' 'K' = True
_validFlagEmoji 'P' 'L' = True
_validFlagEmoji 'P' 'M' = True
_validFlagEmoji 'P' 'N' = True
_validFlagEmoji 'P' 'R' = True
_validFlagEmoji 'P' 'S' = True
_validFlagEmoji 'P' 'T' = True
_validFlagEmoji 'P' 'W' = True
_validFlagEmoji 'P' 'Y' = True
_validFlagEmoji 'Q' 'A' = True
_validFlagEmoji 'R' 'E' = True
_validFlagEmoji 'R' 'O' = True
_validFlagEmoji 'R' 'S' = True
_validFlagEmoji 'R' 'U' = True
_validFlagEmoji 'R' 'W' = True
_validFlagEmoji 'S' 'A' = True
_validFlagEmoji 'S' 'B' = True
_validFlagEmoji 'S' 'C' = True
_validFlagEmoji 'S' 'D' = True
_validFlagEmoji 'S' 'E' = True
_validFlagEmoji 'S' 'G' = True
_validFlagEmoji 'S' 'H' = True
_validFlagEmoji 'S' 'I' = True
_validFlagEmoji 'S' 'J' = True
_validFlagEmoji 'S' 'K' = True
_validFlagEmoji 'S' 'L' = True
_validFlagEmoji 'S' 'M' = True
_validFlagEmoji 'S' 'N' = True
_validFlagEmoji 'S' 'O' = True
_validFlagEmoji 'S' 'R' = True
_validFlagEmoji 'S' 'S' = True
_validFlagEmoji 'S' 'T' = True
_validFlagEmoji 'S' 'V' = True
_validFlagEmoji 'S' 'X' = True
_validFlagEmoji 'S' 'Y' = True
_validFlagEmoji 'S' 'Z' = True
_validFlagEmoji 'T' 'A' = True
_validFlagEmoji 'T' 'C' = True
_validFlagEmoji 'T' 'D' = True
_validFlagEmoji 'T' 'F' = True
_validFlagEmoji 'T' 'G' = True
_validFlagEmoji 'T' 'H' = True
_validFlagEmoji 'T' 'J' = True
_validFlagEmoji 'T' 'K' = True
_validFlagEmoji 'T' 'L' = True
_validFlagEmoji 'T' 'M' = True
_validFlagEmoji 'T' 'N' = True
_validFlagEmoji 'T' 'O' = True
_validFlagEmoji 'T' 'R' = True
_validFlagEmoji 'T' 'T' = True
_validFlagEmoji 'T' 'V' = True
_validFlagEmoji 'T' 'W' = True
_validFlagEmoji 'T' 'Z' = True
_validFlagEmoji 'U' 'A' = True
_validFlagEmoji 'U' 'G' = True
_validFlagEmoji 'U' 'M' = True
_validFlagEmoji 'U' 'N' = True
_validFlagEmoji 'U' 'S' = True
_validFlagEmoji 'U' 'Y' = True
_validFlagEmoji 'U' 'Z' = True
_validFlagEmoji 'V' 'A' = True
_validFlagEmoji 'V' 'C' = True
_validFlagEmoji 'V' 'E' = True
_validFlagEmoji 'V' 'G' = True
_validFlagEmoji 'V' 'I' = True
_validFlagEmoji 'V' 'N' = True
_validFlagEmoji 'V' 'U' = True
_validFlagEmoji 'W' 'F' = True
_validFlagEmoji 'W' 'S' = True
_validFlagEmoji 'X' 'K' = True
_validFlagEmoji 'Y' 'E' = True
_validFlagEmoji 'Y' 'T' = True
_validFlagEmoji 'Z' 'A' = True
_validFlagEmoji 'Z' 'M' = True
_validFlagEmoji 'Z' 'W' = True
_validFlagEmoji _ _ = False

instance Arbitrary SkinColorModifier where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Zodiac where
    arbitrary = arbitraryBoundedEnum

instance UnicodeCharacter SkinColorModifier where
    toUnicodeChar = mapFromEnum _skinColorOffset
    fromUnicodeChar = mapToEnumSafe _skinColorOffset
    fromUnicodeChar' = mapToEnum _skinColorOffset

instance UnicodeCharacter Zodiac where
    toUnicodeChar = mapFromEnum _zodiacOffset
    fromUnicodeChar = mapToEnumSafe _zodiacOffset
    fromUnicodeChar' = mapToEnum _zodiacOffset
