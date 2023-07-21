{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Emoji.Core
-- Description : A module that defines the /flag/ Emoji.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A module that defines the /flag/ emoji. There are basically three types of flags:
-- flags for nations, flags for subnations (like England, Scotland and Wales); and
-- 'ExtraFlag's that contain a list of flags used for occassions and political purposes.
module Data.Char.Emoji.Flag
  ( -- * Flag emoji
    Flag,
    flag,
    flag',
    flagChars,
    iso3166Alpha2ToFlag,
    iso3166Alpha2ToFlag',
    validFlagEmoji,

    -- * Subregional flag emoji
    SubFlag,
    ExtraFlag (ChequeredFlag, TriangularFlagOnPost, CrossedFlags, BlackFlag, WavingWhiteFlag, RainbowFlag, TransgenderFlag, PirateFlag),

    -- * Pattern symbols for 'Flag's
    pattern AC,
    pattern AD,
    pattern AE,
    pattern AF,
    pattern AG,
    pattern AI,
    pattern AL,
    pattern AM,
    pattern AO,
    pattern AQ,
    pattern AR,
    pattern AS,
    pattern AT,
    pattern AU,
    pattern AW,
    pattern AX,
    pattern AZ,
    pattern BA,
    pattern BB,
    pattern BD,
    pattern BE,
    pattern BF,
    pattern BG,
    pattern BH,
    pattern BI,
    pattern BJ,
    pattern BL,
    pattern BM,
    pattern BN,
    pattern BO,
    pattern BQ,
    pattern BR,
    pattern BS,
    pattern BT,
    pattern BV,
    pattern BW,
    pattern BY,
    pattern BZ,
    pattern CA,
    pattern CC,
    pattern CD,
    pattern CF,
    pattern CG,
    pattern CH,
    pattern CI,
    pattern CK,
    pattern CL,
    pattern CM,
    pattern CN,
    pattern CO,
    pattern CP,
    pattern CR,
    pattern CU,
    pattern CV,
    pattern CW,
    pattern CX,
    pattern CY,
    pattern CZ,
    pattern DE,
    pattern DG,
    pattern DJ,
    pattern DK,
    pattern DM,
    pattern DO,
    pattern DZ,
    pattern EA,
    pattern EC,
    pattern EE,
    pattern EG,
    pattern EH,
    pattern ER,
    pattern ES,
    pattern ET,
    pattern EU,
    pattern FI,
    pattern FJ,
    pattern FK,
    pattern FM,
    pattern FO,
    pattern FR,
    pattern GA,
    pattern GB,
    pattern GD,
    pattern GE,
    pattern GF,
    pattern GG,
    pattern GH,
    pattern GI,
    pattern GL,
    pattern GM,
    pattern GN,
    pattern GP,
    pattern GQ,
    pattern GR,
    pattern GS,
    pattern GT,
    pattern GU,
    pattern GW,
    pattern GY,
    pattern HK,
    pattern HM,
    pattern HN,
    pattern HR,
    pattern HT,
    pattern HU,
    pattern IC,
    pattern ID,
    pattern IE,
    pattern IL,
    pattern IM,
    pattern IN,
    pattern IO,
    pattern IQ,
    pattern IR,
    pattern IS,
    pattern IT,
    pattern JE,
    pattern JM,
    pattern JO,
    pattern JP,
    pattern KE,
    pattern KG,
    pattern KH,
    pattern KI,
    pattern KM,
    pattern KN,
    pattern KP,
    pattern KR,
    pattern KW,
    pattern KY,
    pattern KZ,
    pattern LA,
    pattern LB,
    pattern LC,
    pattern LI,
    pattern LK,
    pattern LR,
    pattern LS,
    pattern LT,
    pattern LU,
    pattern LV,
    pattern LY,
    pattern MA,
    pattern MC,
    pattern MD,
    pattern ME,
    pattern MF,
    pattern MG,
    pattern MH,
    pattern MK,
    pattern ML,
    pattern MM,
    pattern MN,
    pattern MO,
    pattern MP,
    pattern MQ,
    pattern MR,
    pattern MS,
    pattern MT,
    pattern MU,
    pattern MV,
    pattern MW,
    pattern MX,
    pattern MY,
    pattern MZ,
    pattern NA,
    pattern NC,
    pattern NE,
    pattern NF,
    pattern NG,
    pattern NI,
    pattern NL,
    pattern NO,
    pattern NP,
    pattern NR,
    pattern NU,
    pattern NZ,
    pattern OM,
    pattern PA,
    pattern PE,
    pattern PF,
    pattern PG,
    pattern PH,
    pattern PK,
    pattern PL,
    pattern PM,
    pattern PN,
    pattern PR,
    pattern PS,
    pattern PT,
    pattern PW,
    pattern PY,
    pattern QA,
    pattern RE,
    pattern RO,
    pattern RS,
    pattern RU,
    pattern RW,
    pattern SA,
    pattern SB,
    pattern SC,
    pattern SD,
    pattern SE,
    pattern SG,
    pattern SH,
    pattern SI,
    pattern SJ,
    pattern SK,
    pattern SL,
    pattern SM,
    pattern SN,
    pattern SO,
    pattern SR,
    pattern SS,
    pattern ST,
    pattern SV,
    pattern SX,
    pattern SY,
    pattern SZ,
    pattern TA,
    pattern TC,
    pattern TD,
    pattern TF,
    pattern TG,
    pattern TH,
    pattern TJ,
    pattern TK,
    pattern TL,
    pattern TM,
    pattern TN,
    pattern TO,
    pattern TR,
    pattern TT,
    pattern TV,
    pattern TW,
    pattern TZ,
    pattern UA,
    pattern UG,
    pattern UM,
    pattern UN,
    pattern US,
    pattern UY,
    pattern UZ,
    pattern VA,
    pattern VC,
    pattern VE,
    pattern VG,
    pattern VI,
    pattern VN,
    pattern VU,
    pattern WF,
    pattern WS,
    pattern XK,
    pattern YE,
    pattern YT,
    pattern ZA,
    pattern ZM,
    pattern ZW,

    -- * Pattern synonyms for 'SubFlag's
    pattern ENG,
    pattern SCT,
    pattern WLS,
    pattern USAL,
    pattern USAK,
    pattern USAS,
    pattern USAZ,
    pattern USAR,
    pattern USCA,
    pattern USCO,
    pattern USCT,
    pattern USDE,
    pattern USFL,
    pattern USGA,
    pattern USGU,
    pattern USHI,
    pattern USID,
    pattern USIL,
    pattern USIN,
    pattern USIA,
    pattern USKS,
    pattern USKY,
    pattern USLA,
    pattern USME,
    pattern USMD,
    pattern USMA,
    pattern USMI,
    pattern USMN,
    pattern USMS,
    pattern USMO,
    pattern USMT,
    pattern USNE,
    pattern USNV,
    pattern USNH,
    pattern USNJ,
    pattern USNM,
    pattern USNY,
    pattern USNC,
    pattern USND,
    pattern USMP,
    pattern USOH,
    pattern USOK,
    pattern USOR,
    pattern USPA,
    pattern USPR,
    pattern USRI,
    pattern USSC,
    pattern USSD,
    pattern USTN,
    pattern USUM,
    pattern USVI,
    pattern USUT,
    pattern USVT,
    pattern USVA,
    pattern USWA,
    pattern USDC,
    pattern USWV,
    pattern USWI,
    pattern USWY,
  )
where

import Control.DeepSeq (NFData)
import Data.Bits ((.|.))
import Data.Char (chr, ord, toLower, toUpper)
import Data.Char.Core (UnicodeText (fromUnicodeText, isInTextRange, toUnicodeText), mapToEnumSafe)
import Data.Char.Enclosed (regionalIndicatorUppercase')
import Data.Data (Data)
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.Text (Text, pack, unpack)
import GHC.Enum (fromEnumError, toEnumError)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary), arbitraryBoundedEnum)
import Test.QuickCheck.Gen (elements)
import Prelude hiding (GT, LT)

_flagCharOffset :: Int
_flagCharOffset = 0x1f1a5

-- | A data type that stores a (country) flag by the two characters of the ISO
-- 3166 Alpa-2 standard. The data constructor is hidden to prevent making flags
-- with a combination of characters that is invalid.
-- Besides the countries defined in the ISO-3166 Alpha-2 standard, only the
-- Antarctica (AQ), the European Union (EU) and the United Nations (UN) have a
-- flag. Deprecated territories like the Soviet Union (SU) and Yugoslavia (YU)
-- have no corresponding flag.
data Flag = Flag Char Char deriving (Data, Eq, Generic, Ord, Read, Show)

instance Hashable Flag

instance NFData Flag

-- | A data type to store a subregion flag. This is specified by the /parent/
-- flag, and three characters of the subregion. At the moment, the only three
-- subregional flags are /England/ (eng), /Scotland/ (sct) and /Wales/ (wls),
-- all beloning under the /United Kingdom/ flag (GB).
-- The data constructor is made private to prevent making non-existing subflags.
data SubFlag = SubFlag Flag Char Char (Maybe Char) deriving (Data, Eq, Generic, Ord, Read, Show)

instance Bounded Flag where
  minBound = AC
  maxBound = ZW

instance Hashable SubFlag

instance NFData SubFlag

-- | Convert the given two characters that represent a flag according to the ISO
-- 3166 Alpha-2 standard to a 'Flag' wrapped in a 'Just' data constructor, if
-- that flag exists; 'Nothing' otherwise.
-- One can pass characters in upper case (@A-Z@) and lower case (@a-z@). The
-- flag will hold the upper case variant.
-- The Emoji have flags for the countries defined by the ISO 3166 Alpha-2
-- standard without deprecated regions like the Soviet Union (SU) and Yugoslavia
-- (YU). Furthermore there are Emoji for the flags of Antarctica (AQ), the
-- European Union (EU) and the United Nations (UN).
flag ::
  -- | The first character of the ISO 3166 Alpha-2 standard.
  Char ->
  -- | The second character of the ISO 3166 Alpha-2 standard.
  Char ->
  -- | A 'Flag' object wrapped in a 'Just' data constructor, given such flag exists; 'Nothing' otherwise.
  Maybe Flag
flag ca cb
  | _validFlagEmoji a b = Just (Flag a b)
  | otherwise = Nothing
  where
    a = toUpper ca
    b = toUpper cb

-- | Convert the given two characters that represent a flag according to the ISO
-- 3166 Alpha-2 standard to a 'Flag'. If the flag does not exists, then the
-- result is unspecified.
-- One can pass characters in upper case (@A-Z@) and lower case (@a-z@). The
-- flag will hold the upper case variant.
-- The Emoji have flags for the countries defined by the ISO 3166 Alpha-2
-- standard without deprecated regions like the Soviet Union (SU) and Yugoslavia
-- (YU). Furthermore there are Emoji for the flags of Antarctica (AQ), the
-- European Union (EU) and the United Nations (UN).
flag' ::
  -- | The first character of the ISO 3166 Alpha-2 standard.
  Char ->
  -- | The second character of the ISO 3166 Alpha-2 standard.
  Char ->
  -- | The equivalent 'Flag' object.
  Flag
flag' ca = fromJust . flag ca

-- | Obtain the two-characters that specify the given 'Flag'. These two
-- characters are always upper case (@A-Z@).
flagChars ::
  -- | The given 'Flag' to convert to a 2-tuple of 'Char'acters.
  Flag ->
  -- | A 2-tuple that contains upper case 'Char'acters for the given 'Flag'.
  (Char, Char)
flagChars (Flag ca cb) = (ca, cb)

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

pattern GBSubFlag :: Char -> Char -> Char -> SubFlag
pattern GBSubFlag a b c = SubFlag GB a b (Just c)

pattern USSubFlag :: Char -> Char -> SubFlag
pattern USSubFlag a b = SubFlag US a b Nothing

-- | The 'SubFlag' pattern used for /England/ denoted with /GB-ENG/ or /ENG/.
pattern ENG :: SubFlag
pattern ENG = GBSubFlag 'e' 'n' 'g'

-- | The 'SubFlag' pattern used for /Scotland/ denoted with /GB-SCT/ or /SCT/.
pattern SCT :: SubFlag
pattern SCT = GBSubFlag 's' 'c' 't'

-- | The 'SubFlag' pattern used for /Wales/ denoted with /GB-WLS/ or /WLS/.
pattern WLS :: SubFlag
pattern WLS = GBSubFlag 'w' 'l' 's'

-- | The 'SubFlag' pattern used for /Alabama/ denoted with /US-AL/ or /AL/
pattern USAL :: SubFlag
pattern USAL = USSubFlag 'a' 'l'

-- | The 'SubFlag' pattern used for /Alaska/ denoted with /US-AK/ or /AK/
pattern USAK :: SubFlag
pattern USAK = USSubFlag 'a' 'k'

-- | The 'SubFlag' pattern used for /American Samoa/ denoted with /US-AS/ or /AS/
pattern USAS :: SubFlag
pattern USAS = USSubFlag 'a' 's'

-- | The 'SubFlag' pattern used for /Arizona/ denoted with /US-AZ/ or /AZ/
pattern USAZ :: SubFlag
pattern USAZ = USSubFlag 'a' 'z'

-- | The 'SubFlag' pattern used for /Arkansas/ denoted with /US-AR/ or /AR/
pattern USAR :: SubFlag
pattern USAR = USSubFlag 'a' 'r'

-- | The 'SubFlag' pattern used for /California/ denoted with /US-CA/ or /CA/
pattern USCA :: SubFlag
pattern USCA = USSubFlag 'c' 'a'

-- | The 'SubFlag' pattern used for /Colorado/ denoted with /US-CO/ or /CO/
pattern USCO :: SubFlag
pattern USCO = USSubFlag 'c' 'o'

-- | The 'SubFlag' pattern used for /Connecticut/ denoted with /US-CT/ or /CT/
pattern USCT :: SubFlag
pattern USCT = USSubFlag 'c' 't'

-- | The 'SubFlag' pattern used for /Delaware/ denoted with /US-DE/ or /DE/
pattern USDE :: SubFlag
pattern USDE = USSubFlag 'd' 'e'

-- | The 'SubFlag' pattern used for /Florida/ denoted with /US-FL/ or /FL/
pattern USFL :: SubFlag
pattern USFL = USSubFlag 'f' 'l'

-- | The 'SubFlag' pattern used for /Georgia/ denoted with /US-GA/ or /GA/
pattern USGA :: SubFlag
pattern USGA = USSubFlag 'g' 'a'

-- | The 'SubFlag' pattern used for /Guam/ denoted with /US-GU/ or /GU/
pattern USGU :: SubFlag
pattern USGU = USSubFlag 'g' 'u'

-- | The 'SubFlag' pattern used for /Hawaii/ denoted with /US-HI/ or /HI/
pattern USHI :: SubFlag
pattern USHI = USSubFlag 'h' 'i'

-- | The 'SubFlag' pattern used for /Idaho/ denoted with /US-ID/ or /ID/
pattern USID :: SubFlag
pattern USID = USSubFlag 'i' 'd'

-- | The 'SubFlag' pattern used for /Illinois/ denoted with /US-IL/ or /IL/
pattern USIL :: SubFlag
pattern USIL = USSubFlag 'i' 'l'

-- | The 'SubFlag' pattern used for /Indiana/ denoted with /US-IN/ or /IN/
pattern USIN :: SubFlag
pattern USIN = USSubFlag 'i' 'n'

-- | The 'SubFlag' pattern used for /Iowa/ denoted with /US-IA/ or /IA/
pattern USIA :: SubFlag
pattern USIA = USSubFlag 'i' 'a'

-- | The 'SubFlag' pattern used for /Kansas/ denoted with /US-KS/ or /KS/
pattern USKS :: SubFlag
pattern USKS = USSubFlag 'k' 's'

-- | The 'SubFlag' pattern used for /Kentucky/ denoted with /US-KY/ or /KY/
pattern USKY :: SubFlag
pattern USKY = USSubFlag 'k' 'y'

-- | The 'SubFlag' pattern used for /Louisiana/ denoted with /US-LA/ or /LA/
pattern USLA :: SubFlag
pattern USLA = USSubFlag 'l' 'a'

-- | The 'SubFlag' pattern used for /Maine/ denoted with /US-ME/ or /ME/
pattern USME :: SubFlag
pattern USME = USSubFlag 'm' 'e'

-- | The 'SubFlag' pattern used for /Maryland/ denoted with /US-MD/ or /MD/
pattern USMD :: SubFlag
pattern USMD = USSubFlag 'm' 'd'

-- | The 'SubFlag' pattern used for /Massachusetts/ denoted with /US-MA/ or /MA/
pattern USMA :: SubFlag
pattern USMA = USSubFlag 'm' 'a'

-- | The 'SubFlag' pattern used for /Michigan/ denoted with /US-MI/ or /MI/
pattern USMI :: SubFlag
pattern USMI = USSubFlag 'm' 'i'

-- | The 'SubFlag' pattern used for /Minnesota/ denoted with /US-MN/ or /MN/
pattern USMN :: SubFlag
pattern USMN = USSubFlag 'm' 'n'

-- | The 'SubFlag' pattern used for /Mississippi/ denoted with /US-MS/ or /MS/
pattern USMS :: SubFlag
pattern USMS = USSubFlag 'm' 's'

-- | The 'SubFlag' pattern used for /Missouri/ denoted with /US-MO/ or /MO/
pattern USMO :: SubFlag
pattern USMO = USSubFlag 'm' 'o'

-- | The 'SubFlag' pattern used for /Montana/ denoted with /US-MT/ or /MT/
pattern USMT :: SubFlag
pattern USMT = USSubFlag 'm' 't'

-- | The 'SubFlag' pattern used for /Nebraska/ denoted with /US-NE/ or /NE/
pattern USNE :: SubFlag
pattern USNE = USSubFlag 'n' 'e'

-- | The 'SubFlag' pattern used for /Nevada/ denoted with /US-NV/ or /NV/
pattern USNV :: SubFlag
pattern USNV = USSubFlag 'n' 'v'

-- | The 'SubFlag' pattern used for /New Hampshire/ denoted with /US-NH/ or /NH/
pattern USNH :: SubFlag
pattern USNH = USSubFlag 'n' 'h'

-- | The 'SubFlag' pattern used for /New Jersey/ denoted with /US-NJ/ or /NJ/
pattern USNJ :: SubFlag
pattern USNJ = USSubFlag 'n' 'j'

-- | The 'SubFlag' pattern used for /New Mexico/ denoted with /US-NM/ or /NM/
pattern USNM :: SubFlag
pattern USNM = USSubFlag 'n' 'm'

-- | The 'SubFlag' pattern used for /New York/ denoted with /US-NY/ or /NY/
pattern USNY :: SubFlag
pattern USNY = USSubFlag 'n' 'y'

-- | The 'SubFlag' pattern used for /North Carolina/ denoted with /US-NC/ or /NC/
pattern USNC :: SubFlag
pattern USNC = USSubFlag 'n' 'c'

-- | The 'SubFlag' pattern used for /North Dakota/ denoted with /US-ND/ or /ND/
pattern USND :: SubFlag
pattern USND = USSubFlag 'n' 'd'

-- | The 'SubFlag' pattern used for /Northern Mariana Islands/ denoted with /US-MP/ or /MP/
pattern USMP :: SubFlag
pattern USMP = USSubFlag 'm' 'p'

-- | The 'SubFlag' pattern used for /Ohio/ denoted with /US-OH/ or /OH/
pattern USOH :: SubFlag
pattern USOH = USSubFlag 'o' 'h'

-- | The 'SubFlag' pattern used for /Oklahoma/ denoted with /US-OK/ or /OK/
pattern USOK :: SubFlag
pattern USOK = USSubFlag 'o' 'k'

-- | The 'SubFlag' pattern used for /Oregon/ denoted with /US-OR/ or /OR/
pattern USOR :: SubFlag
pattern USOR = USSubFlag 'o' 'r'

-- | The 'SubFlag' pattern used for /Pennsylvania/ denoted with /US-PA/ or /PA/
pattern USPA :: SubFlag
pattern USPA = USSubFlag 'p' 'a'

-- | The 'SubFlag' pattern used for /Puerto Rico/ denoted with /US-PR/ or /PR/
pattern USPR :: SubFlag
pattern USPR = USSubFlag 'p' 'r'

-- | The 'SubFlag' pattern used for /Rhode Island/ denoted with /US-RI/ or /RI/
pattern USRI :: SubFlag
pattern USRI = USSubFlag 'r' 'i'

-- | The 'SubFlag' pattern used for /South Carolina/ denoted with /US-SC/ or /SC/
pattern USSC :: SubFlag
pattern USSC = USSubFlag 's' 'c'

-- | The 'SubFlag' pattern used for /South Dakota/ denoted with /US-SD/ or /SD/
pattern USSD :: SubFlag
pattern USSD = USSubFlag 's' 'd'

-- | The 'SubFlag' pattern used for /Tennessee/ denoted with /US-TN/ or /TN/
pattern USTN :: SubFlag
pattern USTN = USSubFlag 't' 'n'

-- | The 'SubFlag' pattern used for /U.S. Outlying Islands/ denoted with /US-UM/ or /UM/
pattern USUM :: SubFlag
pattern USUM = USSubFlag 'u' 'm'

-- | The 'SubFlag' pattern used for /U.S. Virgin Islands/ denoted with /US-VI/ or /VI/
pattern USVI :: SubFlag
pattern USVI = USSubFlag 'v' 'i'

-- | The 'SubFlag' pattern used for /Utah/ denoted with /US-UT/ or /UT/
pattern USUT :: SubFlag
pattern USUT = USSubFlag 'u' 't'

-- | The 'SubFlag' pattern used for /Vermont/ denoted with /US-VT/ or /VT/
pattern USVT :: SubFlag
pattern USVT = USSubFlag 'v' 't'

-- | The 'SubFlag' pattern used for /Virginia/ denoted with /US-VA/ or /VA/
pattern USVA :: SubFlag
pattern USVA = USSubFlag 'v' 'a'

-- | The 'SubFlag' pattern used for /Washington/ denoted with /US-WA/ or /WA/
pattern USWA :: SubFlag
pattern USWA = USSubFlag 'w' 'a'

-- | The 'SubFlag' pattern used for /Washington DC/ denoted with /US-DC/ or /DC/
pattern USDC :: SubFlag
pattern USDC = USSubFlag 'd' 'c'

-- | The 'SubFlag' pattern used for /West Virginia/ denoted with /US-WV/ or /WV/
pattern USWV :: SubFlag
pattern USWV = USSubFlag 'w' 'v'

-- | The 'SubFlag' pattern used for /Wisconsin/ denoted with /US-WI/ or /WI/
pattern USWI :: SubFlag
pattern USWI = USSubFlag 'w' 'i'

-- | The 'SubFlag' pattern used for /Wyoming/ denoted with /US-WY/ or /WY/
pattern USWY :: SubFlag
pattern USWY = USSubFlag 'w' 'y'

instance Bounded SubFlag where
  minBound = head _subflags
  maxBound = last _subflags

-- | Convert the given two 'Char'acters of the ISO3166-1 Alpha-2 standard to an
-- Emoji that renders the flag of the corresponding country or terroitory.
-- Deprecated regions, such as SU (Soviet Union) and YU (Yugoslavia) have no
-- flag. The European Union (EU), Antarctica (AQ) and United Nations (UN)
-- are included as marcoregion flags. This function does not check if the
-- two characters map to a valid flag token.
iso3166Alpha2ToFlag' ::
  -- | The first 'Char'acter of the ISO3166 Alpha-2 code.
  Char ->
  -- | The second 'Char'acter of the ISO3166 Alpha-2 code.
  Char ->
  -- | A 'Text' object that consists of two characters, where the two characters form a flag emoji, if the given flag exists.
  Text
iso3166Alpha2ToFlag' ca cb = pack (map (regionalIndicatorUppercase' . toUpper) [ca, cb])

-- | Convert the given two 'Char'acters of the ISO3166-1 Alpha-2 standard to an
-- Emoji that renders the flag of the corresponding country or terroitory
-- wrapped in a 'Just' data constructor. Deprecated regions, such as SU
-- (Soviet Union) and YU (Yugoslavia) have no flag. The European Union (EU),
-- Antarctica (AQ) and United Nations (UN) are included as marcoregion flags.
-- If the flag does not exists, 'Nothing' is returned.
iso3166Alpha2ToFlag ::
  -- | The first 'Char'acter of the ISO3166 Alpha-2 code.
  Char ->
  -- | The second 'Char'acter of the ISO3166 Alpha-2 code.
  Char ->
  -- | A 'Text' object that consists of two characters, where the two characters form a flag emoji wrapped in a 'Just', if the given flag exists; 'Nothing' otherwise.
  Maybe Text
iso3166Alpha2ToFlag ca cb
  | validFlagEmoji ca cb = Just (iso3166Alpha2ToFlag' ca cb)
  | otherwise = Nothing

-- | Convert the given 'Text' object to its equivalent 'Flag' object wrapped in
-- a 'Just' data constructor if it exists; 'Nothing' otherwise.
fromFlag :: Text -> Maybe Flag
fromFlag t
  | [a', b'] <- unpack t, Just a <- shft a', Just b <- shft b', _validFlagEmoji a b = Just (Flag a b)
  | otherwise = Nothing
  where
    shft = mapToEnumSafe _flagCharOffset

-- | Check if for the given two 'Char'acters, a flag emoji exists. The two
-- character combinations for which a flag exist are defined in the ISO3166-1
-- Alpha-2 standard where deprecated reagions, such as SU and YU have no flag,
-- and the European Union (EU), Antarctica (AQ), and the United Nations (UN)
-- have a flag. The characters can be upper case (@A-Z@) or lower case (@a-z@).
validFlagEmoji ::
  -- | The first 'Char'acter of the ISO3166 Alpha-2 code.
  Char ->
  -- | The second 'Char'acter of the ISO3166 Alpha-2 code.
  Char ->
  -- | 'True' if a flag emoji exists for the given characters; 'False' otherwise.
  Bool
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

instance Arbitrary Flag where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SubFlag where
  arbitrary = elements _subflags

instance Enum Flag where
  fromEnum AC = 0
  fromEnum AD = 1
  fromEnum AE = 2
  fromEnum AF = 3
  fromEnum AG = 4
  fromEnum AI = 5
  fromEnum AL = 6
  fromEnum AM = 7
  fromEnum AO = 8
  fromEnum AQ = 9
  fromEnum AR = 10
  fromEnum AS = 11
  fromEnum AT = 12
  fromEnum AU = 13
  fromEnum AW = 14
  fromEnum AX = 15
  fromEnum AZ = 16
  fromEnum BA = 17
  fromEnum BB = 18
  fromEnum BD = 19
  fromEnum BE = 20
  fromEnum BF = 21
  fromEnum BG = 22
  fromEnum BH = 23
  fromEnum BI = 24
  fromEnum BJ = 25
  fromEnum BL = 26
  fromEnum BM = 27
  fromEnum BN = 28
  fromEnum BO = 29
  fromEnum BQ = 30
  fromEnum BR = 31
  fromEnum BS = 32
  fromEnum BT = 33
  fromEnum BV = 34
  fromEnum BW = 35
  fromEnum BY = 36
  fromEnum BZ = 37
  fromEnum CA = 38
  fromEnum CC = 39
  fromEnum CD = 40
  fromEnum CF = 41
  fromEnum CG = 42
  fromEnum CH = 43
  fromEnum CI = 44
  fromEnum CK = 45
  fromEnum CL = 46
  fromEnum CM = 47
  fromEnum CN = 48
  fromEnum CO = 49
  fromEnum CP = 50
  fromEnum CR = 51
  fromEnum CU = 52
  fromEnum CV = 53
  fromEnum CW = 54
  fromEnum CX = 55
  fromEnum CY = 56
  fromEnum CZ = 57
  fromEnum DE = 58
  fromEnum DG = 59
  fromEnum DJ = 60
  fromEnum DK = 61
  fromEnum DM = 62
  fromEnum DO = 63
  fromEnum DZ = 64
  fromEnum EA = 65
  fromEnum EC = 66
  fromEnum EE = 67
  fromEnum EG = 68
  fromEnum EH = 69
  fromEnum ER = 70
  fromEnum ES = 71
  fromEnum ET = 72
  fromEnum EU = 73
  fromEnum FI = 74
  fromEnum FJ = 75
  fromEnum FK = 76
  fromEnum FM = 77
  fromEnum FO = 78
  fromEnum FR = 79
  fromEnum GA = 80
  fromEnum GB = 81
  fromEnum GD = 82
  fromEnum GE = 83
  fromEnum GF = 84
  fromEnum GG = 85
  fromEnum GH = 86
  fromEnum GI = 87
  fromEnum GL = 88
  fromEnum GM = 89
  fromEnum GN = 90
  fromEnum GP = 91
  fromEnum GQ = 92
  fromEnum GR = 93
  fromEnum GS = 94
  fromEnum GT = 95
  fromEnum GU = 96
  fromEnum GW = 97
  fromEnum GY = 98
  fromEnum HK = 99
  fromEnum HM = 100
  fromEnum HN = 101
  fromEnum HR = 102
  fromEnum HT = 103
  fromEnum HU = 104
  fromEnum IC = 105
  fromEnum ID = 106
  fromEnum IE = 107
  fromEnum IL = 108
  fromEnum IM = 109
  fromEnum IN = 110
  fromEnum IO = 111
  fromEnum IQ = 112
  fromEnum IR = 113
  fromEnum IS = 114
  fromEnum IT = 115
  fromEnum JE = 116
  fromEnum JM = 117
  fromEnum JO = 118
  fromEnum JP = 119
  fromEnum KE = 120
  fromEnum KG = 121
  fromEnum KH = 122
  fromEnum KI = 123
  fromEnum KM = 124
  fromEnum KN = 125
  fromEnum KP = 126
  fromEnum KR = 127
  fromEnum KW = 128
  fromEnum KY = 129
  fromEnum KZ = 130
  fromEnum LA = 131
  fromEnum LB = 132
  fromEnum LC = 133
  fromEnum LI = 134
  fromEnum LK = 135
  fromEnum LR = 136
  fromEnum LS = 137
  fromEnum LT = 138
  fromEnum LU = 139
  fromEnum LV = 140
  fromEnum LY = 141
  fromEnum MA = 142
  fromEnum MC = 143
  fromEnum MD = 144
  fromEnum ME = 145
  fromEnum MF = 146
  fromEnum MG = 147
  fromEnum MH = 148
  fromEnum MK = 149
  fromEnum ML = 150
  fromEnum MM = 151
  fromEnum MN = 152
  fromEnum MO = 153
  fromEnum MP = 154
  fromEnum MQ = 155
  fromEnum MR = 156
  fromEnum MS = 157
  fromEnum MT = 158
  fromEnum MU = 159
  fromEnum MV = 160
  fromEnum MW = 161
  fromEnum MX = 162
  fromEnum MY = 163
  fromEnum MZ = 164
  fromEnum NA = 165
  fromEnum NC = 166
  fromEnum NE = 167
  fromEnum NF = 168
  fromEnum NG = 169
  fromEnum NI = 170
  fromEnum NL = 171
  fromEnum NO = 172
  fromEnum NP = 173
  fromEnum NR = 174
  fromEnum NU = 175
  fromEnum NZ = 176
  fromEnum OM = 177
  fromEnum PA = 178
  fromEnum PE = 179
  fromEnum PF = 180
  fromEnum PG = 181
  fromEnum PH = 182
  fromEnum PK = 183
  fromEnum PL = 184
  fromEnum PM = 185
  fromEnum PN = 186
  fromEnum PR = 187
  fromEnum PS = 188
  fromEnum PT = 189
  fromEnum PW = 190
  fromEnum PY = 191
  fromEnum QA = 192
  fromEnum RE = 193
  fromEnum RO = 194
  fromEnum RS = 195
  fromEnum RU = 196
  fromEnum RW = 197
  fromEnum SA = 198
  fromEnum SB = 199
  fromEnum SC = 200
  fromEnum SD = 201
  fromEnum SE = 202
  fromEnum SG = 203
  fromEnum SH = 204
  fromEnum SI = 205
  fromEnum SJ = 206
  fromEnum SK = 207
  fromEnum SL = 208
  fromEnum SM = 209
  fromEnum SN = 210
  fromEnum SO = 211
  fromEnum SR = 212
  fromEnum SS = 213
  fromEnum ST = 214
  fromEnum SV = 215
  fromEnum SX = 216
  fromEnum SY = 217
  fromEnum SZ = 218
  fromEnum TA = 219
  fromEnum TC = 220
  fromEnum TD = 221
  fromEnum TF = 222
  fromEnum TG = 223
  fromEnum TH = 224
  fromEnum TJ = 225
  fromEnum TK = 226
  fromEnum TL = 227
  fromEnum TM = 228
  fromEnum TN = 229
  fromEnum TO = 230
  fromEnum TR = 231
  fromEnum TT = 232
  fromEnum TV = 233
  fromEnum TW = 234
  fromEnum TZ = 235
  fromEnum UA = 236
  fromEnum UG = 237
  fromEnum UM = 238
  fromEnum UN = 239
  fromEnum US = 240
  fromEnum UY = 241
  fromEnum UZ = 242
  fromEnum VA = 243
  fromEnum VC = 244
  fromEnum VE = 245
  fromEnum VG = 246
  fromEnum VI = 247
  fromEnum VN = 248
  fromEnum VU = 249
  fromEnum WF = 250
  fromEnum WS = 251
  fromEnum XK = 252
  fromEnum YE = 253
  fromEnum YT = 254
  fromEnum ZA = 255
  fromEnum ZM = 256
  fromEnum ZW = 257
  fromEnum f = fromEnumError "Flag" f
  toEnum 0 = AC
  toEnum 1 = AD
  toEnum 2 = AE
  toEnum 3 = AF
  toEnum 4 = AG
  toEnum 5 = AI
  toEnum 6 = AL
  toEnum 7 = AM
  toEnum 8 = AO
  toEnum 9 = AQ
  toEnum 10 = AR
  toEnum 11 = AS
  toEnum 12 = AT
  toEnum 13 = AU
  toEnum 14 = AW
  toEnum 15 = AX
  toEnum 16 = AZ
  toEnum 17 = BA
  toEnum 18 = BB
  toEnum 19 = BD
  toEnum 20 = BE
  toEnum 21 = BF
  toEnum 22 = BG
  toEnum 23 = BH
  toEnum 24 = BI
  toEnum 25 = BJ
  toEnum 26 = BL
  toEnum 27 = BM
  toEnum 28 = BN
  toEnum 29 = BO
  toEnum 30 = BQ
  toEnum 31 = BR
  toEnum 32 = BS
  toEnum 33 = BT
  toEnum 34 = BV
  toEnum 35 = BW
  toEnum 36 = BY
  toEnum 37 = BZ
  toEnum 38 = CA
  toEnum 39 = CC
  toEnum 40 = CD
  toEnum 41 = CF
  toEnum 42 = CG
  toEnum 43 = CH
  toEnum 44 = CI
  toEnum 45 = CK
  toEnum 46 = CL
  toEnum 47 = CM
  toEnum 48 = CN
  toEnum 49 = CO
  toEnum 50 = CP
  toEnum 51 = CR
  toEnum 52 = CU
  toEnum 53 = CV
  toEnum 54 = CW
  toEnum 55 = CX
  toEnum 56 = CY
  toEnum 57 = CZ
  toEnum 58 = DE
  toEnum 59 = DG
  toEnum 60 = DJ
  toEnum 61 = DK
  toEnum 62 = DM
  toEnum 63 = DO
  toEnum 64 = DZ
  toEnum 65 = EA
  toEnum 66 = EC
  toEnum 67 = EE
  toEnum 68 = EG
  toEnum 69 = EH
  toEnum 70 = ER
  toEnum 71 = ES
  toEnum 72 = ET
  toEnum 73 = EU
  toEnum 74 = FI
  toEnum 75 = FJ
  toEnum 76 = FK
  toEnum 77 = FM
  toEnum 78 = FO
  toEnum 79 = FR
  toEnum 80 = GA
  toEnum 81 = GB
  toEnum 82 = GD
  toEnum 83 = GE
  toEnum 84 = GF
  toEnum 85 = GG
  toEnum 86 = GH
  toEnum 87 = GI
  toEnum 88 = GL
  toEnum 89 = GM
  toEnum 90 = GN
  toEnum 91 = GP
  toEnum 92 = GQ
  toEnum 93 = GR
  toEnum 94 = GS
  toEnum 95 = GT
  toEnum 96 = GU
  toEnum 97 = GW
  toEnum 98 = GY
  toEnum 99 = HK
  toEnum 100 = HM
  toEnum 101 = HN
  toEnum 102 = HR
  toEnum 103 = HT
  toEnum 104 = HU
  toEnum 105 = IC
  toEnum 106 = ID
  toEnum 107 = IE
  toEnum 108 = IL
  toEnum 109 = IM
  toEnum 110 = IN
  toEnum 111 = IO
  toEnum 112 = IQ
  toEnum 113 = IR
  toEnum 114 = IS
  toEnum 115 = IT
  toEnum 116 = JE
  toEnum 117 = JM
  toEnum 118 = JO
  toEnum 119 = JP
  toEnum 120 = KE
  toEnum 121 = KG
  toEnum 122 = KH
  toEnum 123 = KI
  toEnum 124 = KM
  toEnum 125 = KN
  toEnum 126 = KP
  toEnum 127 = KR
  toEnum 128 = KW
  toEnum 129 = KY
  toEnum 130 = KZ
  toEnum 131 = LA
  toEnum 132 = LB
  toEnum 133 = LC
  toEnum 134 = LI
  toEnum 135 = LK
  toEnum 136 = LR
  toEnum 137 = LS
  toEnum 138 = LT
  toEnum 139 = LU
  toEnum 140 = LV
  toEnum 141 = LY
  toEnum 142 = MA
  toEnum 143 = MC
  toEnum 144 = MD
  toEnum 145 = ME
  toEnum 146 = MF
  toEnum 147 = MG
  toEnum 148 = MH
  toEnum 149 = MK
  toEnum 150 = ML
  toEnum 151 = MM
  toEnum 152 = MN
  toEnum 153 = MO
  toEnum 154 = MP
  toEnum 155 = MQ
  toEnum 156 = MR
  toEnum 157 = MS
  toEnum 158 = MT
  toEnum 159 = MU
  toEnum 160 = MV
  toEnum 161 = MW
  toEnum 162 = MX
  toEnum 163 = MY
  toEnum 164 = MZ
  toEnum 165 = NA
  toEnum 166 = NC
  toEnum 167 = NE
  toEnum 168 = NF
  toEnum 169 = NG
  toEnum 170 = NI
  toEnum 171 = NL
  toEnum 172 = NO
  toEnum 173 = NP
  toEnum 174 = NR
  toEnum 175 = NU
  toEnum 176 = NZ
  toEnum 177 = OM
  toEnum 178 = PA
  toEnum 179 = PE
  toEnum 180 = PF
  toEnum 181 = PG
  toEnum 182 = PH
  toEnum 183 = PK
  toEnum 184 = PL
  toEnum 185 = PM
  toEnum 186 = PN
  toEnum 187 = PR
  toEnum 188 = PS
  toEnum 189 = PT
  toEnum 190 = PW
  toEnum 191 = PY
  toEnum 192 = QA
  toEnum 193 = RE
  toEnum 194 = RO
  toEnum 195 = RS
  toEnum 196 = RU
  toEnum 197 = RW
  toEnum 198 = SA
  toEnum 199 = SB
  toEnum 200 = SC
  toEnum 201 = SD
  toEnum 202 = SE
  toEnum 203 = SG
  toEnum 204 = SH
  toEnum 205 = SI
  toEnum 206 = SJ
  toEnum 207 = SK
  toEnum 208 = SL
  toEnum 209 = SM
  toEnum 210 = SN
  toEnum 211 = SO
  toEnum 212 = SR
  toEnum 213 = SS
  toEnum 214 = ST
  toEnum 215 = SV
  toEnum 216 = SX
  toEnum 217 = SY
  toEnum 218 = SZ
  toEnum 219 = TA
  toEnum 220 = TC
  toEnum 221 = TD
  toEnum 222 = TF
  toEnum 223 = TG
  toEnum 224 = TH
  toEnum 225 = TJ
  toEnum 226 = TK
  toEnum 227 = TL
  toEnum 228 = TM
  toEnum 229 = TN
  toEnum 230 = TO
  toEnum 231 = TR
  toEnum 232 = TT
  toEnum 233 = TV
  toEnum 234 = TW
  toEnum 235 = TZ
  toEnum 236 = UA
  toEnum 237 = UG
  toEnum 238 = UM
  toEnum 239 = UN
  toEnum 240 = US
  toEnum 241 = UY
  toEnum 242 = UZ
  toEnum 243 = VA
  toEnum 244 = VC
  toEnum 245 = VE
  toEnum 246 = VG
  toEnum 247 = VI
  toEnum 248 = VN
  toEnum 249 = VU
  toEnum 250 = WF
  toEnum 251 = WS
  toEnum 252 = XK
  toEnum 253 = YE
  toEnum 254 = YT
  toEnum 255 = ZA
  toEnum 256 = ZM
  toEnum 257 = ZW
  toEnum i = toEnumError "Flag" i (minBound :: Flag, maxBound)
  enumFrom = (`enumFromTo` maxBound)
  enumFromThen x y = enumFromThenTo x y maxBound

_subflags :: [SubFlag]
_subflags = [ENG, SCT, WLS, USAL, USAK, USAS, USAZ, USAR, USCA, USCO, USCT, USDE, USFL, USGA, USGU, USHI, USID, USIL, USIN, USIA, USKS, USKY, USLA, USME, USMD, USMA, USMI, USMN, USMS, USMO, USMT, USNE, USNV, USNH, USNJ, USNM, USNY, USNC, USND, USMP, USOH, USOK, USOR, USPA, USPR, USRI, USSC, USSD, USTN, USUM, USVI, USUT, USVT, USVA, USWA, USDC, USWV, USWI, USWY]

(?!) :: Int -> [a] -> Maybe a
(?!) n
  | n < 0 = const Nothing
  | otherwise = go n
  where
    go 0 (x : _) = Just x
    go i (_ : xs) = go (i - 1) xs
    go _ [] = Nothing

instance Enum SubFlag where
  fromEnum s = fromMaybe (fromEnumError "SubFlag" s) (elemIndex s _subflags)
  toEnum i = fromMaybe (toEnumError "SubFlag" i (minBound :: SubFlag, maxBound)) (i ?! _subflags)
  enumFrom = (`enumFromTo` maxBound)
  enumFromThen x y = enumFromThenTo x y maxBound

instance UnicodeText Flag where
  toUnicodeText (Flag ca cb) = iso3166Alpha2ToFlag' ca cb
  fromUnicodeText = fromFlag
  isInTextRange c
    | [ca, cb] <- unpack c, Just a <- shft ca, Just b <- shft cb = _validFlagEmoji a b
    | otherwise = False
    where
      shft = mapToEnumSafe _flagCharOffset

instance UnicodeText SubFlag where
  toUnicodeText (SubFlag (Flag ca cb) cc cd ce) = pack ('\x1f3f4' : go' ca : go' cb : map go (cc : cd : (maybe id (:) ce "\DEL")))
    where
      go = chr . (0xe0000 .|.) . ord
      go' = go . toLower
  fromUnicodeText t = listToMaybe [sf | sf <- _subflags, t == toUnicodeText sf]
  isInTextRange = (`elem` map toUnicodeText _subflags)

-- | A data type to represent additional non-regional flags defined by the Unicode standard.
data ExtraFlag
  = -- | A flag with black and white square like in a checkerboard pattern. These are often used to signal the start or end of a car race. This is rendered as 🏁.
    ChequeredFlag
  | -- | A triangular flag that is often used for golf. This is rendered as 🚩.
    TriangularFlagOnPost
  | -- | This emoji depicts two /Japanese/ flags crossed at the base. Older versions of Samsung use two South Korean flags. This is rendered as 🎌.
    CrossedFlags
  | -- | A waving black flag. This is rendered as 🏴.
    BlackFlag
  | -- | A waving white flag. This is often used as a sign of /surrender/. This is rendered as 🏳️.
    WavingWhiteFlag
  | -- | A flag with six colors of the rainbow that usually include red, orange, yellow, green, blue and purple. This is rendered as 🏳️‍🌈.
    RainbowFlag
  | -- | A flag with horizontal pale blue and pale pink stripes and a single white stripe in the middle. This is used as a /transgender/ pride flag. This is rendered as 🏳️‍⚧️.
    TransgenderFlag
  | -- | A /skull and crossbones/ displayed on a black flag. On pirate ships this is known as the Jolly Roger. This is rendered as 🏴‍☠️.
    PirateFlag
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary ExtraFlag where
  arbitrary = arbitraryBoundedEnum

instance Hashable ExtraFlag

instance NFData ExtraFlag

instance UnicodeText ExtraFlag where
  toUnicodeText ChequeredFlag = "\x1f3c1"
  toUnicodeText TriangularFlagOnPost = "\x1f6a9"
  toUnicodeText CrossedFlags = "\x1f38c"
  toUnicodeText BlackFlag = "\x1f3f4"
  toUnicodeText WavingWhiteFlag = "\x1f3f3\xfe0f"
  toUnicodeText RainbowFlag = "\x1f3f3\xfe0f\x200d\x1f308"
  toUnicodeText TransgenderFlag = "\x1f3f3\xfe0f\x200d\x26a7\xfe0f"
  toUnicodeText PirateFlag = "\x1f3f4\x200d\x2620\xfe0f"
  fromUnicodeText "\x1f3c1" = Just ChequeredFlag
  fromUnicodeText "\x1f6a9" = Just TriangularFlagOnPost
  fromUnicodeText "\x1f38c" = Just CrossedFlags
  fromUnicodeText "\x1f3f4" = Just BlackFlag
  fromUnicodeText "\x1f3f3\xfe0f" = Just WavingWhiteFlag
  fromUnicodeText "\x1f3f3\xfe0f\x200d\x1f308" = Just RainbowFlag
  fromUnicodeText "\x1f3f3\xfe0f\x200d\x26a7\xfe0f" = Just TransgenderFlag
  fromUnicodeText "\x1f3f4\x200d\x2620\xfe0f" = Just PirateFlag
  fromUnicodeText _ = Nothing
  isInTextRange "\x1f3c1" = True
  isInTextRange "\x1f6a9" = True
  isInTextRange "\x1f38c" = True
  isInTextRange "\x1f3f4" = True
  isInTextRange "\x1f3f3\xfe0f" = True
  isInTextRange "\x1f3f3\xfe0f\x200d\x1f308" = True
  isInTextRange "\x1f3f3\xfe0f\x200d\x26a7\xfe0f" = True
  isInTextRange "\x1f3f4\x200d\x2620\xfe0f" = True
  isInTextRange _ = False
