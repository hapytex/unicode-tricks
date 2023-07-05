{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Egyptian
-- Description : A module that defines pattern synonyms for the 1099 hieroglyphs in Unicode.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Unicode defines the <http://unicode.org/charts/PDF/U13000.pdf 13000 block> that contains 1099 hieroglyphs. This module defines pattern synonyms to make working with the hieroglyphs more
-- convenient.
module Data.Char.Egyptian
  ( -- * A. Man and his occupations
    pattern A001,
    pattern A002,
    pattern A003,
    pattern A004,
    pattern A005,
    pattern A005A,
    pattern A006,
    pattern A006A,
    pattern A006B,
    pattern A007,
    pattern A008,
    pattern A009,
    pattern A010,
    pattern A011,
    pattern A012,
    pattern A013,
    pattern A014,
    pattern A014A,
    pattern A015,
    pattern A016,
    pattern A017,
    pattern A017A,
    pattern A018,
    pattern A019,
    pattern A020,
    pattern A021,
    pattern A022,
    pattern A023,
    pattern A024,
    pattern A025,
    pattern A026,
    pattern A027,
    pattern A028,
    pattern A029,
    pattern A030,
    pattern A031,
    pattern A032,
    pattern A032A,
    pattern A033,
    pattern A034,
    pattern A035,
    pattern A036,
    pattern A037,
    pattern A038,
    pattern A039,
    pattern A040,
    pattern A040A,
    pattern A041,
    pattern A042,
    pattern A042A,
    pattern A043,
    pattern A043A,
    pattern A044,
    pattern A045,
    pattern A045A,
    pattern A046,
    pattern A047,
    pattern A048,
    pattern A049,
    pattern A050,
    pattern A051,
    pattern A052,
    pattern A053,
    pattern A054,
    pattern A055,
    pattern A056,
    pattern A057,
    pattern A058,
    pattern A059,
    pattern A060,
    pattern A061,
    pattern A062,
    pattern A063,
    pattern A064,
    pattern A065,
    pattern A066,
    pattern A067,
    pattern A068,
    pattern A069,
    pattern A070,

    -- * B. Woman and her occupations
    pattern B001,
    pattern B002,
    pattern B003,
    pattern B004,
    pattern B005,
    pattern B005A,
    pattern B006,
    pattern B007,
    pattern B008,
    pattern B009,

    -- * C. Anthropomorphic deities
    pattern C001,
    pattern C002,
    pattern C002A,
    pattern C002B,
    pattern C002C,
    pattern C003,
    pattern C004,
    pattern C005,
    pattern C006,
    pattern C007,
    pattern C008,
    pattern C009,
    pattern C010,
    pattern C010A,
    pattern C011,
    pattern C012,
    pattern C013,
    pattern C014,
    pattern C015,
    pattern C016,
    pattern C017,
    pattern C018,
    pattern C019,
    pattern C020,
    pattern C021,
    pattern C022,
    pattern C023,
    pattern C024,

    -- * D. Parts of the human body
    pattern D001,
    pattern D002,
    pattern D003,
    pattern D004,
    pattern D005,
    pattern D006,
    pattern D007,
    pattern D008,
    pattern D008A,
    pattern D009,
    pattern D010,
    pattern D011,
    pattern D012,
    pattern D013,
    pattern D014,
    pattern D015,
    pattern D016,
    pattern D017,
    pattern D018,
    pattern D019,
    pattern D020,
    pattern D021,
    pattern D022,
    pattern D023,
    pattern D024,
    pattern D025,
    pattern D026,
    pattern D027,
    pattern D027A,
    pattern D028,
    pattern D029,
    pattern D030,
    pattern D031,
    pattern D031A,
    pattern D032,
    pattern D033,
    pattern D034,
    pattern D034A,
    pattern D035,
    pattern D036,
    pattern D037,
    pattern D038,
    pattern D039,
    pattern D040,
    pattern D041,
    pattern D042,
    pattern D043,
    pattern D044,
    pattern D045,
    pattern D046,
    pattern D046A,
    pattern D047,
    pattern D048,
    pattern D048A,
    pattern D049,
    pattern D050,
    pattern D050A,
    pattern D050B,
    pattern D050C,
    pattern D050D,
    pattern D050E,
    pattern D050F,
    pattern D050G,
    pattern D050H,
    pattern D050I,
    pattern D051,
    pattern D052,
    pattern D052A,
    pattern D053,
    pattern D054,
    pattern D054A,
    pattern D055,
    pattern D056,
    pattern D057,
    pattern D058,
    pattern D059,
    pattern D060,
    pattern D061,
    pattern D062,
    pattern D063,
    pattern D064,
    pattern D065,
    pattern D066,
    pattern D067,
    pattern D067A,
    pattern D067B,
    pattern D067C,
    pattern D067D,
    pattern D067E,
    pattern D067F,
    pattern D067G,
    pattern D067H,

    -- * E. Mammals
    pattern E001,
    pattern E002,
    pattern E003,
    pattern E004,
    pattern E005,
    pattern E006,
    pattern E007,
    pattern E008,
    pattern E008A,
    pattern E009,
    pattern E009A,
    pattern E010,
    pattern E011,
    pattern E012,
    pattern E013,
    pattern E014,
    pattern E015,
    pattern E016,
    pattern E016A,
    pattern E017,
    pattern E017A,
    pattern E018,
    pattern E019,
    pattern E020,
    pattern E020A,
    pattern E021,
    pattern E022,
    pattern E023,
    pattern E024,
    pattern E025,
    pattern E026,
    pattern E027,
    pattern E028,
    pattern E028A,
    pattern E029,
    pattern E030,
    pattern E031,
    pattern E032,
    pattern E033,
    pattern E034,
    pattern E034A,
    pattern E036,
    pattern E037,
    pattern E038,

    -- * F. Parts of mammals
    pattern F001,
    pattern F001A,
    pattern F002,
    pattern F003,
    pattern F004,
    pattern F005,
    pattern F006,
    pattern F007,
    pattern F008,
    pattern F009,
    pattern F010,
    pattern F011,
    pattern F012,
    pattern F013,
    pattern F013A,
    pattern F014,
    pattern F015,
    pattern F016,
    pattern F017,
    pattern F018,
    pattern F019,
    pattern F020,
    pattern F021,
    pattern F021A,
    pattern F022,
    pattern F023,
    pattern F024,
    pattern F025,
    pattern F026,
    pattern F027,
    pattern F028,
    pattern F029,
    pattern F030,
    pattern F031,
    pattern F031A,
    pattern F032,
    pattern F033,
    pattern F034,
    pattern F035,
    pattern F036,
    pattern F037,
    pattern F037A,
    pattern F038,
    pattern F038A,
    pattern F039,
    pattern F040,
    pattern F041,
    pattern F042,
    pattern F043,
    pattern F044,
    pattern F045,
    pattern F045A,
    pattern F046,
    pattern F046A,
    pattern F047,
    pattern F047A,
    pattern F048,
    pattern F049,
    pattern F050,
    pattern F051,
    pattern F051A,
    pattern F051B,
    pattern F051C,
    pattern F052,
    pattern F053,

    -- * G. Birds
    pattern G001,
    pattern G002,
    pattern G003,
    pattern G004,
    pattern G005,
    pattern G006,
    pattern G006A,
    pattern G007,
    pattern G007A,
    pattern G007B,
    pattern G008,
    pattern G009,
    pattern G010,
    pattern G011,
    pattern G011A,
    pattern G012,
    pattern G013,
    pattern G014,
    pattern G015,
    pattern G016,
    pattern G017,
    pattern G018,
    pattern G019,
    pattern G020,
    pattern G020A,
    pattern G021,
    pattern G022,
    pattern G023,
    pattern G024,
    pattern G025,
    pattern G026,
    pattern G026A,
    pattern G027,
    pattern G028,
    pattern G029,
    pattern G030,
    pattern G031,
    pattern G032,
    pattern G033,
    pattern G034,
    pattern G035,
    pattern G036,
    pattern G036A,
    pattern G037,
    pattern G037A,
    pattern G038,
    pattern G039,
    pattern G040,
    pattern G041,
    pattern G042,
    pattern G043,
    pattern G043A,
    pattern G044,
    pattern G045,
    pattern G045A,
    pattern G046,
    pattern G047,
    pattern G048,
    pattern G049,
    pattern G050,
    pattern G051,
    pattern G052,
    pattern G053,
    pattern G054,

    -- * H. Parts of birds
    pattern H001,
    pattern H002,
    pattern H003,
    pattern H004,
    pattern H005,
    pattern H006,
    pattern H006A,
    pattern H007,
    pattern H008,

    -- * I. Amphibious animals, reptiles, etc.
    pattern I001,
    pattern I002,
    pattern I003,
    pattern I004,
    pattern I005,
    pattern I005A,
    pattern I006,
    pattern I007,
    pattern I008,
    pattern I009,
    pattern I009A,
    pattern I010,
    pattern I010A,
    pattern I011,
    pattern I011A,
    pattern I012,
    pattern I013,
    pattern I014,
    pattern I015,

    -- * K. Fishes and parts of fishes
    pattern K001,
    pattern K002,
    pattern K003,
    pattern K004,
    pattern K005,
    pattern K006,
    pattern K007,
    pattern K008,

    -- * L. Invertabrata and lesser animals
    pattern L001,
    pattern L002,
    pattern L002A,
    pattern L003,
    pattern L004,
    pattern L005,
    pattern L006,
    pattern L006A,
    pattern L007,
    pattern L008,

    -- * M. Trees and plants
    pattern M001,
    pattern M001A,
    pattern M001B,
    pattern M002,
    pattern M003,
    pattern M003A,
    pattern M004,
    pattern M005,
    pattern M006,
    pattern M007,
    pattern M008,
    pattern M009,
    pattern M010,
    pattern M010A,
    pattern M011,
    pattern M012,
    pattern M012A,
    pattern M012B,
    pattern M012C,
    pattern M012D,
    pattern M012E,
    pattern M012F,
    pattern M012G,
    pattern M012H,
    pattern M013,
    pattern M014,
    pattern M015,
    pattern M015A,
    pattern M016,
    pattern M016A,
    pattern M017,
    pattern M017A,
    pattern M018,
    pattern M019,
    pattern M020,
    pattern M021,
    pattern M022,
    pattern M022A,
    pattern M023,
    pattern M024,
    pattern M024A,
    pattern M025,
    pattern M026,
    pattern M027,
    pattern M028,
    pattern M028A,
    pattern M029,
    pattern M030,
    pattern M031,
    pattern M031A,
    pattern M032,
    pattern M033,
    pattern M033A,
    pattern M033B,
    pattern M034,
    pattern M035,
    pattern M036,
    pattern M037,
    pattern M038,
    pattern M039,
    pattern M040,
    pattern M040A,
    pattern M041,
    pattern M042,
    pattern M043,
    pattern M044,

    -- * N. Sky, earth, water
    pattern N001,
    pattern N002,
    pattern N003,
    pattern N004,
    pattern N005,
    pattern N006,
    pattern N007,
    pattern N008,
    pattern N009,
    pattern N010,
    pattern N011,
    pattern N012,
    pattern N013,
    pattern N014,
    pattern N015,
    pattern N016,
    pattern N017,
    pattern N018,
    pattern N018A,
    pattern N018B,
    pattern N019,
    pattern N020,
    pattern N021,
    pattern N022,
    pattern N023,
    pattern N024,
    pattern N025,
    pattern N025A,
    pattern N026,
    pattern N027,
    pattern N028,
    pattern N029,
    pattern N030,
    pattern N031,
    pattern N032,
    pattern N033,
    pattern N033A,
    pattern N034,
    pattern N034A,
    pattern N035,
    pattern N035A,
    pattern N036,
    pattern N037,
    pattern N037A,
    pattern N038,
    pattern N039,
    pattern N040,
    pattern N041,
    pattern N042,

    -- * NL. Nomes of Lower Egypt
    pattern NL001,
    pattern NL002,
    pattern NL003,
    pattern NL004,
    pattern NL005,
    pattern NL005A,
    pattern NL006,
    pattern NL007,
    pattern NL008,
    pattern NL009,
    pattern NL010,
    pattern NL011,
    pattern NL012,
    pattern NL013,
    pattern NL014,
    pattern NL015,
    pattern NL016,
    pattern NL017,
    pattern NL017A,
    pattern NL018,
    pattern NL019,
    pattern NL020,

    -- * NU. Nomes of Upper Egypt
    pattern NU001,
    pattern NU002,
    pattern NU003,
    pattern NU004,
    pattern NU005,
    pattern NU006,
    pattern NU007,
    pattern NU008,
    pattern NU009,
    pattern NU010,
    pattern NU010A,
    pattern NU011,
    pattern NU011A,
    pattern NU012,
    pattern NU013,
    pattern NU014,
    pattern NU015,
    pattern NU016,
    pattern NU017,
    pattern NU018,
    pattern NU018A,
    pattern NU019,
    pattern NU020,
    pattern NU021,
    pattern NU022,
    pattern NU022A,

    -- * O. Buildings, parts of buildings, etc.
    pattern O001,
    pattern O001A,
    pattern O002,
    pattern O003,
    pattern O004,
    pattern O005,
    pattern O005A,
    pattern O006,
    pattern O006A,
    pattern O006B,
    pattern O006C,
    pattern O006D,
    pattern O006E,
    pattern O006F,
    pattern O007,
    pattern O008,
    pattern O009,
    pattern O010,
    pattern O010A,
    pattern O010B,
    pattern O010C,
    pattern O011,
    pattern O012,
    pattern O013,
    pattern O014,
    pattern O015,
    pattern O016,
    pattern O017,
    pattern O018,
    pattern O019,
    pattern O019A,
    pattern O020,
    pattern O020A,
    pattern O021,
    pattern O022,
    pattern O023,
    pattern O024,
    pattern O024A,
    pattern O025,
    pattern O025A,
    pattern O026,
    pattern O027,
    pattern O028,
    pattern O029,
    pattern O029A,
    pattern O030,
    pattern O030A,
    pattern O031,
    pattern O032,
    pattern O033,
    pattern O033A,
    pattern O034,
    pattern O035,
    pattern O036,
    pattern O036A,
    pattern O036B,
    pattern O036C,
    pattern O036D,
    pattern O037,
    pattern O038,
    pattern O039,
    pattern O040,
    pattern O041,
    pattern O042,
    pattern O043,
    pattern O044,
    pattern O045,
    pattern O046,
    pattern O047,
    pattern O048,
    pattern O049,
    pattern O050,
    pattern O050A,
    pattern O050B,
    pattern O051,

    -- * P. Ships and parts of ships
    pattern P001,
    pattern P001A,
    pattern P002,
    pattern P003,
    pattern P003A,
    pattern P004,
    pattern P005,
    pattern P006,
    pattern P007,
    pattern P008,
    pattern P009,
    pattern P010,
    pattern P011,

    -- * Q. Domestic and funerary furniture
    pattern Q001,
    pattern Q002,
    pattern Q003,
    pattern Q004,
    pattern Q005,
    pattern Q006,
    pattern Q007,

    -- * R. Temple furniture and sacred emblems
    pattern R001,
    pattern R002,
    pattern R002A,
    pattern R003,
    pattern R003A,
    pattern R003B,
    pattern R004,
    pattern R005,
    pattern R006,
    pattern R007,
    pattern R008,
    pattern R009,
    pattern R010,
    pattern R010A,
    pattern R011,
    pattern R012,
    pattern R013,
    pattern R014,
    pattern R015,
    pattern R016,
    pattern R016A,
    pattern R017,
    pattern R018,
    pattern R019,
    pattern R020,
    pattern R021,
    pattern R022,
    pattern R023,
    pattern R024,
    pattern R025,
    pattern R026,
    pattern R027,
    pattern R028,
    pattern R029,

    -- * S. Crowns, dress, staves, etc.
    pattern S001,
    pattern S002,
    pattern S002A,
    pattern S003,
    pattern S004,
    pattern S005,
    pattern S006,
    pattern S006A,
    pattern S007,
    pattern S008,
    pattern S009,
    pattern S010,
    pattern S011,
    pattern S012,
    pattern S013,
    pattern S014,
    pattern S014A,
    pattern S014B,
    pattern S015,
    pattern S016,
    pattern S017,
    pattern S017A,
    pattern S018,
    pattern S019,
    pattern S020,
    pattern S021,
    pattern S022,
    pattern S023,
    pattern S024,
    pattern S025,
    pattern S026,
    pattern S026A,
    pattern S026B,
    pattern S027,
    pattern S028,
    pattern S029,
    pattern S030,
    pattern S031,
    pattern S032,
    pattern S033,
    pattern S034,
    pattern S035,
    pattern S035A,
    pattern S036,
    pattern S037,
    pattern S038,
    pattern S039,
    pattern S040,
    pattern S041,
    pattern S042,
    pattern S043,
    pattern S044,
    pattern S045,
    pattern S046,

    -- * T. Warfare, hunting, butchery
    pattern T001,
    pattern T002,
    pattern T003,
    pattern T003A,
    pattern T004,
    pattern T005,
    pattern T006,
    pattern T007,
    pattern T007A,
    pattern T008,
    pattern T008A,
    pattern T009,
    pattern T009A,
    pattern T010,
    pattern T011,
    pattern T011A,
    pattern T012,
    pattern T013,
    pattern T014,
    pattern T015,
    pattern T016,
    pattern T016A,
    pattern T017,
    pattern T018,
    pattern T019,
    pattern T020,
    pattern T021,
    pattern T022,
    pattern T023,
    pattern T024,
    pattern T025,
    pattern T026,
    pattern T027,
    pattern T028,
    pattern T029,
    pattern T030,
    pattern T031,
    pattern T032,
    pattern T032A,
    pattern T033,
    pattern T033A,
    pattern T034,
    pattern T035,
    pattern T036,

    -- * U. Agriculture, crafts, and professions
    pattern U001,
    pattern U002,
    pattern U003,
    pattern U004,
    pattern U005,
    pattern U006,
    pattern U006A,
    pattern U006B,
    pattern U007,
    pattern U008,
    pattern U009,
    pattern U010,
    pattern U011,
    pattern U012,
    pattern U013,
    pattern U014,
    pattern U015,
    pattern U016,
    pattern U017,
    pattern U018,
    pattern U019,
    pattern U020,
    pattern U021,
    pattern U022,
    pattern U023,
    pattern U023A,
    pattern U024,
    pattern U025,
    pattern U026,
    pattern U027,
    pattern U028,
    pattern U029,
    pattern U029A,
    pattern U030,
    pattern U031,
    pattern U032,
    pattern U032A,
    pattern U033,
    pattern U034,
    pattern U035,
    pattern U036,
    pattern U037,
    pattern U038,
    pattern U039,
    pattern U040,
    pattern U041,
    pattern U042,

    -- * V. Rope, fiber, baskets, bags, etc.
    pattern V001,
    pattern V001A,
    pattern V001B,
    pattern V001C,
    pattern V001D,
    pattern V001E,
    pattern V001F,
    pattern V001G,
    pattern V001H,
    pattern V001I,
    pattern V002,
    pattern V002A,
    pattern V003,
    pattern V004,
    pattern V005,
    pattern V006,
    pattern V007,
    pattern V007A,
    pattern V007B,
    pattern V008,
    pattern V009,
    pattern V010,
    pattern V011,
    pattern V011A,
    pattern V011B,
    pattern V011C,
    pattern V012,
    pattern V012A,
    pattern V012B,
    pattern V013,
    pattern V014,
    pattern V015,
    pattern V016,
    pattern V017,
    pattern V018,
    pattern V019,
    pattern V020,
    pattern V020A,
    pattern V020B,
    pattern V020C,
    pattern V020D,
    pattern V020E,
    pattern V020F,
    pattern V020G,
    pattern V020H,
    pattern V020I,
    pattern V020J,
    pattern V020K,
    pattern V020L,
    pattern V021,
    pattern V022,
    pattern V023,
    pattern V023A,
    pattern V024,
    pattern V025,
    pattern V026,
    pattern V027,
    pattern V028,
    pattern V028A,
    pattern V029,
    pattern V029A,
    pattern V030,
    pattern V030A,
    pattern V031,
    pattern V031A,
    pattern V032,
    pattern V033,
    pattern V033A,
    pattern V034,
    pattern V035,
    pattern V036,
    pattern V037,
    pattern V037A,
    pattern V038,
    pattern V039,
    pattern V040,
    pattern V040A,

    -- * W. Vessels of stone and earthenware
    pattern W001,
    pattern W002,
    pattern W003,
    pattern W003A,
    pattern W004,
    pattern W005,
    pattern W006,
    pattern W007,
    pattern W008,
    pattern W009,
    pattern W009A,
    pattern W010,
    pattern W010A,
    pattern W011,
    pattern W012,
    pattern W013,
    pattern W014,
    pattern W014A,
    pattern W015,
    pattern W016,
    pattern W017,
    pattern W017A,
    pattern W018,
    pattern W018A,
    pattern W019,
    pattern W020,
    pattern W021,
    pattern W022,
    pattern W023,
    pattern W024,
    pattern W024A,
    pattern W025,

    -- * X. Loaves and cakes
    pattern X001,
    pattern X002,
    pattern X003,
    pattern X004,
    pattern X004A,
    pattern X004B,
    pattern X005,
    pattern X006,
    pattern X006A,
    pattern X007,
    pattern X008,
    pattern X008A,

    -- * Y. Writings, games, music
    pattern Y001,
    pattern Y001A,
    pattern Y002,
    pattern Y003,
    pattern Y004,
    pattern Y005,
    pattern Y006,
    pattern Y007,
    pattern Y008,

    -- * Z. Strokes, signs derived from Hieratic, geometrical figures
    pattern Z001,
    pattern Z002,
    pattern Z002A,
    pattern Z002B,
    pattern Z002C,
    pattern Z002D,
    pattern Z003,
    pattern Z003A,
    pattern Z003B,
    pattern Z004,
    pattern Z004A,
    pattern Z005,
    pattern Z005A,
    pattern Z006,
    pattern Z007,
    pattern Z008,
    pattern Z009,
    pattern Z010,
    pattern Z011,
    pattern Z012,
    pattern Z013,
    pattern Z014,
    pattern Z015,
    pattern Z015A,
    pattern Z015B,
    pattern Z015C,
    pattern Z015D,
    pattern Z015E,
    pattern Z015F,
    pattern Z015G,
    pattern Z015H,
    pattern Z015I,
    pattern Z016,
    pattern Z016A,
    pattern Z016B,
    pattern Z016C,
    pattern Z016D,
    pattern Z016E,
    pattern Z016F,
    pattern Z016G,
    pattern Z016H,

    -- * AA. Unclassified
    pattern AA001,
    pattern AA002,
    pattern AA003,
    pattern AA004,
    pattern AA005,
    pattern AA006,
    pattern AA007,
    pattern AA007A,
    pattern AA007B,
    pattern AA008,
    pattern AA009,
    pattern AA010,
    pattern AA011,
    pattern AA012,
    pattern AA013,
    pattern AA014,
    pattern AA015,
    pattern AA016,
    pattern AA017,
    pattern AA018,
    pattern AA019,
    pattern AA020,
    pattern AA021,
    pattern AA022,
    pattern AA023,
    pattern AA024,
    pattern AA025,
    pattern AA026,
    pattern AA027,
    pattern AA028,
    pattern AA029,
    pattern AA030,
    pattern AA031,
    pattern AA032,
  )
where

-- | The Egyptian hieroglyph /A001/ that renders as &#x13000;.
pattern A001 :: Char
pattern A001 = '\x13000'

-- | The Egyptian hieroglyph /A002/ that renders as &#x13001;.
pattern A002 :: Char
pattern A002 = '\x13001'

-- | The Egyptian hieroglyph /A003/ that renders as &#x13002;.
pattern A003 :: Char
pattern A003 = '\x13002'

-- | The Egyptian hieroglyph /A004/ that renders as &#x13003;.
pattern A004 :: Char
pattern A004 = '\x13003'

-- | The Egyptian hieroglyph /A005/ that renders as &#x13004;.
pattern A005 :: Char
pattern A005 = '\x13004'

-- | The Egyptian hieroglyph /A005A/ that renders as &#x13005;.
pattern A005A :: Char
pattern A005A = '\x13005'

-- | The Egyptian hieroglyph /A006/ that renders as &#x13006;.
pattern A006 :: Char
pattern A006 = '\x13006'

-- | The Egyptian hieroglyph /A006A/ that renders as &#x13007;.
pattern A006A :: Char
pattern A006A = '\x13007'

-- | The Egyptian hieroglyph /A006B/ that renders as &#x13008;.
pattern A006B :: Char
pattern A006B = '\x13008'

-- | The Egyptian hieroglyph /A007/ that renders as &#x13009;.
pattern A007 :: Char
pattern A007 = '\x13009'

-- | The Egyptian hieroglyph /A008/ that renders as &#x1300a;.
pattern A008 :: Char
pattern A008 = '\x1300a'

-- | The Egyptian hieroglyph /A009/ that renders as &#x1300b;.
pattern A009 :: Char
pattern A009 = '\x1300b'

-- | The Egyptian hieroglyph /A010/ that renders as &#x1300c;.
pattern A010 :: Char
pattern A010 = '\x1300c'

-- | The Egyptian hieroglyph /A011/ that renders as &#x1300d;.
pattern A011 :: Char
pattern A011 = '\x1300d'

-- | The Egyptian hieroglyph /A012/ that renders as &#x1300e;.
pattern A012 :: Char
pattern A012 = '\x1300e'

-- | The Egyptian hieroglyph /A013/ that renders as &#x1300f;.
pattern A013 :: Char
pattern A013 = '\x1300f'

-- | The Egyptian hieroglyph /A014/ that renders as &#x13010;.
pattern A014 :: Char
pattern A014 = '\x13010'

-- | The Egyptian hieroglyph /A014A/ that renders as &#x13011;.
pattern A014A :: Char
pattern A014A = '\x13011'

-- | The Egyptian hieroglyph /A015/ that renders as &#x13012;.
pattern A015 :: Char
pattern A015 = '\x13012'

-- | The Egyptian hieroglyph /A016/ that renders as &#x13013;.
pattern A016 :: Char
pattern A016 = '\x13013'

-- | The Egyptian hieroglyph /A017/ that renders as &#x13014;.
pattern A017 :: Char
pattern A017 = '\x13014'

-- | The Egyptian hieroglyph /A017A/ that renders as &#x13015;.
pattern A017A :: Char
pattern A017A = '\x13015'

-- | The Egyptian hieroglyph /A018/ that renders as &#x13016;.
pattern A018 :: Char
pattern A018 = '\x13016'

-- | The Egyptian hieroglyph /A019/ that renders as &#x13017;.
pattern A019 :: Char
pattern A019 = '\x13017'

-- | The Egyptian hieroglyph /A020/ that renders as &#x13018;.
pattern A020 :: Char
pattern A020 = '\x13018'

-- | The Egyptian hieroglyph /A021/ that renders as &#x13019;.
pattern A021 :: Char
pattern A021 = '\x13019'

-- | The Egyptian hieroglyph /A022/ that renders as &#x1301a;.
pattern A022 :: Char
pattern A022 = '\x1301a'

-- | The Egyptian hieroglyph /A023/ that renders as &#x1301b;.
pattern A023 :: Char
pattern A023 = '\x1301b'

-- | The Egyptian hieroglyph /A024/ that renders as &#x1301c;.
pattern A024 :: Char
pattern A024 = '\x1301c'

-- | The Egyptian hieroglyph /A025/ that renders as &#x1301d;.
pattern A025 :: Char
pattern A025 = '\x1301d'

-- | The Egyptian hieroglyph /A026/ that renders as &#x1301e;.
pattern A026 :: Char
pattern A026 = '\x1301e'

-- | The Egyptian hieroglyph /A027/ that renders as &#x1301f;.
pattern A027 :: Char
pattern A027 = '\x1301f'

-- | The Egyptian hieroglyph /A028/ that renders as &#x13020;.
pattern A028 :: Char
pattern A028 = '\x13020'

-- | The Egyptian hieroglyph /A029/ that renders as &#x13021;.
pattern A029 :: Char
pattern A029 = '\x13021'

-- | The Egyptian hieroglyph /A030/ that renders as &#x13022;.
pattern A030 :: Char
pattern A030 = '\x13022'

-- | The Egyptian hieroglyph /A031/ that renders as &#x13023;.
pattern A031 :: Char
pattern A031 = '\x13023'

-- | The Egyptian hieroglyph /A032/ that renders as &#x13024;.
pattern A032 :: Char
pattern A032 = '\x13024'

-- | The Egyptian hieroglyph /A032A/ that renders as &#x13025;.
pattern A032A :: Char
pattern A032A = '\x13025'

-- | The Egyptian hieroglyph /A033/ that renders as &#x13026;.
pattern A033 :: Char
pattern A033 = '\x13026'

-- | The Egyptian hieroglyph /A034/ that renders as &#x13027;.
pattern A034 :: Char
pattern A034 = '\x13027'

-- | The Egyptian hieroglyph /A035/ that renders as &#x13028;.
pattern A035 :: Char
pattern A035 = '\x13028'

-- | The Egyptian hieroglyph /A036/ that renders as &#x13029;.
pattern A036 :: Char
pattern A036 = '\x13029'

-- | The Egyptian hieroglyph /A037/ that renders as &#x1302a;.
pattern A037 :: Char
pattern A037 = '\x1302a'

-- | The Egyptian hieroglyph /A038/ that renders as &#x1302b;.
pattern A038 :: Char
pattern A038 = '\x1302b'

-- | The Egyptian hieroglyph /A039/ that renders as &#x1302c;.
pattern A039 :: Char
pattern A039 = '\x1302c'

-- | The Egyptian hieroglyph /A040/ that renders as &#x1302d;.
pattern A040 :: Char
pattern A040 = '\x1302d'

-- | The Egyptian hieroglyph /A040A/ that renders as &#x1302e;.
pattern A040A :: Char
pattern A040A = '\x1302e'

-- | The Egyptian hieroglyph /A041/ that renders as &#x1302f;.
pattern A041 :: Char
pattern A041 = '\x1302f'

-- | The Egyptian hieroglyph /A042/ that renders as &#x13030;.
pattern A042 :: Char
pattern A042 = '\x13030'

-- | The Egyptian hieroglyph /A042A/ that renders as &#x13031;.
pattern A042A :: Char
pattern A042A = '\x13031'

-- | The Egyptian hieroglyph /A043/ that renders as &#x13032;.
pattern A043 :: Char
pattern A043 = '\x13032'

-- | The Egyptian hieroglyph /A043A/ that renders as &#x13033;.
pattern A043A :: Char
pattern A043A = '\x13033'

-- | The Egyptian hieroglyph /A044/ that renders as &#x13034;.
pattern A044 :: Char
pattern A044 = '\x13034'

-- | The Egyptian hieroglyph /A045/ that renders as &#x13035;.
pattern A045 :: Char
pattern A045 = '\x13035'

-- | The Egyptian hieroglyph /A045A/ that renders as &#x13036;.
pattern A045A :: Char
pattern A045A = '\x13036'

-- | The Egyptian hieroglyph /A046/ that renders as &#x13037;.
pattern A046 :: Char
pattern A046 = '\x13037'

-- | The Egyptian hieroglyph /A047/ that renders as &#x13038;.
pattern A047 :: Char
pattern A047 = '\x13038'

-- | The Egyptian hieroglyph /A048/ that renders as &#x13039;.
pattern A048 :: Char
pattern A048 = '\x13039'

-- | The Egyptian hieroglyph /A049/ that renders as &#x1303a;.
pattern A049 :: Char
pattern A049 = '\x1303a'

-- | The Egyptian hieroglyph /A050/ that renders as &#x1303b;.
pattern A050 :: Char
pattern A050 = '\x1303b'

-- | The Egyptian hieroglyph /A051/ that renders as &#x1303c;.
pattern A051 :: Char
pattern A051 = '\x1303c'

-- | The Egyptian hieroglyph /A052/ that renders as &#x1303d;.
pattern A052 :: Char
pattern A052 = '\x1303d'

-- | The Egyptian hieroglyph /A053/ that renders as &#x1303e;.
pattern A053 :: Char
pattern A053 = '\x1303e'

-- | The Egyptian hieroglyph /A054/ that renders as &#x1303f;.
pattern A054 :: Char
pattern A054 = '\x1303f'

-- | The Egyptian hieroglyph /A055/ that renders as &#x13040;.
pattern A055 :: Char
pattern A055 = '\x13040'

-- | The Egyptian hieroglyph /A056/ that renders as &#x13041;.
pattern A056 :: Char
pattern A056 = '\x13041'

-- | The Egyptian hieroglyph /A057/ that renders as &#x13042;.
pattern A057 :: Char
pattern A057 = '\x13042'

-- | The Egyptian hieroglyph /A058/ that renders as &#x13043;.
pattern A058 :: Char
pattern A058 = '\x13043'

-- | The Egyptian hieroglyph /A059/ that renders as &#x13044;.
pattern A059 :: Char
pattern A059 = '\x13044'

-- | The Egyptian hieroglyph /A060/ that renders as &#x13045;.
pattern A060 :: Char
pattern A060 = '\x13045'

-- | The Egyptian hieroglyph /A061/ that renders as &#x13046;.
pattern A061 :: Char
pattern A061 = '\x13046'

-- | The Egyptian hieroglyph /A062/ that renders as &#x13047;.
pattern A062 :: Char
pattern A062 = '\x13047'

-- | The Egyptian hieroglyph /A063/ that renders as &#x13048;.
pattern A063 :: Char
pattern A063 = '\x13048'

-- | The Egyptian hieroglyph /A064/ that renders as &#x13049;.
pattern A064 :: Char
pattern A064 = '\x13049'

-- | The Egyptian hieroglyph /A065/ that renders as &#x1304a;.
pattern A065 :: Char
pattern A065 = '\x1304a'

-- | The Egyptian hieroglyph /A066/ that renders as &#x1304b;.
pattern A066 :: Char
pattern A066 = '\x1304b'

-- | The Egyptian hieroglyph /A067/ that renders as &#x1304c;.
pattern A067 :: Char
pattern A067 = '\x1304c'

-- | The Egyptian hieroglyph /A068/ that renders as &#x1304d;.
pattern A068 :: Char
pattern A068 = '\x1304d'

-- | The Egyptian hieroglyph /A069/ that renders as &#x1304e;.
pattern A069 :: Char
pattern A069 = '\x1304e'

-- | The Egyptian hieroglyph /A070/ that renders as &#x1304f;.
pattern A070 :: Char
pattern A070 = '\x1304f'

-- | The Egyptian hieroglyph /B001/ that renders as &#x13050;.
pattern B001 :: Char
pattern B001 = '\x13050'

-- | The Egyptian hieroglyph /B002/ that renders as &#x13051;.
pattern B002 :: Char
pattern B002 = '\x13051'

-- | The Egyptian hieroglyph /B003/ that renders as &#x13052;.
pattern B003 :: Char
pattern B003 = '\x13052'

-- | The Egyptian hieroglyph /B004/ that renders as &#x13053;.
pattern B004 :: Char
pattern B004 = '\x13053'

-- | The Egyptian hieroglyph /B005/ that renders as &#x13054;.
pattern B005 :: Char
pattern B005 = '\x13054'

-- | The Egyptian hieroglyph /B005A/ that renders as &#x13055;.
pattern B005A :: Char
pattern B005A = '\x13055'

-- | The Egyptian hieroglyph /B006/ that renders as &#x13056;.
pattern B006 :: Char
pattern B006 = '\x13056'

-- | The Egyptian hieroglyph /B007/ that renders as &#x13057;.
pattern B007 :: Char
pattern B007 = '\x13057'

-- | The Egyptian hieroglyph /B008/ that renders as &#x13058;.
pattern B008 :: Char
pattern B008 = '\x13058'

-- | The Egyptian hieroglyph /B009/ that renders as &#x13059;.
pattern B009 :: Char
pattern B009 = '\x13059'

-- | The Egyptian hieroglyph /C001/ that renders as &#x1305a;.
pattern C001 :: Char
pattern C001 = '\x1305a'

-- | The Egyptian hieroglyph /C002/ that renders as &#x1305b;.
pattern C002 :: Char
pattern C002 = '\x1305b'

-- | The Egyptian hieroglyph /C002A/ that renders as &#x1305c;.
pattern C002A :: Char
pattern C002A = '\x1305c'

-- | The Egyptian hieroglyph /C002B/ that renders as &#x1305d;.
pattern C002B :: Char
pattern C002B = '\x1305d'

-- | The Egyptian hieroglyph /C002C/ that renders as &#x1305e;.
pattern C002C :: Char
pattern C002C = '\x1305e'

-- | The Egyptian hieroglyph /C003/ that renders as &#x1305f;.
pattern C003 :: Char
pattern C003 = '\x1305f'

-- | The Egyptian hieroglyph /C004/ that renders as &#x13060;.
pattern C004 :: Char
pattern C004 = '\x13060'

-- | The Egyptian hieroglyph /C005/ that renders as &#x13061;.
pattern C005 :: Char
pattern C005 = '\x13061'

-- | The Egyptian hieroglyph /C006/ that renders as &#x13062;.
pattern C006 :: Char
pattern C006 = '\x13062'

-- | The Egyptian hieroglyph /C007/ that renders as &#x13063;.
pattern C007 :: Char
pattern C007 = '\x13063'

-- | The Egyptian hieroglyph /C008/ that renders as &#x13064;.
pattern C008 :: Char
pattern C008 = '\x13064'

-- | The Egyptian hieroglyph /C009/ that renders as &#x13065;.
pattern C009 :: Char
pattern C009 = '\x13065'

-- | The Egyptian hieroglyph /C010/ that renders as &#x13066;.
pattern C010 :: Char
pattern C010 = '\x13066'

-- | The Egyptian hieroglyph /C010A/ that renders as &#x13067;.
pattern C010A :: Char
pattern C010A = '\x13067'

-- | The Egyptian hieroglyph /C011/ that renders as &#x13068;.
pattern C011 :: Char
pattern C011 = '\x13068'

-- | The Egyptian hieroglyph /C012/ that renders as &#x13069;.
pattern C012 :: Char
pattern C012 = '\x13069'

-- | The Egyptian hieroglyph /C013/ that renders as &#x1306a;.
pattern C013 :: Char
pattern C013 = '\x1306a'

-- | The Egyptian hieroglyph /C014/ that renders as &#x1306b;.
pattern C014 :: Char
pattern C014 = '\x1306b'

-- | The Egyptian hieroglyph /C015/ that renders as &#x1306c;.
pattern C015 :: Char
pattern C015 = '\x1306c'

-- | The Egyptian hieroglyph /C016/ that renders as &#x1306d;.
pattern C016 :: Char
pattern C016 = '\x1306d'

-- | The Egyptian hieroglyph /C017/ that renders as &#x1306e;.
pattern C017 :: Char
pattern C017 = '\x1306e'

-- | The Egyptian hieroglyph /C018/ that renders as &#x1306f;.
pattern C018 :: Char
pattern C018 = '\x1306f'

-- | The Egyptian hieroglyph /C019/ that renders as &#x13070;.
pattern C019 :: Char
pattern C019 = '\x13070'

-- | The Egyptian hieroglyph /C020/ that renders as &#x13071;.
pattern C020 :: Char
pattern C020 = '\x13071'

-- | The Egyptian hieroglyph /C021/ that renders as &#x13072;.
pattern C021 :: Char
pattern C021 = '\x13072'

-- | The Egyptian hieroglyph /C022/ that renders as &#x13073;.
pattern C022 :: Char
pattern C022 = '\x13073'

-- | The Egyptian hieroglyph /C023/ that renders as &#x13074;.
pattern C023 :: Char
pattern C023 = '\x13074'

-- | The Egyptian hieroglyph /C024/ that renders as &#x13075;.
pattern C024 :: Char
pattern C024 = '\x13075'

-- | The Egyptian hieroglyph /D001/ that renders as &#x13076;.
pattern D001 :: Char
pattern D001 = '\x13076'

-- | The Egyptian hieroglyph /D002/ that renders as &#x13077;.
pattern D002 :: Char
pattern D002 = '\x13077'

-- | The Egyptian hieroglyph /D003/ that renders as &#x13078;.
pattern D003 :: Char
pattern D003 = '\x13078'

-- | The Egyptian hieroglyph /D004/ that renders as &#x13079;.
pattern D004 :: Char
pattern D004 = '\x13079'

-- | The Egyptian hieroglyph /D005/ that renders as &#x1307a;.
pattern D005 :: Char
pattern D005 = '\x1307a'

-- | The Egyptian hieroglyph /D006/ that renders as &#x1307b;.
pattern D006 :: Char
pattern D006 = '\x1307b'

-- | The Egyptian hieroglyph /D007/ that renders as &#x1307c;.
pattern D007 :: Char
pattern D007 = '\x1307c'

-- | The Egyptian hieroglyph /D008/ that renders as &#x1307d;.
pattern D008 :: Char
pattern D008 = '\x1307d'

-- | The Egyptian hieroglyph /D008A/ that renders as &#x1307e;.
pattern D008A :: Char
pattern D008A = '\x1307e'

-- | The Egyptian hieroglyph /D009/ that renders as &#x1307f;.
pattern D009 :: Char
pattern D009 = '\x1307f'

-- | The Egyptian hieroglyph /D010/ that renders as &#x13080;.
pattern D010 :: Char
pattern D010 = '\x13080'

-- | The Egyptian hieroglyph /D011/ that renders as &#x13081;.
pattern D011 :: Char
pattern D011 = '\x13081'

-- | The Egyptian hieroglyph /D012/ that renders as &#x13082;.
pattern D012 :: Char
pattern D012 = '\x13082'

-- | The Egyptian hieroglyph /D013/ that renders as &#x13083;.
pattern D013 :: Char
pattern D013 = '\x13083'

-- | The Egyptian hieroglyph /D014/ that renders as &#x13084;.
pattern D014 :: Char
pattern D014 = '\x13084'

-- | The Egyptian hieroglyph /D015/ that renders as &#x13085;.
pattern D015 :: Char
pattern D015 = '\x13085'

-- | The Egyptian hieroglyph /D016/ that renders as &#x13086;.
pattern D016 :: Char
pattern D016 = '\x13086'

-- | The Egyptian hieroglyph /D017/ that renders as &#x13087;.
pattern D017 :: Char
pattern D017 = '\x13087'

-- | The Egyptian hieroglyph /D018/ that renders as &#x13088;.
pattern D018 :: Char
pattern D018 = '\x13088'

-- | The Egyptian hieroglyph /D019/ that renders as &#x13089;.
pattern D019 :: Char
pattern D019 = '\x13089'

-- | The Egyptian hieroglyph /D020/ that renders as &#x1308a;.
pattern D020 :: Char
pattern D020 = '\x1308a'

-- | The Egyptian hieroglyph /D021/ that renders as &#x1308b;.
pattern D021 :: Char
pattern D021 = '\x1308b'

-- | The Egyptian hieroglyph /D022/ that renders as &#x1308c;.
pattern D022 :: Char
pattern D022 = '\x1308c'

-- | The Egyptian hieroglyph /D023/ that renders as &#x1308d;.
pattern D023 :: Char
pattern D023 = '\x1308d'

-- | The Egyptian hieroglyph /D024/ that renders as &#x1308e;.
pattern D024 :: Char
pattern D024 = '\x1308e'

-- | The Egyptian hieroglyph /D025/ that renders as &#x1308f;.
pattern D025 :: Char
pattern D025 = '\x1308f'

-- | The Egyptian hieroglyph /D026/ that renders as &#x13090;.
pattern D026 :: Char
pattern D026 = '\x13090'

-- | The Egyptian hieroglyph /D027/ that renders as &#x13091;.
pattern D027 :: Char
pattern D027 = '\x13091'

-- | The Egyptian hieroglyph /D027A/ that renders as &#x13092;.
pattern D027A :: Char
pattern D027A = '\x13092'

-- | The Egyptian hieroglyph /D028/ that renders as &#x13093;.
pattern D028 :: Char
pattern D028 = '\x13093'

-- | The Egyptian hieroglyph /D029/ that renders as &#x13094;.
pattern D029 :: Char
pattern D029 = '\x13094'

-- | The Egyptian hieroglyph /D030/ that renders as &#x13095;.
pattern D030 :: Char
pattern D030 = '\x13095'

-- | The Egyptian hieroglyph /D031/ that renders as &#x13096;.
pattern D031 :: Char
pattern D031 = '\x13096'

-- | The Egyptian hieroglyph /D031A/ that renders as &#x13097;.
pattern D031A :: Char
pattern D031A = '\x13097'

-- | The Egyptian hieroglyph /D032/ that renders as &#x13098;.
pattern D032 :: Char
pattern D032 = '\x13098'

-- | The Egyptian hieroglyph /D033/ that renders as &#x13099;.
pattern D033 :: Char
pattern D033 = '\x13099'

-- | The Egyptian hieroglyph /D034/ that renders as &#x1309a;.
pattern D034 :: Char
pattern D034 = '\x1309a'

-- | The Egyptian hieroglyph /D034A/ that renders as &#x1309b;.
pattern D034A :: Char
pattern D034A = '\x1309b'

-- | The Egyptian hieroglyph /D035/ that renders as &#x1309c;.
pattern D035 :: Char
pattern D035 = '\x1309c'

-- | The Egyptian hieroglyph /D036/ that renders as &#x1309d;.
pattern D036 :: Char
pattern D036 = '\x1309d'

-- | The Egyptian hieroglyph /D037/ that renders as &#x1309e;.
pattern D037 :: Char
pattern D037 = '\x1309e'

-- | The Egyptian hieroglyph /D038/ that renders as &#x1309f;.
pattern D038 :: Char
pattern D038 = '\x1309f'

-- | The Egyptian hieroglyph /D039/ that renders as &#x130a0;.
pattern D039 :: Char
pattern D039 = '\x130a0'

-- | The Egyptian hieroglyph /D040/ that renders as &#x130a1;.
pattern D040 :: Char
pattern D040 = '\x130a1'

-- | The Egyptian hieroglyph /D041/ that renders as &#x130a2;.
pattern D041 :: Char
pattern D041 = '\x130a2'

-- | The Egyptian hieroglyph /D042/ that renders as &#x130a3;.
pattern D042 :: Char
pattern D042 = '\x130a3'

-- | The Egyptian hieroglyph /D043/ that renders as &#x130a4;.
pattern D043 :: Char
pattern D043 = '\x130a4'

-- | The Egyptian hieroglyph /D044/ that renders as &#x130a5;.
pattern D044 :: Char
pattern D044 = '\x130a5'

-- | The Egyptian hieroglyph /D045/ that renders as &#x130a6;.
pattern D045 :: Char
pattern D045 = '\x130a6'

-- | The Egyptian hieroglyph /D046/ that renders as &#x130a7;.
pattern D046 :: Char
pattern D046 = '\x130a7'

-- | The Egyptian hieroglyph /D046A/ that renders as &#x130a8;.
pattern D046A :: Char
pattern D046A = '\x130a8'

-- | The Egyptian hieroglyph /D047/ that renders as &#x130a9;.
pattern D047 :: Char
pattern D047 = '\x130a9'

-- | The Egyptian hieroglyph /D048/ that renders as &#x130aa;.
pattern D048 :: Char
pattern D048 = '\x130aa'

-- | The Egyptian hieroglyph /D048A/ that renders as &#x130ab;.
pattern D048A :: Char
pattern D048A = '\x130ab'

-- | The Egyptian hieroglyph /D049/ that renders as &#x130ac;.
pattern D049 :: Char
pattern D049 = '\x130ac'

-- | The Egyptian hieroglyph /D050/ that renders as &#x130ad;.
pattern D050 :: Char
pattern D050 = '\x130ad'

-- | The Egyptian hieroglyph /D050A/ that renders as &#x130ae;.
pattern D050A :: Char
pattern D050A = '\x130ae'

-- | The Egyptian hieroglyph /D050B/ that renders as &#x130af;.
pattern D050B :: Char
pattern D050B = '\x130af'

-- | The Egyptian hieroglyph /D050C/ that renders as &#x130b0;.
pattern D050C :: Char
pattern D050C = '\x130b0'

-- | The Egyptian hieroglyph /D050D/ that renders as &#x130b1;.
pattern D050D :: Char
pattern D050D = '\x130b1'

-- | The Egyptian hieroglyph /D050E/ that renders as &#x130b2;.
pattern D050E :: Char
pattern D050E = '\x130b2'

-- | The Egyptian hieroglyph /D050F/ that renders as &#x130b3;.
pattern D050F :: Char
pattern D050F = '\x130b3'

-- | The Egyptian hieroglyph /D050G/ that renders as &#x130b4;.
pattern D050G :: Char
pattern D050G = '\x130b4'

-- | The Egyptian hieroglyph /D050H/ that renders as &#x130b5;.
pattern D050H :: Char
pattern D050H = '\x130b5'

-- | The Egyptian hieroglyph /D050I/ that renders as &#x130b6;.
pattern D050I :: Char
pattern D050I = '\x130b6'

-- | The Egyptian hieroglyph /D051/ that renders as &#x130b7;.
pattern D051 :: Char
pattern D051 = '\x130b7'

-- | The Egyptian hieroglyph /D052/ that renders as &#x130b8;.
pattern D052 :: Char
pattern D052 = '\x130b8'

-- | The Egyptian hieroglyph /D052A/ that renders as &#x130b9;.
pattern D052A :: Char
pattern D052A = '\x130b9'

-- | The Egyptian hieroglyph /D053/ that renders as &#x130ba;.
pattern D053 :: Char
pattern D053 = '\x130ba'

-- | The Egyptian hieroglyph /D054/ that renders as &#x130bb;.
pattern D054 :: Char
pattern D054 = '\x130bb'

-- | The Egyptian hieroglyph /D054A/ that renders as &#x130bc;.
pattern D054A :: Char
pattern D054A = '\x130bc'

-- | The Egyptian hieroglyph /D055/ that renders as &#x130bd;.
pattern D055 :: Char
pattern D055 = '\x130bd'

-- | The Egyptian hieroglyph /D056/ that renders as &#x130be;.
pattern D056 :: Char
pattern D056 = '\x130be'

-- | The Egyptian hieroglyph /D057/ that renders as &#x130bf;.
pattern D057 :: Char
pattern D057 = '\x130bf'

-- | The Egyptian hieroglyph /D058/ that renders as &#x130c0;.
pattern D058 :: Char
pattern D058 = '\x130c0'

-- | The Egyptian hieroglyph /D059/ that renders as &#x130c1;.
pattern D059 :: Char
pattern D059 = '\x130c1'

-- | The Egyptian hieroglyph /D060/ that renders as &#x130c2;.
pattern D060 :: Char
pattern D060 = '\x130c2'

-- | The Egyptian hieroglyph /D061/ that renders as &#x130c3;.
pattern D061 :: Char
pattern D061 = '\x130c3'

-- | The Egyptian hieroglyph /D062/ that renders as &#x130c4;.
pattern D062 :: Char
pattern D062 = '\x130c4'

-- | The Egyptian hieroglyph /D063/ that renders as &#x130c5;.
pattern D063 :: Char
pattern D063 = '\x130c5'

-- | The Egyptian hieroglyph /D064/ that renders as &#x130c6;.
pattern D064 :: Char
pattern D064 = '\x130c6'

-- | The Egyptian hieroglyph /D065/ that renders as &#x130c7;.
pattern D065 :: Char
pattern D065 = '\x130c7'

-- | The Egyptian hieroglyph /D066/ that renders as &#x130c8;.
pattern D066 :: Char
pattern D066 = '\x130c8'

-- | The Egyptian hieroglyph /D067/ that renders as &#x130c9;.
pattern D067 :: Char
pattern D067 = '\x130c9'

-- | The Egyptian hieroglyph /D067A/ that renders as &#x130ca;.
pattern D067A :: Char
pattern D067A = '\x130ca'

-- | The Egyptian hieroglyph /D067B/ that renders as &#x130cb;.
pattern D067B :: Char
pattern D067B = '\x130cb'

-- | The Egyptian hieroglyph /D067C/ that renders as &#x130cc;.
pattern D067C :: Char
pattern D067C = '\x130cc'

-- | The Egyptian hieroglyph /D067D/ that renders as &#x130cd;.
pattern D067D :: Char
pattern D067D = '\x130cd'

-- | The Egyptian hieroglyph /D067E/ that renders as &#x130ce;.
pattern D067E :: Char
pattern D067E = '\x130ce'

-- | The Egyptian hieroglyph /D067F/ that renders as &#x130cf;.
pattern D067F :: Char
pattern D067F = '\x130cf'

-- | The Egyptian hieroglyph /D067G/ that renders as &#x130d0;.
pattern D067G :: Char
pattern D067G = '\x130d0'

-- | The Egyptian hieroglyph /D067H/ that renders as &#x130d1;.
pattern D067H :: Char
pattern D067H = '\x130d1'

-- | The Egyptian hieroglyph /E001/ that renders as &#x130d2;.
pattern E001 :: Char
pattern E001 = '\x130d2'

-- | The Egyptian hieroglyph /E002/ that renders as &#x130d3;.
pattern E002 :: Char
pattern E002 = '\x130d3'

-- | The Egyptian hieroglyph /E003/ that renders as &#x130d4;.
pattern E003 :: Char
pattern E003 = '\x130d4'

-- | The Egyptian hieroglyph /E004/ that renders as &#x130d5;.
pattern E004 :: Char
pattern E004 = '\x130d5'

-- | The Egyptian hieroglyph /E005/ that renders as &#x130d6;.
pattern E005 :: Char
pattern E005 = '\x130d6'

-- | The Egyptian hieroglyph /E006/ that renders as &#x130d7;.
pattern E006 :: Char
pattern E006 = '\x130d7'

-- | The Egyptian hieroglyph /E007/ that renders as &#x130d8;.
pattern E007 :: Char
pattern E007 = '\x130d8'

-- | The Egyptian hieroglyph /E008/ that renders as &#x130d9;.
pattern E008 :: Char
pattern E008 = '\x130d9'

-- | The Egyptian hieroglyph /E008A/ that renders as &#x130da;.
pattern E008A :: Char
pattern E008A = '\x130da'

-- | The Egyptian hieroglyph /E009/ that renders as &#x130db;.
pattern E009 :: Char
pattern E009 = '\x130db'

-- | The Egyptian hieroglyph /E009A/ that renders as &#x130dc;.
pattern E009A :: Char
pattern E009A = '\x130dc'

-- | The Egyptian hieroglyph /E010/ that renders as &#x130dd;.
pattern E010 :: Char
pattern E010 = '\x130dd'

-- | The Egyptian hieroglyph /E011/ that renders as &#x130de;.
pattern E011 :: Char
pattern E011 = '\x130de'

-- | The Egyptian hieroglyph /E012/ that renders as &#x130df;.
pattern E012 :: Char
pattern E012 = '\x130df'

-- | The Egyptian hieroglyph /E013/ that renders as &#x130e0;.
pattern E013 :: Char
pattern E013 = '\x130e0'

-- | The Egyptian hieroglyph /E014/ that renders as &#x130e1;.
pattern E014 :: Char
pattern E014 = '\x130e1'

-- | The Egyptian hieroglyph /E015/ that renders as &#x130e2;.
pattern E015 :: Char
pattern E015 = '\x130e2'

-- | The Egyptian hieroglyph /E016/ that renders as &#x130e3;.
pattern E016 :: Char
pattern E016 = '\x130e3'

-- | The Egyptian hieroglyph /E016A/ that renders as &#x130e4;.
pattern E016A :: Char
pattern E016A = '\x130e4'

-- | The Egyptian hieroglyph /E017/ that renders as &#x130e5;.
pattern E017 :: Char
pattern E017 = '\x130e5'

-- | The Egyptian hieroglyph /E017A/ that renders as &#x130e6;.
pattern E017A :: Char
pattern E017A = '\x130e6'

-- | The Egyptian hieroglyph /E018/ that renders as &#x130e7;.
pattern E018 :: Char
pattern E018 = '\x130e7'

-- | The Egyptian hieroglyph /E019/ that renders as &#x130e8;.
pattern E019 :: Char
pattern E019 = '\x130e8'

-- | The Egyptian hieroglyph /E020/ that renders as &#x130e9;.
pattern E020 :: Char
pattern E020 = '\x130e9'

-- | The Egyptian hieroglyph /E020A/ that renders as &#x130ea;.
pattern E020A :: Char
pattern E020A = '\x130ea'

-- | The Egyptian hieroglyph /E021/ that renders as &#x130eb;.
pattern E021 :: Char
pattern E021 = '\x130eb'

-- | The Egyptian hieroglyph /E022/ that renders as &#x130ec;.
pattern E022 :: Char
pattern E022 = '\x130ec'

-- | The Egyptian hieroglyph /E023/ that renders as &#x130ed;.
pattern E023 :: Char
pattern E023 = '\x130ed'

-- | The Egyptian hieroglyph /E024/ that renders as &#x130ee;.
pattern E024 :: Char
pattern E024 = '\x130ee'

-- | The Egyptian hieroglyph /E025/ that renders as &#x130ef;.
pattern E025 :: Char
pattern E025 = '\x130ef'

-- | The Egyptian hieroglyph /E026/ that renders as &#x130f0;.
pattern E026 :: Char
pattern E026 = '\x130f0'

-- | The Egyptian hieroglyph /E027/ that renders as &#x130f1;.
pattern E027 :: Char
pattern E027 = '\x130f1'

-- | The Egyptian hieroglyph /E028/ that renders as &#x130f2;.
pattern E028 :: Char
pattern E028 = '\x130f2'

-- | The Egyptian hieroglyph /E028A/ that renders as &#x130f3;.
pattern E028A :: Char
pattern E028A = '\x130f3'

-- | The Egyptian hieroglyph /E029/ that renders as &#x130f4;.
pattern E029 :: Char
pattern E029 = '\x130f4'

-- | The Egyptian hieroglyph /E030/ that renders as &#x130f5;.
pattern E030 :: Char
pattern E030 = '\x130f5'

-- | The Egyptian hieroglyph /E031/ that renders as &#x130f6;.
pattern E031 :: Char
pattern E031 = '\x130f6'

-- | The Egyptian hieroglyph /E032/ that renders as &#x130f7;.
pattern E032 :: Char
pattern E032 = '\x130f7'

-- | The Egyptian hieroglyph /E033/ that renders as &#x130f8;.
pattern E033 :: Char
pattern E033 = '\x130f8'

-- | The Egyptian hieroglyph /E034/ that renders as &#x130f9;.
pattern E034 :: Char
pattern E034 = '\x130f9'

-- | The Egyptian hieroglyph /E034A/ that renders as &#x130fa;.
pattern E034A :: Char
pattern E034A = '\x130fa'

-- | The Egyptian hieroglyph /E036/ that renders as &#x130fb;.
pattern E036 :: Char
pattern E036 = '\x130fb'

-- | The Egyptian hieroglyph /E037/ that renders as &#x130fc;.
pattern E037 :: Char
pattern E037 = '\x130fc'

-- | The Egyptian hieroglyph /E038/ that renders as &#x130fd;.
pattern E038 :: Char
pattern E038 = '\x130fd'

-- | The Egyptian hieroglyph /F001/ that renders as &#x130fe;.
pattern F001 :: Char
pattern F001 = '\x130fe'

-- | The Egyptian hieroglyph /F001A/ that renders as &#x130ff;.
pattern F001A :: Char
pattern F001A = '\x130ff'

-- | The Egyptian hieroglyph /F002/ that renders as &#x13100;.
pattern F002 :: Char
pattern F002 = '\x13100'

-- | The Egyptian hieroglyph /F003/ that renders as &#x13101;.
pattern F003 :: Char
pattern F003 = '\x13101'

-- | The Egyptian hieroglyph /F004/ that renders as &#x13102;.
pattern F004 :: Char
pattern F004 = '\x13102'

-- | The Egyptian hieroglyph /F005/ that renders as &#x13103;.
pattern F005 :: Char
pattern F005 = '\x13103'

-- | The Egyptian hieroglyph /F006/ that renders as &#x13104;.
pattern F006 :: Char
pattern F006 = '\x13104'

-- | The Egyptian hieroglyph /F007/ that renders as &#x13105;.
pattern F007 :: Char
pattern F007 = '\x13105'

-- | The Egyptian hieroglyph /F008/ that renders as &#x13106;.
pattern F008 :: Char
pattern F008 = '\x13106'

-- | The Egyptian hieroglyph /F009/ that renders as &#x13107;.
pattern F009 :: Char
pattern F009 = '\x13107'

-- | The Egyptian hieroglyph /F010/ that renders as &#x13108;.
pattern F010 :: Char
pattern F010 = '\x13108'

-- | The Egyptian hieroglyph /F011/ that renders as &#x13109;.
pattern F011 :: Char
pattern F011 = '\x13109'

-- | The Egyptian hieroglyph /F012/ that renders as &#x1310a;.
pattern F012 :: Char
pattern F012 = '\x1310a'

-- | The Egyptian hieroglyph /F013/ that renders as &#x1310b;.
pattern F013 :: Char
pattern F013 = '\x1310b'

-- | The Egyptian hieroglyph /F013A/ that renders as &#x1310c;.
pattern F013A :: Char
pattern F013A = '\x1310c'

-- | The Egyptian hieroglyph /F014/ that renders as &#x1310d;.
pattern F014 :: Char
pattern F014 = '\x1310d'

-- | The Egyptian hieroglyph /F015/ that renders as &#x1310e;.
pattern F015 :: Char
pattern F015 = '\x1310e'

-- | The Egyptian hieroglyph /F016/ that renders as &#x1310f;.
pattern F016 :: Char
pattern F016 = '\x1310f'

-- | The Egyptian hieroglyph /F017/ that renders as &#x13110;.
pattern F017 :: Char
pattern F017 = '\x13110'

-- | The Egyptian hieroglyph /F018/ that renders as &#x13111;.
pattern F018 :: Char
pattern F018 = '\x13111'

-- | The Egyptian hieroglyph /F019/ that renders as &#x13112;.
pattern F019 :: Char
pattern F019 = '\x13112'

-- | The Egyptian hieroglyph /F020/ that renders as &#x13113;.
pattern F020 :: Char
pattern F020 = '\x13113'

-- | The Egyptian hieroglyph /F021/ that renders as &#x13114;.
pattern F021 :: Char
pattern F021 = '\x13114'

-- | The Egyptian hieroglyph /F021A/ that renders as &#x13115;.
pattern F021A :: Char
pattern F021A = '\x13115'

-- | The Egyptian hieroglyph /F022/ that renders as &#x13116;.
pattern F022 :: Char
pattern F022 = '\x13116'

-- | The Egyptian hieroglyph /F023/ that renders as &#x13117;.
pattern F023 :: Char
pattern F023 = '\x13117'

-- | The Egyptian hieroglyph /F024/ that renders as &#x13118;.
pattern F024 :: Char
pattern F024 = '\x13118'

-- | The Egyptian hieroglyph /F025/ that renders as &#x13119;.
pattern F025 :: Char
pattern F025 = '\x13119'

-- | The Egyptian hieroglyph /F026/ that renders as &#x1311a;.
pattern F026 :: Char
pattern F026 = '\x1311a'

-- | The Egyptian hieroglyph /F027/ that renders as &#x1311b;.
pattern F027 :: Char
pattern F027 = '\x1311b'

-- | The Egyptian hieroglyph /F028/ that renders as &#x1311c;.
pattern F028 :: Char
pattern F028 = '\x1311c'

-- | The Egyptian hieroglyph /F029/ that renders as &#x1311d;.
pattern F029 :: Char
pattern F029 = '\x1311d'

-- | The Egyptian hieroglyph /F030/ that renders as &#x1311e;.
pattern F030 :: Char
pattern F030 = '\x1311e'

-- | The Egyptian hieroglyph /F031/ that renders as &#x1311f;.
pattern F031 :: Char
pattern F031 = '\x1311f'

-- | The Egyptian hieroglyph /F031A/ that renders as &#x13120;.
pattern F031A :: Char
pattern F031A = '\x13120'

-- | The Egyptian hieroglyph /F032/ that renders as &#x13121;.
pattern F032 :: Char
pattern F032 = '\x13121'

-- | The Egyptian hieroglyph /F033/ that renders as &#x13122;.
pattern F033 :: Char
pattern F033 = '\x13122'

-- | The Egyptian hieroglyph /F034/ that renders as &#x13123;.
pattern F034 :: Char
pattern F034 = '\x13123'

-- | The Egyptian hieroglyph /F035/ that renders as &#x13124;.
pattern F035 :: Char
pattern F035 = '\x13124'

-- | The Egyptian hieroglyph /F036/ that renders as &#x13125;.
pattern F036 :: Char
pattern F036 = '\x13125'

-- | The Egyptian hieroglyph /F037/ that renders as &#x13126;.
pattern F037 :: Char
pattern F037 = '\x13126'

-- | The Egyptian hieroglyph /F037A/ that renders as &#x13127;.
pattern F037A :: Char
pattern F037A = '\x13127'

-- | The Egyptian hieroglyph /F038/ that renders as &#x13128;.
pattern F038 :: Char
pattern F038 = '\x13128'

-- | The Egyptian hieroglyph /F038A/ that renders as &#x13129;.
pattern F038A :: Char
pattern F038A = '\x13129'

-- | The Egyptian hieroglyph /F039/ that renders as &#x1312a;.
pattern F039 :: Char
pattern F039 = '\x1312a'

-- | The Egyptian hieroglyph /F040/ that renders as &#x1312b;.
pattern F040 :: Char
pattern F040 = '\x1312b'

-- | The Egyptian hieroglyph /F041/ that renders as &#x1312c;.
pattern F041 :: Char
pattern F041 = '\x1312c'

-- | The Egyptian hieroglyph /F042/ that renders as &#x1312d;.
pattern F042 :: Char
pattern F042 = '\x1312d'

-- | The Egyptian hieroglyph /F043/ that renders as &#x1312e;.
pattern F043 :: Char
pattern F043 = '\x1312e'

-- | The Egyptian hieroglyph /F044/ that renders as &#x1312f;.
pattern F044 :: Char
pattern F044 = '\x1312f'

-- | The Egyptian hieroglyph /F045/ that renders as &#x13130;.
pattern F045 :: Char
pattern F045 = '\x13130'

-- | The Egyptian hieroglyph /F045A/ that renders as &#x13131;.
pattern F045A :: Char
pattern F045A = '\x13131'

-- | The Egyptian hieroglyph /F046/ that renders as &#x13132;.
pattern F046 :: Char
pattern F046 = '\x13132'

-- | The Egyptian hieroglyph /F046A/ that renders as &#x13133;.
pattern F046A :: Char
pattern F046A = '\x13133'

-- | The Egyptian hieroglyph /F047/ that renders as &#x13134;.
pattern F047 :: Char
pattern F047 = '\x13134'

-- | The Egyptian hieroglyph /F047A/ that renders as &#x13135;.
pattern F047A :: Char
pattern F047A = '\x13135'

-- | The Egyptian hieroglyph /F048/ that renders as &#x13136;.
pattern F048 :: Char
pattern F048 = '\x13136'

-- | The Egyptian hieroglyph /F049/ that renders as &#x13137;.
pattern F049 :: Char
pattern F049 = '\x13137'

-- | The Egyptian hieroglyph /F050/ that renders as &#x13138;.
pattern F050 :: Char
pattern F050 = '\x13138'

-- | The Egyptian hieroglyph /F051/ that renders as &#x13139;.
pattern F051 :: Char
pattern F051 = '\x13139'

-- | The Egyptian hieroglyph /F051A/ that renders as &#x1313a;.
pattern F051A :: Char
pattern F051A = '\x1313a'

-- | The Egyptian hieroglyph /F051B/ that renders as &#x1313b;.
pattern F051B :: Char
pattern F051B = '\x1313b'

-- | The Egyptian hieroglyph /F051C/ that renders as &#x1313c;.
pattern F051C :: Char
pattern F051C = '\x1313c'

-- | The Egyptian hieroglyph /F052/ that renders as &#x1313d;.
pattern F052 :: Char
pattern F052 = '\x1313d'

-- | The Egyptian hieroglyph /F053/ that renders as &#x1313e;.
pattern F053 :: Char
pattern F053 = '\x1313e'

-- | The Egyptian hieroglyph /G001/ that renders as &#x1313f;.
pattern G001 :: Char
pattern G001 = '\x1313f'

-- | The Egyptian hieroglyph /G002/ that renders as &#x13140;.
pattern G002 :: Char
pattern G002 = '\x13140'

-- | The Egyptian hieroglyph /G003/ that renders as &#x13141;.
pattern G003 :: Char
pattern G003 = '\x13141'

-- | The Egyptian hieroglyph /G004/ that renders as &#x13142;.
pattern G004 :: Char
pattern G004 = '\x13142'

-- | The Egyptian hieroglyph /G005/ that renders as &#x13143;.
pattern G005 :: Char
pattern G005 = '\x13143'

-- | The Egyptian hieroglyph /G006/ that renders as &#x13144;.
pattern G006 :: Char
pattern G006 = '\x13144'

-- | The Egyptian hieroglyph /G006A/ that renders as &#x13145;.
pattern G006A :: Char
pattern G006A = '\x13145'

-- | The Egyptian hieroglyph /G007/ that renders as &#x13146;.
pattern G007 :: Char
pattern G007 = '\x13146'

-- | The Egyptian hieroglyph /G007A/ that renders as &#x13147;.
pattern G007A :: Char
pattern G007A = '\x13147'

-- | The Egyptian hieroglyph /G007B/ that renders as &#x13148;.
pattern G007B :: Char
pattern G007B = '\x13148'

-- | The Egyptian hieroglyph /G008/ that renders as &#x13149;.
pattern G008 :: Char
pattern G008 = '\x13149'

-- | The Egyptian hieroglyph /G009/ that renders as &#x1314a;.
pattern G009 :: Char
pattern G009 = '\x1314a'

-- | The Egyptian hieroglyph /G010/ that renders as &#x1314b;.
pattern G010 :: Char
pattern G010 = '\x1314b'

-- | The Egyptian hieroglyph /G011/ that renders as &#x1314c;.
pattern G011 :: Char
pattern G011 = '\x1314c'

-- | The Egyptian hieroglyph /G011A/ that renders as &#x1314d;.
pattern G011A :: Char
pattern G011A = '\x1314d'

-- | The Egyptian hieroglyph /G012/ that renders as &#x1314e;.
pattern G012 :: Char
pattern G012 = '\x1314e'

-- | The Egyptian hieroglyph /G013/ that renders as &#x1314f;.
pattern G013 :: Char
pattern G013 = '\x1314f'

-- | The Egyptian hieroglyph /G014/ that renders as &#x13150;.
pattern G014 :: Char
pattern G014 = '\x13150'

-- | The Egyptian hieroglyph /G015/ that renders as &#x13151;.
pattern G015 :: Char
pattern G015 = '\x13151'

-- | The Egyptian hieroglyph /G016/ that renders as &#x13152;.
pattern G016 :: Char
pattern G016 = '\x13152'

-- | The Egyptian hieroglyph /G017/ that renders as &#x13153;.
pattern G017 :: Char
pattern G017 = '\x13153'

-- | The Egyptian hieroglyph /G018/ that renders as &#x13154;.
pattern G018 :: Char
pattern G018 = '\x13154'

-- | The Egyptian hieroglyph /G019/ that renders as &#x13155;.
pattern G019 :: Char
pattern G019 = '\x13155'

-- | The Egyptian hieroglyph /G020/ that renders as &#x13156;.
pattern G020 :: Char
pattern G020 = '\x13156'

-- | The Egyptian hieroglyph /G020A/ that renders as &#x13157;.
pattern G020A :: Char
pattern G020A = '\x13157'

-- | The Egyptian hieroglyph /G021/ that renders as &#x13158;.
pattern G021 :: Char
pattern G021 = '\x13158'

-- | The Egyptian hieroglyph /G022/ that renders as &#x13159;.
pattern G022 :: Char
pattern G022 = '\x13159'

-- | The Egyptian hieroglyph /G023/ that renders as &#x1315a;.
pattern G023 :: Char
pattern G023 = '\x1315a'

-- | The Egyptian hieroglyph /G024/ that renders as &#x1315b;.
pattern G024 :: Char
pattern G024 = '\x1315b'

-- | The Egyptian hieroglyph /G025/ that renders as &#x1315c;.
pattern G025 :: Char
pattern G025 = '\x1315c'

-- | The Egyptian hieroglyph /G026/ that renders as &#x1315d;.
pattern G026 :: Char
pattern G026 = '\x1315d'

-- | The Egyptian hieroglyph /G026A/ that renders as &#x1315e;.
pattern G026A :: Char
pattern G026A = '\x1315e'

-- | The Egyptian hieroglyph /G027/ that renders as &#x1315f;.
pattern G027 :: Char
pattern G027 = '\x1315f'

-- | The Egyptian hieroglyph /G028/ that renders as &#x13160;.
pattern G028 :: Char
pattern G028 = '\x13160'

-- | The Egyptian hieroglyph /G029/ that renders as &#x13161;.
pattern G029 :: Char
pattern G029 = '\x13161'

-- | The Egyptian hieroglyph /G030/ that renders as &#x13162;.
pattern G030 :: Char
pattern G030 = '\x13162'

-- | The Egyptian hieroglyph /G031/ that renders as &#x13163;.
pattern G031 :: Char
pattern G031 = '\x13163'

-- | The Egyptian hieroglyph /G032/ that renders as &#x13164;.
pattern G032 :: Char
pattern G032 = '\x13164'

-- | The Egyptian hieroglyph /G033/ that renders as &#x13165;.
pattern G033 :: Char
pattern G033 = '\x13165'

-- | The Egyptian hieroglyph /G034/ that renders as &#x13166;.
pattern G034 :: Char
pattern G034 = '\x13166'

-- | The Egyptian hieroglyph /G035/ that renders as &#x13167;.
pattern G035 :: Char
pattern G035 = '\x13167'

-- | The Egyptian hieroglyph /G036/ that renders as &#x13168;.
pattern G036 :: Char
pattern G036 = '\x13168'

-- | The Egyptian hieroglyph /G036A/ that renders as &#x13169;.
pattern G036A :: Char
pattern G036A = '\x13169'

-- | The Egyptian hieroglyph /G037/ that renders as &#x1316a;.
pattern G037 :: Char
pattern G037 = '\x1316a'

-- | The Egyptian hieroglyph /G037A/ that renders as &#x1316b;.
pattern G037A :: Char
pattern G037A = '\x1316b'

-- | The Egyptian hieroglyph /G038/ that renders as &#x1316c;.
pattern G038 :: Char
pattern G038 = '\x1316c'

-- | The Egyptian hieroglyph /G039/ that renders as &#x1316d;.
pattern G039 :: Char
pattern G039 = '\x1316d'

-- | The Egyptian hieroglyph /G040/ that renders as &#x1316e;.
pattern G040 :: Char
pattern G040 = '\x1316e'

-- | The Egyptian hieroglyph /G041/ that renders as &#x1316f;.
pattern G041 :: Char
pattern G041 = '\x1316f'

-- | The Egyptian hieroglyph /G042/ that renders as &#x13170;.
pattern G042 :: Char
pattern G042 = '\x13170'

-- | The Egyptian hieroglyph /G043/ that renders as &#x13171;.
pattern G043 :: Char
pattern G043 = '\x13171'

-- | The Egyptian hieroglyph /G043A/ that renders as &#x13172;.
pattern G043A :: Char
pattern G043A = '\x13172'

-- | The Egyptian hieroglyph /G044/ that renders as &#x13173;.
pattern G044 :: Char
pattern G044 = '\x13173'

-- | The Egyptian hieroglyph /G045/ that renders as &#x13174;.
pattern G045 :: Char
pattern G045 = '\x13174'

-- | The Egyptian hieroglyph /G045A/ that renders as &#x13175;.
pattern G045A :: Char
pattern G045A = '\x13175'

-- | The Egyptian hieroglyph /G046/ that renders as &#x13176;.
pattern G046 :: Char
pattern G046 = '\x13176'

-- | The Egyptian hieroglyph /G047/ that renders as &#x13177;.
pattern G047 :: Char
pattern G047 = '\x13177'

-- | The Egyptian hieroglyph /G048/ that renders as &#x13178;.
pattern G048 :: Char
pattern G048 = '\x13178'

-- | The Egyptian hieroglyph /G049/ that renders as &#x13179;.
pattern G049 :: Char
pattern G049 = '\x13179'

-- | The Egyptian hieroglyph /G050/ that renders as &#x1317a;.
pattern G050 :: Char
pattern G050 = '\x1317a'

-- | The Egyptian hieroglyph /G051/ that renders as &#x1317b;.
pattern G051 :: Char
pattern G051 = '\x1317b'

-- | The Egyptian hieroglyph /G052/ that renders as &#x1317c;.
pattern G052 :: Char
pattern G052 = '\x1317c'

-- | The Egyptian hieroglyph /G053/ that renders as &#x1317d;.
pattern G053 :: Char
pattern G053 = '\x1317d'

-- | The Egyptian hieroglyph /G054/ that renders as &#x1317e;.
pattern G054 :: Char
pattern G054 = '\x1317e'

-- | The Egyptian hieroglyph /H001/ that renders as &#x1317f;.
pattern H001 :: Char
pattern H001 = '\x1317f'

-- | The Egyptian hieroglyph /H002/ that renders as &#x13180;.
pattern H002 :: Char
pattern H002 = '\x13180'

-- | The Egyptian hieroglyph /H003/ that renders as &#x13181;.
pattern H003 :: Char
pattern H003 = '\x13181'

-- | The Egyptian hieroglyph /H004/ that renders as &#x13182;.
pattern H004 :: Char
pattern H004 = '\x13182'

-- | The Egyptian hieroglyph /H005/ that renders as &#x13183;.
pattern H005 :: Char
pattern H005 = '\x13183'

-- | The Egyptian hieroglyph /H006/ that renders as &#x13184;.
pattern H006 :: Char
pattern H006 = '\x13184'

-- | The Egyptian hieroglyph /H006A/ that renders as &#x13185;.
pattern H006A :: Char
pattern H006A = '\x13185'

-- | The Egyptian hieroglyph /H007/ that renders as &#x13186;.
pattern H007 :: Char
pattern H007 = '\x13186'

-- | The Egyptian hieroglyph /H008/ that renders as &#x13187;.
pattern H008 :: Char
pattern H008 = '\x13187'

-- | The Egyptian hieroglyph /I001/ that renders as &#x13188;.
pattern I001 :: Char
pattern I001 = '\x13188'

-- | The Egyptian hieroglyph /I002/ that renders as &#x13189;.
pattern I002 :: Char
pattern I002 = '\x13189'

-- | The Egyptian hieroglyph /I003/ that renders as &#x1318a;.
pattern I003 :: Char
pattern I003 = '\x1318a'

-- | The Egyptian hieroglyph /I004/ that renders as &#x1318b;.
pattern I004 :: Char
pattern I004 = '\x1318b'

-- | The Egyptian hieroglyph /I005/ that renders as &#x1318c;.
pattern I005 :: Char
pattern I005 = '\x1318c'

-- | The Egyptian hieroglyph /I005A/ that renders as &#x1318d;.
pattern I005A :: Char
pattern I005A = '\x1318d'

-- | The Egyptian hieroglyph /I006/ that renders as &#x1318e;.
pattern I006 :: Char
pattern I006 = '\x1318e'

-- | The Egyptian hieroglyph /I007/ that renders as &#x1318f;.
pattern I007 :: Char
pattern I007 = '\x1318f'

-- | The Egyptian hieroglyph /I008/ that renders as &#x13190;.
pattern I008 :: Char
pattern I008 = '\x13190'

-- | The Egyptian hieroglyph /I009/ that renders as &#x13191;.
pattern I009 :: Char
pattern I009 = '\x13191'

-- | The Egyptian hieroglyph /I009A/ that renders as &#x13192;.
pattern I009A :: Char
pattern I009A = '\x13192'

-- | The Egyptian hieroglyph /I010/ that renders as &#x13193;.
pattern I010 :: Char
pattern I010 = '\x13193'

-- | The Egyptian hieroglyph /I010A/ that renders as &#x13194;.
pattern I010A :: Char
pattern I010A = '\x13194'

-- | The Egyptian hieroglyph /I011/ that renders as &#x13195;.
pattern I011 :: Char
pattern I011 = '\x13195'

-- | The Egyptian hieroglyph /I011A/ that renders as &#x13196;.
pattern I011A :: Char
pattern I011A = '\x13196'

-- | The Egyptian hieroglyph /I012/ that renders as &#x13197;.
pattern I012 :: Char
pattern I012 = '\x13197'

-- | The Egyptian hieroglyph /I013/ that renders as &#x13198;.
pattern I013 :: Char
pattern I013 = '\x13198'

-- | The Egyptian hieroglyph /I014/ that renders as &#x13199;.
pattern I014 :: Char
pattern I014 = '\x13199'

-- | The Egyptian hieroglyph /I015/ that renders as &#x1319a;.
pattern I015 :: Char
pattern I015 = '\x1319a'

-- | The Egyptian hieroglyph /K001/ that renders as &#x1319b;.
pattern K001 :: Char
pattern K001 = '\x1319b'

-- | The Egyptian hieroglyph /K002/ that renders as &#x1319c;.
pattern K002 :: Char
pattern K002 = '\x1319c'

-- | The Egyptian hieroglyph /K003/ that renders as &#x1319d;.
pattern K003 :: Char
pattern K003 = '\x1319d'

-- | The Egyptian hieroglyph /K004/ that renders as &#x1319e;.
pattern K004 :: Char
pattern K004 = '\x1319e'

-- | The Egyptian hieroglyph /K005/ that renders as &#x1319f;.
pattern K005 :: Char
pattern K005 = '\x1319f'

-- | The Egyptian hieroglyph /K006/ that renders as &#x131a0;.
pattern K006 :: Char
pattern K006 = '\x131a0'

-- | The Egyptian hieroglyph /K007/ that renders as &#x131a1;.
pattern K007 :: Char
pattern K007 = '\x131a1'

-- | The Egyptian hieroglyph /K008/ that renders as &#x131a2;.
pattern K008 :: Char
pattern K008 = '\x131a2'

-- | The Egyptian hieroglyph /L001/ that renders as &#x131a3;.
pattern L001 :: Char
pattern L001 = '\x131a3'

-- | The Egyptian hieroglyph /L002/ that renders as &#x131a4;.
pattern L002 :: Char
pattern L002 = '\x131a4'

-- | The Egyptian hieroglyph /L002A/ that renders as &#x131a5;.
pattern L002A :: Char
pattern L002A = '\x131a5'

-- | The Egyptian hieroglyph /L003/ that renders as &#x131a6;.
pattern L003 :: Char
pattern L003 = '\x131a6'

-- | The Egyptian hieroglyph /L004/ that renders as &#x131a7;.
pattern L004 :: Char
pattern L004 = '\x131a7'

-- | The Egyptian hieroglyph /L005/ that renders as &#x131a8;.
pattern L005 :: Char
pattern L005 = '\x131a8'

-- | The Egyptian hieroglyph /L006/ that renders as &#x131a9;.
pattern L006 :: Char
pattern L006 = '\x131a9'

-- | The Egyptian hieroglyph /L006A/ that renders as &#x131aa;.
pattern L006A :: Char
pattern L006A = '\x131aa'

-- | The Egyptian hieroglyph /L007/ that renders as &#x131ab;.
pattern L007 :: Char
pattern L007 = '\x131ab'

-- | The Egyptian hieroglyph /L008/ that renders as &#x131ac;.
pattern L008 :: Char
pattern L008 = '\x131ac'

-- | The Egyptian hieroglyph /M001/ that renders as &#x131ad;.
pattern M001 :: Char
pattern M001 = '\x131ad'

-- | The Egyptian hieroglyph /M001A/ that renders as &#x131ae;.
pattern M001A :: Char
pattern M001A = '\x131ae'

-- | The Egyptian hieroglyph /M001B/ that renders as &#x131af;.
pattern M001B :: Char
pattern M001B = '\x131af'

-- | The Egyptian hieroglyph /M002/ that renders as &#x131b0;.
pattern M002 :: Char
pattern M002 = '\x131b0'

-- | The Egyptian hieroglyph /M003/ that renders as &#x131b1;.
pattern M003 :: Char
pattern M003 = '\x131b1'

-- | The Egyptian hieroglyph /M003A/ that renders as &#x131b2;.
pattern M003A :: Char
pattern M003A = '\x131b2'

-- | The Egyptian hieroglyph /M004/ that renders as &#x131b3;.
pattern M004 :: Char
pattern M004 = '\x131b3'

-- | The Egyptian hieroglyph /M005/ that renders as &#x131b4;.
pattern M005 :: Char
pattern M005 = '\x131b4'

-- | The Egyptian hieroglyph /M006/ that renders as &#x131b5;.
pattern M006 :: Char
pattern M006 = '\x131b5'

-- | The Egyptian hieroglyph /M007/ that renders as &#x131b6;.
pattern M007 :: Char
pattern M007 = '\x131b6'

-- | The Egyptian hieroglyph /M008/ that renders as &#x131b7;.
pattern M008 :: Char
pattern M008 = '\x131b7'

-- | The Egyptian hieroglyph /M009/ that renders as &#x131b8;.
pattern M009 :: Char
pattern M009 = '\x131b8'

-- | The Egyptian hieroglyph /M010/ that renders as &#x131b9;.
pattern M010 :: Char
pattern M010 = '\x131b9'

-- | The Egyptian hieroglyph /M010A/ that renders as &#x131ba;.
pattern M010A :: Char
pattern M010A = '\x131ba'

-- | The Egyptian hieroglyph /M011/ that renders as &#x131bb;.
pattern M011 :: Char
pattern M011 = '\x131bb'

-- | The Egyptian hieroglyph /M012/ that renders as &#x131bc;.
pattern M012 :: Char
pattern M012 = '\x131bc'

-- | The Egyptian hieroglyph /M012A/ that renders as &#x131bd;.
pattern M012A :: Char
pattern M012A = '\x131bd'

-- | The Egyptian hieroglyph /M012B/ that renders as &#x131be;.
pattern M012B :: Char
pattern M012B = '\x131be'

-- | The Egyptian hieroglyph /M012C/ that renders as &#x131bf;.
pattern M012C :: Char
pattern M012C = '\x131bf'

-- | The Egyptian hieroglyph /M012D/ that renders as &#x131c0;.
pattern M012D :: Char
pattern M012D = '\x131c0'

-- | The Egyptian hieroglyph /M012E/ that renders as &#x131c1;.
pattern M012E :: Char
pattern M012E = '\x131c1'

-- | The Egyptian hieroglyph /M012F/ that renders as &#x131c2;.
pattern M012F :: Char
pattern M012F = '\x131c2'

-- | The Egyptian hieroglyph /M012G/ that renders as &#x131c3;.
pattern M012G :: Char
pattern M012G = '\x131c3'

-- | The Egyptian hieroglyph /M012H/ that renders as &#x131c4;.
pattern M012H :: Char
pattern M012H = '\x131c4'

-- | The Egyptian hieroglyph /M013/ that renders as &#x131c5;.
pattern M013 :: Char
pattern M013 = '\x131c5'

-- | The Egyptian hieroglyph /M014/ that renders as &#x131c6;.
pattern M014 :: Char
pattern M014 = '\x131c6'

-- | The Egyptian hieroglyph /M015/ that renders as &#x131c7;.
pattern M015 :: Char
pattern M015 = '\x131c7'

-- | The Egyptian hieroglyph /M015A/ that renders as &#x131c8;.
pattern M015A :: Char
pattern M015A = '\x131c8'

-- | The Egyptian hieroglyph /M016/ that renders as &#x131c9;.
pattern M016 :: Char
pattern M016 = '\x131c9'

-- | The Egyptian hieroglyph /M016A/ that renders as &#x131ca;.
pattern M016A :: Char
pattern M016A = '\x131ca'

-- | The Egyptian hieroglyph /M017/ that renders as &#x131cb;.
pattern M017 :: Char
pattern M017 = '\x131cb'

-- | The Egyptian hieroglyph /M017A/ that renders as &#x131cc;.
pattern M017A :: Char
pattern M017A = '\x131cc'

-- | The Egyptian hieroglyph /M018/ that renders as &#x131cd;.
pattern M018 :: Char
pattern M018 = '\x131cd'

-- | The Egyptian hieroglyph /M019/ that renders as &#x131ce;.
pattern M019 :: Char
pattern M019 = '\x131ce'

-- | The Egyptian hieroglyph /M020/ that renders as &#x131cf;.
pattern M020 :: Char
pattern M020 = '\x131cf'

-- | The Egyptian hieroglyph /M021/ that renders as &#x131d0;.
pattern M021 :: Char
pattern M021 = '\x131d0'

-- | The Egyptian hieroglyph /M022/ that renders as &#x131d1;.
pattern M022 :: Char
pattern M022 = '\x131d1'

-- | The Egyptian hieroglyph /M022A/ that renders as &#x131d2;.
pattern M022A :: Char
pattern M022A = '\x131d2'

-- | The Egyptian hieroglyph /M023/ that renders as &#x131d3;.
pattern M023 :: Char
pattern M023 = '\x131d3'

-- | The Egyptian hieroglyph /M024/ that renders as &#x131d4;.
pattern M024 :: Char
pattern M024 = '\x131d4'

-- | The Egyptian hieroglyph /M024A/ that renders as &#x131d5;.
pattern M024A :: Char
pattern M024A = '\x131d5'

-- | The Egyptian hieroglyph /M025/ that renders as &#x131d6;.
pattern M025 :: Char
pattern M025 = '\x131d6'

-- | The Egyptian hieroglyph /M026/ that renders as &#x131d7;.
pattern M026 :: Char
pattern M026 = '\x131d7'

-- | The Egyptian hieroglyph /M027/ that renders as &#x131d8;.
pattern M027 :: Char
pattern M027 = '\x131d8'

-- | The Egyptian hieroglyph /M028/ that renders as &#x131d9;.
pattern M028 :: Char
pattern M028 = '\x131d9'

-- | The Egyptian hieroglyph /M028A/ that renders as &#x131da;.
pattern M028A :: Char
pattern M028A = '\x131da'

-- | The Egyptian hieroglyph /M029/ that renders as &#x131db;.
pattern M029 :: Char
pattern M029 = '\x131db'

-- | The Egyptian hieroglyph /M030/ that renders as &#x131dc;.
pattern M030 :: Char
pattern M030 = '\x131dc'

-- | The Egyptian hieroglyph /M031/ that renders as &#x131dd;.
pattern M031 :: Char
pattern M031 = '\x131dd'

-- | The Egyptian hieroglyph /M031A/ that renders as &#x131de;.
pattern M031A :: Char
pattern M031A = '\x131de'

-- | The Egyptian hieroglyph /M032/ that renders as &#x131df;.
pattern M032 :: Char
pattern M032 = '\x131df'

-- | The Egyptian hieroglyph /M033/ that renders as &#x131e0;.
pattern M033 :: Char
pattern M033 = '\x131e0'

-- | The Egyptian hieroglyph /M033A/ that renders as &#x131e1;.
pattern M033A :: Char
pattern M033A = '\x131e1'

-- | The Egyptian hieroglyph /M033B/ that renders as &#x131e2;.
pattern M033B :: Char
pattern M033B = '\x131e2'

-- | The Egyptian hieroglyph /M034/ that renders as &#x131e3;.
pattern M034 :: Char
pattern M034 = '\x131e3'

-- | The Egyptian hieroglyph /M035/ that renders as &#x131e4;.
pattern M035 :: Char
pattern M035 = '\x131e4'

-- | The Egyptian hieroglyph /M036/ that renders as &#x131e5;.
pattern M036 :: Char
pattern M036 = '\x131e5'

-- | The Egyptian hieroglyph /M037/ that renders as &#x131e6;.
pattern M037 :: Char
pattern M037 = '\x131e6'

-- | The Egyptian hieroglyph /M038/ that renders as &#x131e7;.
pattern M038 :: Char
pattern M038 = '\x131e7'

-- | The Egyptian hieroglyph /M039/ that renders as &#x131e8;.
pattern M039 :: Char
pattern M039 = '\x131e8'

-- | The Egyptian hieroglyph /M040/ that renders as &#x131e9;.
pattern M040 :: Char
pattern M040 = '\x131e9'

-- | The Egyptian hieroglyph /M040A/ that renders as &#x131ea;.
pattern M040A :: Char
pattern M040A = '\x131ea'

-- | The Egyptian hieroglyph /M041/ that renders as &#x131eb;.
pattern M041 :: Char
pattern M041 = '\x131eb'

-- | The Egyptian hieroglyph /M042/ that renders as &#x131ec;.
pattern M042 :: Char
pattern M042 = '\x131ec'

-- | The Egyptian hieroglyph /M043/ that renders as &#x131ed;.
pattern M043 :: Char
pattern M043 = '\x131ed'

-- | The Egyptian hieroglyph /M044/ that renders as &#x131ee;.
pattern M044 :: Char
pattern M044 = '\x131ee'

-- | The Egyptian hieroglyph /N001/ that renders as &#x131ef;.
pattern N001 :: Char
pattern N001 = '\x131ef'

-- | The Egyptian hieroglyph /N002/ that renders as &#x131f0;.
pattern N002 :: Char
pattern N002 = '\x131f0'

-- | The Egyptian hieroglyph /N003/ that renders as &#x131f1;.
pattern N003 :: Char
pattern N003 = '\x131f1'

-- | The Egyptian hieroglyph /N004/ that renders as &#x131f2;.
pattern N004 :: Char
pattern N004 = '\x131f2'

-- | The Egyptian hieroglyph /N005/ that renders as &#x131f3;.
pattern N005 :: Char
pattern N005 = '\x131f3'

-- | The Egyptian hieroglyph /N006/ that renders as &#x131f4;.
pattern N006 :: Char
pattern N006 = '\x131f4'

-- | The Egyptian hieroglyph /N007/ that renders as &#x131f5;.
pattern N007 :: Char
pattern N007 = '\x131f5'

-- | The Egyptian hieroglyph /N008/ that renders as &#x131f6;.
pattern N008 :: Char
pattern N008 = '\x131f6'

-- | The Egyptian hieroglyph /N009/ that renders as &#x131f7;.
pattern N009 :: Char
pattern N009 = '\x131f7'

-- | The Egyptian hieroglyph /N010/ that renders as &#x131f8;.
pattern N010 :: Char
pattern N010 = '\x131f8'

-- | The Egyptian hieroglyph /N011/ that renders as &#x131f9;.
pattern N011 :: Char
pattern N011 = '\x131f9'

-- | The Egyptian hieroglyph /N012/ that renders as &#x131fa;.
pattern N012 :: Char
pattern N012 = '\x131fa'

-- | The Egyptian hieroglyph /N013/ that renders as &#x131fb;.
pattern N013 :: Char
pattern N013 = '\x131fb'

-- | The Egyptian hieroglyph /N014/ that renders as &#x131fc;.
pattern N014 :: Char
pattern N014 = '\x131fc'

-- | The Egyptian hieroglyph /N015/ that renders as &#x131fd;.
pattern N015 :: Char
pattern N015 = '\x131fd'

-- | The Egyptian hieroglyph /N016/ that renders as &#x131fe;.
pattern N016 :: Char
pattern N016 = '\x131fe'

-- | The Egyptian hieroglyph /N017/ that renders as &#x131ff;.
pattern N017 :: Char
pattern N017 = '\x131ff'

-- | The Egyptian hieroglyph /N018/ that renders as &#x13200;.
pattern N018 :: Char
pattern N018 = '\x13200'

-- | The Egyptian hieroglyph /N018A/ that renders as &#x13201;.
pattern N018A :: Char
pattern N018A = '\x13201'

-- | The Egyptian hieroglyph /N018B/ that renders as &#x13202;.
pattern N018B :: Char
pattern N018B = '\x13202'

-- | The Egyptian hieroglyph /N019/ that renders as &#x13203;.
pattern N019 :: Char
pattern N019 = '\x13203'

-- | The Egyptian hieroglyph /N020/ that renders as &#x13204;.
pattern N020 :: Char
pattern N020 = '\x13204'

-- | The Egyptian hieroglyph /N021/ that renders as &#x13205;.
pattern N021 :: Char
pattern N021 = '\x13205'

-- | The Egyptian hieroglyph /N022/ that renders as &#x13206;.
pattern N022 :: Char
pattern N022 = '\x13206'

-- | The Egyptian hieroglyph /N023/ that renders as &#x13207;.
pattern N023 :: Char
pattern N023 = '\x13207'

-- | The Egyptian hieroglyph /N024/ that renders as &#x13208;.
pattern N024 :: Char
pattern N024 = '\x13208'

-- | The Egyptian hieroglyph /N025/ that renders as &#x13209;.
pattern N025 :: Char
pattern N025 = '\x13209'

-- | The Egyptian hieroglyph /N025A/ that renders as &#x1320a;.
pattern N025A :: Char
pattern N025A = '\x1320a'

-- | The Egyptian hieroglyph /N026/ that renders as &#x1320b;.
pattern N026 :: Char
pattern N026 = '\x1320b'

-- | The Egyptian hieroglyph /N027/ that renders as &#x1320c;.
pattern N027 :: Char
pattern N027 = '\x1320c'

-- | The Egyptian hieroglyph /N028/ that renders as &#x1320d;.
pattern N028 :: Char
pattern N028 = '\x1320d'

-- | The Egyptian hieroglyph /N029/ that renders as &#x1320e;.
pattern N029 :: Char
pattern N029 = '\x1320e'

-- | The Egyptian hieroglyph /N030/ that renders as &#x1320f;.
pattern N030 :: Char
pattern N030 = '\x1320f'

-- | The Egyptian hieroglyph /N031/ that renders as &#x13210;.
pattern N031 :: Char
pattern N031 = '\x13210'

-- | The Egyptian hieroglyph /N032/ that renders as &#x13211;.
pattern N032 :: Char
pattern N032 = '\x13211'

-- | The Egyptian hieroglyph /N033/ that renders as &#x13212;.
pattern N033 :: Char
pattern N033 = '\x13212'

-- | The Egyptian hieroglyph /N033A/ that renders as &#x13213;.
pattern N033A :: Char
pattern N033A = '\x13213'

-- | The Egyptian hieroglyph /N034/ that renders as &#x13214;.
pattern N034 :: Char
pattern N034 = '\x13214'

-- | The Egyptian hieroglyph /N034A/ that renders as &#x13215;.
pattern N034A :: Char
pattern N034A = '\x13215'

-- | The Egyptian hieroglyph /N035/ that renders as &#x13216;.
pattern N035 :: Char
pattern N035 = '\x13216'

-- | The Egyptian hieroglyph /N035A/ that renders as &#x13217;.
pattern N035A :: Char
pattern N035A = '\x13217'

-- | The Egyptian hieroglyph /N036/ that renders as &#x13218;.
pattern N036 :: Char
pattern N036 = '\x13218'

-- | The Egyptian hieroglyph /N037/ that renders as &#x13219;.
pattern N037 :: Char
pattern N037 = '\x13219'

-- | The Egyptian hieroglyph /N037A/ that renders as &#x1321a;.
pattern N037A :: Char
pattern N037A = '\x1321a'

-- | The Egyptian hieroglyph /N038/ that renders as &#x1321b;.
pattern N038 :: Char
pattern N038 = '\x1321b'

-- | The Egyptian hieroglyph /N039/ that renders as &#x1321c;.
pattern N039 :: Char
pattern N039 = '\x1321c'

-- | The Egyptian hieroglyph /N040/ that renders as &#x1321d;.
pattern N040 :: Char
pattern N040 = '\x1321d'

-- | The Egyptian hieroglyph /N041/ that renders as &#x1321e;.
pattern N041 :: Char
pattern N041 = '\x1321e'

-- | The Egyptian hieroglyph /N042/ that renders as &#x1321f;.
pattern N042 :: Char
pattern N042 = '\x1321f'

-- | The Egyptian hieroglyph /NL001/ that renders as &#x13220;.
pattern NL001 :: Char
pattern NL001 = '\x13220'

-- | The Egyptian hieroglyph /NL002/ that renders as &#x13221;.
pattern NL002 :: Char
pattern NL002 = '\x13221'

-- | The Egyptian hieroglyph /NL003/ that renders as &#x13222;.
pattern NL003 :: Char
pattern NL003 = '\x13222'

-- | The Egyptian hieroglyph /NL004/ that renders as &#x13223;.
pattern NL004 :: Char
pattern NL004 = '\x13223'

-- | The Egyptian hieroglyph /NL005/ that renders as &#x13224;.
pattern NL005 :: Char
pattern NL005 = '\x13224'

-- | The Egyptian hieroglyph /NL005A/ that renders as &#x13225;.
pattern NL005A :: Char
pattern NL005A = '\x13225'

-- | The Egyptian hieroglyph /NL006/ that renders as &#x13226;.
pattern NL006 :: Char
pattern NL006 = '\x13226'

-- | The Egyptian hieroglyph /NL007/ that renders as &#x13227;.
pattern NL007 :: Char
pattern NL007 = '\x13227'

-- | The Egyptian hieroglyph /NL008/ that renders as &#x13228;.
pattern NL008 :: Char
pattern NL008 = '\x13228'

-- | The Egyptian hieroglyph /NL009/ that renders as &#x13229;.
pattern NL009 :: Char
pattern NL009 = '\x13229'

-- | The Egyptian hieroglyph /NL010/ that renders as &#x1322a;.
pattern NL010 :: Char
pattern NL010 = '\x1322a'

-- | The Egyptian hieroglyph /NL011/ that renders as &#x1322b;.
pattern NL011 :: Char
pattern NL011 = '\x1322b'

-- | The Egyptian hieroglyph /NL012/ that renders as &#x1322c;.
pattern NL012 :: Char
pattern NL012 = '\x1322c'

-- | The Egyptian hieroglyph /NL013/ that renders as &#x1322d;.
pattern NL013 :: Char
pattern NL013 = '\x1322d'

-- | The Egyptian hieroglyph /NL014/ that renders as &#x1322e;.
pattern NL014 :: Char
pattern NL014 = '\x1322e'

-- | The Egyptian hieroglyph /NL015/ that renders as &#x1322f;.
pattern NL015 :: Char
pattern NL015 = '\x1322f'

-- | The Egyptian hieroglyph /NL016/ that renders as &#x13230;.
pattern NL016 :: Char
pattern NL016 = '\x13230'

-- | The Egyptian hieroglyph /NL017/ that renders as &#x13231;.
pattern NL017 :: Char
pattern NL017 = '\x13231'

-- | The Egyptian hieroglyph /NL017A/ that renders as &#x13232;.
pattern NL017A :: Char
pattern NL017A = '\x13232'

-- | The Egyptian hieroglyph /NL018/ that renders as &#x13233;.
pattern NL018 :: Char
pattern NL018 = '\x13233'

-- | The Egyptian hieroglyph /NL019/ that renders as &#x13234;.
pattern NL019 :: Char
pattern NL019 = '\x13234'

-- | The Egyptian hieroglyph /NL020/ that renders as &#x13235;.
pattern NL020 :: Char
pattern NL020 = '\x13235'

-- | The Egyptian hieroglyph /NU001/ that renders as &#x13236;.
pattern NU001 :: Char
pattern NU001 = '\x13236'

-- | The Egyptian hieroglyph /NU002/ that renders as &#x13237;.
pattern NU002 :: Char
pattern NU002 = '\x13237'

-- | The Egyptian hieroglyph /NU003/ that renders as &#x13238;.
pattern NU003 :: Char
pattern NU003 = '\x13238'

-- | The Egyptian hieroglyph /NU004/ that renders as &#x13239;.
pattern NU004 :: Char
pattern NU004 = '\x13239'

-- | The Egyptian hieroglyph /NU005/ that renders as &#x1323a;.
pattern NU005 :: Char
pattern NU005 = '\x1323a'

-- | The Egyptian hieroglyph /NU006/ that renders as &#x1323b;.
pattern NU006 :: Char
pattern NU006 = '\x1323b'

-- | The Egyptian hieroglyph /NU007/ that renders as &#x1323c;.
pattern NU007 :: Char
pattern NU007 = '\x1323c'

-- | The Egyptian hieroglyph /NU008/ that renders as &#x1323d;.
pattern NU008 :: Char
pattern NU008 = '\x1323d'

-- | The Egyptian hieroglyph /NU009/ that renders as &#x1323e;.
pattern NU009 :: Char
pattern NU009 = '\x1323e'

-- | The Egyptian hieroglyph /NU010/ that renders as &#x1323f;.
pattern NU010 :: Char
pattern NU010 = '\x1323f'

-- | The Egyptian hieroglyph /NU010A/ that renders as &#x13240;.
pattern NU010A :: Char
pattern NU010A = '\x13240'

-- | The Egyptian hieroglyph /NU011/ that renders as &#x13241;.
pattern NU011 :: Char
pattern NU011 = '\x13241'

-- | The Egyptian hieroglyph /NU011A/ that renders as &#x13242;.
pattern NU011A :: Char
pattern NU011A = '\x13242'

-- | The Egyptian hieroglyph /NU012/ that renders as &#x13243;.
pattern NU012 :: Char
pattern NU012 = '\x13243'

-- | The Egyptian hieroglyph /NU013/ that renders as &#x13244;.
pattern NU013 :: Char
pattern NU013 = '\x13244'

-- | The Egyptian hieroglyph /NU014/ that renders as &#x13245;.
pattern NU014 :: Char
pattern NU014 = '\x13245'

-- | The Egyptian hieroglyph /NU015/ that renders as &#x13246;.
pattern NU015 :: Char
pattern NU015 = '\x13246'

-- | The Egyptian hieroglyph /NU016/ that renders as &#x13247;.
pattern NU016 :: Char
pattern NU016 = '\x13247'

-- | The Egyptian hieroglyph /NU017/ that renders as &#x13248;.
pattern NU017 :: Char
pattern NU017 = '\x13248'

-- | The Egyptian hieroglyph /NU018/ that renders as &#x13249;.
pattern NU018 :: Char
pattern NU018 = '\x13249'

-- | The Egyptian hieroglyph /NU018A/ that renders as &#x1324a;.
pattern NU018A :: Char
pattern NU018A = '\x1324a'

-- | The Egyptian hieroglyph /NU019/ that renders as &#x1324b;.
pattern NU019 :: Char
pattern NU019 = '\x1324b'

-- | The Egyptian hieroglyph /NU020/ that renders as &#x1324c;.
pattern NU020 :: Char
pattern NU020 = '\x1324c'

-- | The Egyptian hieroglyph /NU021/ that renders as &#x1324d;.
pattern NU021 :: Char
pattern NU021 = '\x1324d'

-- | The Egyptian hieroglyph /NU022/ that renders as &#x1324e;.
pattern NU022 :: Char
pattern NU022 = '\x1324e'

-- | The Egyptian hieroglyph /NU022A/ that renders as &#x1324f;.
pattern NU022A :: Char
pattern NU022A = '\x1324f'

-- | The Egyptian hieroglyph /O001/ that renders as &#x13250;.
pattern O001 :: Char
pattern O001 = '\x13250'

-- | The Egyptian hieroglyph /O001A/ that renders as &#x13251;.
pattern O001A :: Char
pattern O001A = '\x13251'

-- | The Egyptian hieroglyph /O002/ that renders as &#x13252;.
pattern O002 :: Char
pattern O002 = '\x13252'

-- | The Egyptian hieroglyph /O003/ that renders as &#x13253;.
pattern O003 :: Char
pattern O003 = '\x13253'

-- | The Egyptian hieroglyph /O004/ that renders as &#x13254;.
pattern O004 :: Char
pattern O004 = '\x13254'

-- | The Egyptian hieroglyph /O005/ that renders as &#x13255;.
pattern O005 :: Char
pattern O005 = '\x13255'

-- | The Egyptian hieroglyph /O005A/ that renders as &#x13256;.
pattern O005A :: Char
pattern O005A = '\x13256'

-- | The Egyptian hieroglyph /O006/ that renders as &#x13257;.
pattern O006 :: Char
pattern O006 = '\x13257'

-- | The Egyptian hieroglyph /O006A/ that renders as &#x13258;.
pattern O006A :: Char
pattern O006A = '\x13258'

-- | The Egyptian hieroglyph /O006B/ that renders as &#x13259;.
pattern O006B :: Char
pattern O006B = '\x13259'

-- | The Egyptian hieroglyph /O006C/ that renders as &#x1325a;.
pattern O006C :: Char
pattern O006C = '\x1325a'

-- | The Egyptian hieroglyph /O006D/ that renders as &#x1325b;.
pattern O006D :: Char
pattern O006D = '\x1325b'

-- | The Egyptian hieroglyph /O006E/ that renders as &#x1325c;.
pattern O006E :: Char
pattern O006E = '\x1325c'

-- | The Egyptian hieroglyph /O006F/ that renders as &#x1325d;.
pattern O006F :: Char
pattern O006F = '\x1325d'

-- | The Egyptian hieroglyph /O007/ that renders as &#x1325e;.
pattern O007 :: Char
pattern O007 = '\x1325e'

-- | The Egyptian hieroglyph /O008/ that renders as &#x1325f;.
pattern O008 :: Char
pattern O008 = '\x1325f'

-- | The Egyptian hieroglyph /O009/ that renders as &#x13260;.
pattern O009 :: Char
pattern O009 = '\x13260'

-- | The Egyptian hieroglyph /O010/ that renders as &#x13261;.
pattern O010 :: Char
pattern O010 = '\x13261'

-- | The Egyptian hieroglyph /O010A/ that renders as &#x13262;.
pattern O010A :: Char
pattern O010A = '\x13262'

-- | The Egyptian hieroglyph /O010B/ that renders as &#x13263;.
pattern O010B :: Char
pattern O010B = '\x13263'

-- | The Egyptian hieroglyph /O010C/ that renders as &#x13264;.
pattern O010C :: Char
pattern O010C = '\x13264'

-- | The Egyptian hieroglyph /O011/ that renders as &#x13265;.
pattern O011 :: Char
pattern O011 = '\x13265'

-- | The Egyptian hieroglyph /O012/ that renders as &#x13266;.
pattern O012 :: Char
pattern O012 = '\x13266'

-- | The Egyptian hieroglyph /O013/ that renders as &#x13267;.
pattern O013 :: Char
pattern O013 = '\x13267'

-- | The Egyptian hieroglyph /O014/ that renders as &#x13268;.
pattern O014 :: Char
pattern O014 = '\x13268'

-- | The Egyptian hieroglyph /O015/ that renders as &#x13269;.
pattern O015 :: Char
pattern O015 = '\x13269'

-- | The Egyptian hieroglyph /O016/ that renders as &#x1326a;.
pattern O016 :: Char
pattern O016 = '\x1326a'

-- | The Egyptian hieroglyph /O017/ that renders as &#x1326b;.
pattern O017 :: Char
pattern O017 = '\x1326b'

-- | The Egyptian hieroglyph /O018/ that renders as &#x1326c;.
pattern O018 :: Char
pattern O018 = '\x1326c'

-- | The Egyptian hieroglyph /O019/ that renders as &#x1326d;.
pattern O019 :: Char
pattern O019 = '\x1326d'

-- | The Egyptian hieroglyph /O019A/ that renders as &#x1326e;.
pattern O019A :: Char
pattern O019A = '\x1326e'

-- | The Egyptian hieroglyph /O020/ that renders as &#x1326f;.
pattern O020 :: Char
pattern O020 = '\x1326f'

-- | The Egyptian hieroglyph /O020A/ that renders as &#x13270;.
pattern O020A :: Char
pattern O020A = '\x13270'

-- | The Egyptian hieroglyph /O021/ that renders as &#x13271;.
pattern O021 :: Char
pattern O021 = '\x13271'

-- | The Egyptian hieroglyph /O022/ that renders as &#x13272;.
pattern O022 :: Char
pattern O022 = '\x13272'

-- | The Egyptian hieroglyph /O023/ that renders as &#x13273;.
pattern O023 :: Char
pattern O023 = '\x13273'

-- | The Egyptian hieroglyph /O024/ that renders as &#x13274;.
pattern O024 :: Char
pattern O024 = '\x13274'

-- | The Egyptian hieroglyph /O024A/ that renders as &#x13275;.
pattern O024A :: Char
pattern O024A = '\x13275'

-- | The Egyptian hieroglyph /O025/ that renders as &#x13276;.
pattern O025 :: Char
pattern O025 = '\x13276'

-- | The Egyptian hieroglyph /O025A/ that renders as &#x13277;.
pattern O025A :: Char
pattern O025A = '\x13277'

-- | The Egyptian hieroglyph /O026/ that renders as &#x13278;.
pattern O026 :: Char
pattern O026 = '\x13278'

-- | The Egyptian hieroglyph /O027/ that renders as &#x13279;.
pattern O027 :: Char
pattern O027 = '\x13279'

-- | The Egyptian hieroglyph /O028/ that renders as &#x1327a;.
pattern O028 :: Char
pattern O028 = '\x1327a'

-- | The Egyptian hieroglyph /O029/ that renders as &#x1327b;.
pattern O029 :: Char
pattern O029 = '\x1327b'

-- | The Egyptian hieroglyph /O029A/ that renders as &#x1327c;.
pattern O029A :: Char
pattern O029A = '\x1327c'

-- | The Egyptian hieroglyph /O030/ that renders as &#x1327d;.
pattern O030 :: Char
pattern O030 = '\x1327d'

-- | The Egyptian hieroglyph /O030A/ that renders as &#x1327e;.
pattern O030A :: Char
pattern O030A = '\x1327e'

-- | The Egyptian hieroglyph /O031/ that renders as &#x1327f;.
pattern O031 :: Char
pattern O031 = '\x1327f'

-- | The Egyptian hieroglyph /O032/ that renders as &#x13280;.
pattern O032 :: Char
pattern O032 = '\x13280'

-- | The Egyptian hieroglyph /O033/ that renders as &#x13281;.
pattern O033 :: Char
pattern O033 = '\x13281'

-- | The Egyptian hieroglyph /O033A/ that renders as &#x13282;.
pattern O033A :: Char
pattern O033A = '\x13282'

-- | The Egyptian hieroglyph /O034/ that renders as &#x13283;.
pattern O034 :: Char
pattern O034 = '\x13283'

-- | The Egyptian hieroglyph /O035/ that renders as &#x13284;.
pattern O035 :: Char
pattern O035 = '\x13284'

-- | The Egyptian hieroglyph /O036/ that renders as &#x13285;.
pattern O036 :: Char
pattern O036 = '\x13285'

-- | The Egyptian hieroglyph /O036A/ that renders as &#x13286;.
pattern O036A :: Char
pattern O036A = '\x13286'

-- | The Egyptian hieroglyph /O036B/ that renders as &#x13287;.
pattern O036B :: Char
pattern O036B = '\x13287'

-- | The Egyptian hieroglyph /O036C/ that renders as &#x13288;.
pattern O036C :: Char
pattern O036C = '\x13288'

-- | The Egyptian hieroglyph /O036D/ that renders as &#x13289;.
pattern O036D :: Char
pattern O036D = '\x13289'

-- | The Egyptian hieroglyph /O037/ that renders as &#x1328a;.
pattern O037 :: Char
pattern O037 = '\x1328a'

-- | The Egyptian hieroglyph /O038/ that renders as &#x1328b;.
pattern O038 :: Char
pattern O038 = '\x1328b'

-- | The Egyptian hieroglyph /O039/ that renders as &#x1328c;.
pattern O039 :: Char
pattern O039 = '\x1328c'

-- | The Egyptian hieroglyph /O040/ that renders as &#x1328d;.
pattern O040 :: Char
pattern O040 = '\x1328d'

-- | The Egyptian hieroglyph /O041/ that renders as &#x1328e;.
pattern O041 :: Char
pattern O041 = '\x1328e'

-- | The Egyptian hieroglyph /O042/ that renders as &#x1328f;.
pattern O042 :: Char
pattern O042 = '\x1328f'

-- | The Egyptian hieroglyph /O043/ that renders as &#x13290;.
pattern O043 :: Char
pattern O043 = '\x13290'

-- | The Egyptian hieroglyph /O044/ that renders as &#x13291;.
pattern O044 :: Char
pattern O044 = '\x13291'

-- | The Egyptian hieroglyph /O045/ that renders as &#x13292;.
pattern O045 :: Char
pattern O045 = '\x13292'

-- | The Egyptian hieroglyph /O046/ that renders as &#x13293;.
pattern O046 :: Char
pattern O046 = '\x13293'

-- | The Egyptian hieroglyph /O047/ that renders as &#x13294;.
pattern O047 :: Char
pattern O047 = '\x13294'

-- | The Egyptian hieroglyph /O048/ that renders as &#x13295;.
pattern O048 :: Char
pattern O048 = '\x13295'

-- | The Egyptian hieroglyph /O049/ that renders as &#x13296;.
pattern O049 :: Char
pattern O049 = '\x13296'

-- | The Egyptian hieroglyph /O050/ that renders as &#x13297;.
pattern O050 :: Char
pattern O050 = '\x13297'

-- | The Egyptian hieroglyph /O050A/ that renders as &#x13298;.
pattern O050A :: Char
pattern O050A = '\x13298'

-- | The Egyptian hieroglyph /O050B/ that renders as &#x13299;.
pattern O050B :: Char
pattern O050B = '\x13299'

-- | The Egyptian hieroglyph /O051/ that renders as &#x1329a;.
pattern O051 :: Char
pattern O051 = '\x1329a'

-- | The Egyptian hieroglyph /P001/ that renders as &#x1329b;.
pattern P001 :: Char
pattern P001 = '\x1329b'

-- | The Egyptian hieroglyph /P001A/ that renders as &#x1329c;.
pattern P001A :: Char
pattern P001A = '\x1329c'

-- | The Egyptian hieroglyph /P002/ that renders as &#x1329d;.
pattern P002 :: Char
pattern P002 = '\x1329d'

-- | The Egyptian hieroglyph /P003/ that renders as &#x1329e;.
pattern P003 :: Char
pattern P003 = '\x1329e'

-- | The Egyptian hieroglyph /P003A/ that renders as &#x1329f;.
pattern P003A :: Char
pattern P003A = '\x1329f'

-- | The Egyptian hieroglyph /P004/ that renders as &#x132a0;.
pattern P004 :: Char
pattern P004 = '\x132a0'

-- | The Egyptian hieroglyph /P005/ that renders as &#x132a1;.
pattern P005 :: Char
pattern P005 = '\x132a1'

-- | The Egyptian hieroglyph /P006/ that renders as &#x132a2;.
pattern P006 :: Char
pattern P006 = '\x132a2'

-- | The Egyptian hieroglyph /P007/ that renders as &#x132a3;.
pattern P007 :: Char
pattern P007 = '\x132a3'

-- | The Egyptian hieroglyph /P008/ that renders as &#x132a4;.
pattern P008 :: Char
pattern P008 = '\x132a4'

-- | The Egyptian hieroglyph /P009/ that renders as &#x132a5;.
pattern P009 :: Char
pattern P009 = '\x132a5'

-- | The Egyptian hieroglyph /P010/ that renders as &#x132a6;.
pattern P010 :: Char
pattern P010 = '\x132a6'

-- | The Egyptian hieroglyph /P011/ that renders as &#x132a7;.
pattern P011 :: Char
pattern P011 = '\x132a7'

-- | The Egyptian hieroglyph /Q001/ that renders as &#x132a8;.
pattern Q001 :: Char
pattern Q001 = '\x132a8'

-- | The Egyptian hieroglyph /Q002/ that renders as &#x132a9;.
pattern Q002 :: Char
pattern Q002 = '\x132a9'

-- | The Egyptian hieroglyph /Q003/ that renders as &#x132aa;.
pattern Q003 :: Char
pattern Q003 = '\x132aa'

-- | The Egyptian hieroglyph /Q004/ that renders as &#x132ab;.
pattern Q004 :: Char
pattern Q004 = '\x132ab'

-- | The Egyptian hieroglyph /Q005/ that renders as &#x132ac;.
pattern Q005 :: Char
pattern Q005 = '\x132ac'

-- | The Egyptian hieroglyph /Q006/ that renders as &#x132ad;.
pattern Q006 :: Char
pattern Q006 = '\x132ad'

-- | The Egyptian hieroglyph /Q007/ that renders as &#x132ae;.
pattern Q007 :: Char
pattern Q007 = '\x132ae'

-- | The Egyptian hieroglyph /R001/ that renders as &#x132af;.
pattern R001 :: Char
pattern R001 = '\x132af'

-- | The Egyptian hieroglyph /R002/ that renders as &#x132b0;.
pattern R002 :: Char
pattern R002 = '\x132b0'

-- | The Egyptian hieroglyph /R002A/ that renders as &#x132b1;.
pattern R002A :: Char
pattern R002A = '\x132b1'

-- | The Egyptian hieroglyph /R003/ that renders as &#x132b2;.
pattern R003 :: Char
pattern R003 = '\x132b2'

-- | The Egyptian hieroglyph /R003A/ that renders as &#x132b3;.
pattern R003A :: Char
pattern R003A = '\x132b3'

-- | The Egyptian hieroglyph /R003B/ that renders as &#x132b4;.
pattern R003B :: Char
pattern R003B = '\x132b4'

-- | The Egyptian hieroglyph /R004/ that renders as &#x132b5;.
pattern R004 :: Char
pattern R004 = '\x132b5'

-- | The Egyptian hieroglyph /R005/ that renders as &#x132b6;.
pattern R005 :: Char
pattern R005 = '\x132b6'

-- | The Egyptian hieroglyph /R006/ that renders as &#x132b7;.
pattern R006 :: Char
pattern R006 = '\x132b7'

-- | The Egyptian hieroglyph /R007/ that renders as &#x132b8;.
pattern R007 :: Char
pattern R007 = '\x132b8'

-- | The Egyptian hieroglyph /R008/ that renders as &#x132b9;.
pattern R008 :: Char
pattern R008 = '\x132b9'

-- | The Egyptian hieroglyph /R009/ that renders as &#x132ba;.
pattern R009 :: Char
pattern R009 = '\x132ba'

-- | The Egyptian hieroglyph /R010/ that renders as &#x132bb;.
pattern R010 :: Char
pattern R010 = '\x132bb'

-- | The Egyptian hieroglyph /R010A/ that renders as &#x132bc;.
pattern R010A :: Char
pattern R010A = '\x132bc'

-- | The Egyptian hieroglyph /R011/ that renders as &#x132bd;.
pattern R011 :: Char
pattern R011 = '\x132bd'

-- | The Egyptian hieroglyph /R012/ that renders as &#x132be;.
pattern R012 :: Char
pattern R012 = '\x132be'

-- | The Egyptian hieroglyph /R013/ that renders as &#x132bf;.
pattern R013 :: Char
pattern R013 = '\x132bf'

-- | The Egyptian hieroglyph /R014/ that renders as &#x132c0;.
pattern R014 :: Char
pattern R014 = '\x132c0'

-- | The Egyptian hieroglyph /R015/ that renders as &#x132c1;.
pattern R015 :: Char
pattern R015 = '\x132c1'

-- | The Egyptian hieroglyph /R016/ that renders as &#x132c2;.
pattern R016 :: Char
pattern R016 = '\x132c2'

-- | The Egyptian hieroglyph /R016A/ that renders as &#x132c3;.
pattern R016A :: Char
pattern R016A = '\x132c3'

-- | The Egyptian hieroglyph /R017/ that renders as &#x132c4;.
pattern R017 :: Char
pattern R017 = '\x132c4'

-- | The Egyptian hieroglyph /R018/ that renders as &#x132c5;.
pattern R018 :: Char
pattern R018 = '\x132c5'

-- | The Egyptian hieroglyph /R019/ that renders as &#x132c6;.
pattern R019 :: Char
pattern R019 = '\x132c6'

-- | The Egyptian hieroglyph /R020/ that renders as &#x132c7;.
pattern R020 :: Char
pattern R020 = '\x132c7'

-- | The Egyptian hieroglyph /R021/ that renders as &#x132c8;.
pattern R021 :: Char
pattern R021 = '\x132c8'

-- | The Egyptian hieroglyph /R022/ that renders as &#x132c9;.
pattern R022 :: Char
pattern R022 = '\x132c9'

-- | The Egyptian hieroglyph /R023/ that renders as &#x132ca;.
pattern R023 :: Char
pattern R023 = '\x132ca'

-- | The Egyptian hieroglyph /R024/ that renders as &#x132cb;.
pattern R024 :: Char
pattern R024 = '\x132cb'

-- | The Egyptian hieroglyph /R025/ that renders as &#x132cc;.
pattern R025 :: Char
pattern R025 = '\x132cc'

-- | The Egyptian hieroglyph /R026/ that renders as &#x132cd;.
pattern R026 :: Char
pattern R026 = '\x132cd'

-- | The Egyptian hieroglyph /R027/ that renders as &#x132ce;.
pattern R027 :: Char
pattern R027 = '\x132ce'

-- | The Egyptian hieroglyph /R028/ that renders as &#x132cf;.
pattern R028 :: Char
pattern R028 = '\x132cf'

-- | The Egyptian hieroglyph /R029/ that renders as &#x132d0;.
pattern R029 :: Char
pattern R029 = '\x132d0'

-- | The Egyptian hieroglyph /S001/ that renders as &#x132d1;.
pattern S001 :: Char
pattern S001 = '\x132d1'

-- | The Egyptian hieroglyph /S002/ that renders as &#x132d2;.
pattern S002 :: Char
pattern S002 = '\x132d2'

-- | The Egyptian hieroglyph /S002A/ that renders as &#x132d3;.
pattern S002A :: Char
pattern S002A = '\x132d3'

-- | The Egyptian hieroglyph /S003/ that renders as &#x132d4;.
pattern S003 :: Char
pattern S003 = '\x132d4'

-- | The Egyptian hieroglyph /S004/ that renders as &#x132d5;.
pattern S004 :: Char
pattern S004 = '\x132d5'

-- | The Egyptian hieroglyph /S005/ that renders as &#x132d6;.
pattern S005 :: Char
pattern S005 = '\x132d6'

-- | The Egyptian hieroglyph /S006/ that renders as &#x132d7;.
pattern S006 :: Char
pattern S006 = '\x132d7'

-- | The Egyptian hieroglyph /S006A/ that renders as &#x132d8;.
pattern S006A :: Char
pattern S006A = '\x132d8'

-- | The Egyptian hieroglyph /S007/ that renders as &#x132d9;.
pattern S007 :: Char
pattern S007 = '\x132d9'

-- | The Egyptian hieroglyph /S008/ that renders as &#x132da;.
pattern S008 :: Char
pattern S008 = '\x132da'

-- | The Egyptian hieroglyph /S009/ that renders as &#x132db;.
pattern S009 :: Char
pattern S009 = '\x132db'

-- | The Egyptian hieroglyph /S010/ that renders as &#x132dc;.
pattern S010 :: Char
pattern S010 = '\x132dc'

-- | The Egyptian hieroglyph /S011/ that renders as &#x132dd;.
pattern S011 :: Char
pattern S011 = '\x132dd'

-- | The Egyptian hieroglyph /S012/ that renders as &#x132de;.
pattern S012 :: Char
pattern S012 = '\x132de'

-- | The Egyptian hieroglyph /S013/ that renders as &#x132df;.
pattern S013 :: Char
pattern S013 = '\x132df'

-- | The Egyptian hieroglyph /S014/ that renders as &#x132e0;.
pattern S014 :: Char
pattern S014 = '\x132e0'

-- | The Egyptian hieroglyph /S014A/ that renders as &#x132e1;.
pattern S014A :: Char
pattern S014A = '\x132e1'

-- | The Egyptian hieroglyph /S014B/ that renders as &#x132e2;.
pattern S014B :: Char
pattern S014B = '\x132e2'

-- | The Egyptian hieroglyph /S015/ that renders as &#x132e3;.
pattern S015 :: Char
pattern S015 = '\x132e3'

-- | The Egyptian hieroglyph /S016/ that renders as &#x132e4;.
pattern S016 :: Char
pattern S016 = '\x132e4'

-- | The Egyptian hieroglyph /S017/ that renders as &#x132e5;.
pattern S017 :: Char
pattern S017 = '\x132e5'

-- | The Egyptian hieroglyph /S017A/ that renders as &#x132e6;.
pattern S017A :: Char
pattern S017A = '\x132e6'

-- | The Egyptian hieroglyph /S018/ that renders as &#x132e7;.
pattern S018 :: Char
pattern S018 = '\x132e7'

-- | The Egyptian hieroglyph /S019/ that renders as &#x132e8;.
pattern S019 :: Char
pattern S019 = '\x132e8'

-- | The Egyptian hieroglyph /S020/ that renders as &#x132e9;.
pattern S020 :: Char
pattern S020 = '\x132e9'

-- | The Egyptian hieroglyph /S021/ that renders as &#x132ea;.
pattern S021 :: Char
pattern S021 = '\x132ea'

-- | The Egyptian hieroglyph /S022/ that renders as &#x132eb;.
pattern S022 :: Char
pattern S022 = '\x132eb'

-- | The Egyptian hieroglyph /S023/ that renders as &#x132ec;.
pattern S023 :: Char
pattern S023 = '\x132ec'

-- | The Egyptian hieroglyph /S024/ that renders as &#x132ed;.
pattern S024 :: Char
pattern S024 = '\x132ed'

-- | The Egyptian hieroglyph /S025/ that renders as &#x132ee;.
pattern S025 :: Char
pattern S025 = '\x132ee'

-- | The Egyptian hieroglyph /S026/ that renders as &#x132ef;.
pattern S026 :: Char
pattern S026 = '\x132ef'

-- | The Egyptian hieroglyph /S026A/ that renders as &#x132f0;.
pattern S026A :: Char
pattern S026A = '\x132f0'

-- | The Egyptian hieroglyph /S026B/ that renders as &#x132f1;.
pattern S026B :: Char
pattern S026B = '\x132f1'

-- | The Egyptian hieroglyph /S027/ that renders as &#x132f2;.
pattern S027 :: Char
pattern S027 = '\x132f2'

-- | The Egyptian hieroglyph /S028/ that renders as &#x132f3;.
pattern S028 :: Char
pattern S028 = '\x132f3'

-- | The Egyptian hieroglyph /S029/ that renders as &#x132f4;.
pattern S029 :: Char
pattern S029 = '\x132f4'

-- | The Egyptian hieroglyph /S030/ that renders as &#x132f5;.
pattern S030 :: Char
pattern S030 = '\x132f5'

-- | The Egyptian hieroglyph /S031/ that renders as &#x132f6;.
pattern S031 :: Char
pattern S031 = '\x132f6'

-- | The Egyptian hieroglyph /S032/ that renders as &#x132f7;.
pattern S032 :: Char
pattern S032 = '\x132f7'

-- | The Egyptian hieroglyph /S033/ that renders as &#x132f8;.
pattern S033 :: Char
pattern S033 = '\x132f8'

-- | The Egyptian hieroglyph /S034/ that renders as &#x132f9;.
pattern S034 :: Char
pattern S034 = '\x132f9'

-- | The Egyptian hieroglyph /S035/ that renders as &#x132fa;.
pattern S035 :: Char
pattern S035 = '\x132fa'

-- | The Egyptian hieroglyph /S035A/ that renders as &#x132fb;.
pattern S035A :: Char
pattern S035A = '\x132fb'

-- | The Egyptian hieroglyph /S036/ that renders as &#x132fc;.
pattern S036 :: Char
pattern S036 = '\x132fc'

-- | The Egyptian hieroglyph /S037/ that renders as &#x132fd;.
pattern S037 :: Char
pattern S037 = '\x132fd'

-- | The Egyptian hieroglyph /S038/ that renders as &#x132fe;.
pattern S038 :: Char
pattern S038 = '\x132fe'

-- | The Egyptian hieroglyph /S039/ that renders as &#x132ff;.
pattern S039 :: Char
pattern S039 = '\x132ff'

-- | The Egyptian hieroglyph /S040/ that renders as &#x13300;.
pattern S040 :: Char
pattern S040 = '\x13300'

-- | The Egyptian hieroglyph /S041/ that renders as &#x13301;.
pattern S041 :: Char
pattern S041 = '\x13301'

-- | The Egyptian hieroglyph /S042/ that renders as &#x13302;.
pattern S042 :: Char
pattern S042 = '\x13302'

-- | The Egyptian hieroglyph /S043/ that renders as &#x13303;.
pattern S043 :: Char
pattern S043 = '\x13303'

-- | The Egyptian hieroglyph /S044/ that renders as &#x13304;.
pattern S044 :: Char
pattern S044 = '\x13304'

-- | The Egyptian hieroglyph /S045/ that renders as &#x13305;.
pattern S045 :: Char
pattern S045 = '\x13305'

-- | The Egyptian hieroglyph /S046/ that renders as &#x13306;.
pattern S046 :: Char
pattern S046 = '\x13306'

-- | The Egyptian hieroglyph /T001/ that renders as &#x13307;.
pattern T001 :: Char
pattern T001 = '\x13307'

-- | The Egyptian hieroglyph /T002/ that renders as &#x13308;.
pattern T002 :: Char
pattern T002 = '\x13308'

-- | The Egyptian hieroglyph /T003/ that renders as &#x13309;.
pattern T003 :: Char
pattern T003 = '\x13309'

-- | The Egyptian hieroglyph /T003A/ that renders as &#x1330a;.
pattern T003A :: Char
pattern T003A = '\x1330a'

-- | The Egyptian hieroglyph /T004/ that renders as &#x1330b;.
pattern T004 :: Char
pattern T004 = '\x1330b'

-- | The Egyptian hieroglyph /T005/ that renders as &#x1330c;.
pattern T005 :: Char
pattern T005 = '\x1330c'

-- | The Egyptian hieroglyph /T006/ that renders as &#x1330d;.
pattern T006 :: Char
pattern T006 = '\x1330d'

-- | The Egyptian hieroglyph /T007/ that renders as &#x1330e;.
pattern T007 :: Char
pattern T007 = '\x1330e'

-- | The Egyptian hieroglyph /T007A/ that renders as &#x1330f;.
pattern T007A :: Char
pattern T007A = '\x1330f'

-- | The Egyptian hieroglyph /T008/ that renders as &#x13310;.
pattern T008 :: Char
pattern T008 = '\x13310'

-- | The Egyptian hieroglyph /T008A/ that renders as &#x13311;.
pattern T008A :: Char
pattern T008A = '\x13311'

-- | The Egyptian hieroglyph /T009/ that renders as &#x13312;.
pattern T009 :: Char
pattern T009 = '\x13312'

-- | The Egyptian hieroglyph /T009A/ that renders as &#x13313;.
pattern T009A :: Char
pattern T009A = '\x13313'

-- | The Egyptian hieroglyph /T010/ that renders as &#x13314;.
pattern T010 :: Char
pattern T010 = '\x13314'

-- | The Egyptian hieroglyph /T011/ that renders as &#x13315;.
pattern T011 :: Char
pattern T011 = '\x13315'

-- | The Egyptian hieroglyph /T011A/ that renders as &#x13316;.
pattern T011A :: Char
pattern T011A = '\x13316'

-- | The Egyptian hieroglyph /T012/ that renders as &#x13317;.
pattern T012 :: Char
pattern T012 = '\x13317'

-- | The Egyptian hieroglyph /T013/ that renders as &#x13318;.
pattern T013 :: Char
pattern T013 = '\x13318'

-- | The Egyptian hieroglyph /T014/ that renders as &#x13319;.
pattern T014 :: Char
pattern T014 = '\x13319'

-- | The Egyptian hieroglyph /T015/ that renders as &#x1331a;.
pattern T015 :: Char
pattern T015 = '\x1331a'

-- | The Egyptian hieroglyph /T016/ that renders as &#x1331b;.
pattern T016 :: Char
pattern T016 = '\x1331b'

-- | The Egyptian hieroglyph /T016A/ that renders as &#x1331c;.
pattern T016A :: Char
pattern T016A = '\x1331c'

-- | The Egyptian hieroglyph /T017/ that renders as &#x1331d;.
pattern T017 :: Char
pattern T017 = '\x1331d'

-- | The Egyptian hieroglyph /T018/ that renders as &#x1331e;.
pattern T018 :: Char
pattern T018 = '\x1331e'

-- | The Egyptian hieroglyph /T019/ that renders as &#x1331f;.
pattern T019 :: Char
pattern T019 = '\x1331f'

-- | The Egyptian hieroglyph /T020/ that renders as &#x13320;.
pattern T020 :: Char
pattern T020 = '\x13320'

-- | The Egyptian hieroglyph /T021/ that renders as &#x13321;.
pattern T021 :: Char
pattern T021 = '\x13321'

-- | The Egyptian hieroglyph /T022/ that renders as &#x13322;.
pattern T022 :: Char
pattern T022 = '\x13322'

-- | The Egyptian hieroglyph /T023/ that renders as &#x13323;.
pattern T023 :: Char
pattern T023 = '\x13323'

-- | The Egyptian hieroglyph /T024/ that renders as &#x13324;.
pattern T024 :: Char
pattern T024 = '\x13324'

-- | The Egyptian hieroglyph /T025/ that renders as &#x13325;.
pattern T025 :: Char
pattern T025 = '\x13325'

-- | The Egyptian hieroglyph /T026/ that renders as &#x13326;.
pattern T026 :: Char
pattern T026 = '\x13326'

-- | The Egyptian hieroglyph /T027/ that renders as &#x13327;.
pattern T027 :: Char
pattern T027 = '\x13327'

-- | The Egyptian hieroglyph /T028/ that renders as &#x13328;.
pattern T028 :: Char
pattern T028 = '\x13328'

-- | The Egyptian hieroglyph /T029/ that renders as &#x13329;.
pattern T029 :: Char
pattern T029 = '\x13329'

-- | The Egyptian hieroglyph /T030/ that renders as &#x1332a;.
pattern T030 :: Char
pattern T030 = '\x1332a'

-- | The Egyptian hieroglyph /T031/ that renders as &#x1332b;.
pattern T031 :: Char
pattern T031 = '\x1332b'

-- | The Egyptian hieroglyph /T032/ that renders as &#x1332c;.
pattern T032 :: Char
pattern T032 = '\x1332c'

-- | The Egyptian hieroglyph /T032A/ that renders as &#x1332d;.
pattern T032A :: Char
pattern T032A = '\x1332d'

-- | The Egyptian hieroglyph /T033/ that renders as &#x1332e;.
pattern T033 :: Char
pattern T033 = '\x1332e'

-- | The Egyptian hieroglyph /T033A/ that renders as &#x1332f;.
pattern T033A :: Char
pattern T033A = '\x1332f'

-- | The Egyptian hieroglyph /T034/ that renders as &#x13330;.
pattern T034 :: Char
pattern T034 = '\x13330'

-- | The Egyptian hieroglyph /T035/ that renders as &#x13331;.
pattern T035 :: Char
pattern T035 = '\x13331'

-- | The Egyptian hieroglyph /T036/ that renders as &#x13332;.
pattern T036 :: Char
pattern T036 = '\x13332'

-- | The Egyptian hieroglyph /U001/ that renders as &#x13333;.
pattern U001 :: Char
pattern U001 = '\x13333'

-- | The Egyptian hieroglyph /U002/ that renders as &#x13334;.
pattern U002 :: Char
pattern U002 = '\x13334'

-- | The Egyptian hieroglyph /U003/ that renders as &#x13335;.
pattern U003 :: Char
pattern U003 = '\x13335'

-- | The Egyptian hieroglyph /U004/ that renders as &#x13336;.
pattern U004 :: Char
pattern U004 = '\x13336'

-- | The Egyptian hieroglyph /U005/ that renders as &#x13337;.
pattern U005 :: Char
pattern U005 = '\x13337'

-- | The Egyptian hieroglyph /U006/ that renders as &#x13338;.
pattern U006 :: Char
pattern U006 = '\x13338'

-- | The Egyptian hieroglyph /U006A/ that renders as &#x13339;.
pattern U006A :: Char
pattern U006A = '\x13339'

-- | The Egyptian hieroglyph /U006B/ that renders as &#x1333a;.
pattern U006B :: Char
pattern U006B = '\x1333a'

-- | The Egyptian hieroglyph /U007/ that renders as &#x1333b;.
pattern U007 :: Char
pattern U007 = '\x1333b'

-- | The Egyptian hieroglyph /U008/ that renders as &#x1333c;.
pattern U008 :: Char
pattern U008 = '\x1333c'

-- | The Egyptian hieroglyph /U009/ that renders as &#x1333d;.
pattern U009 :: Char
pattern U009 = '\x1333d'

-- | The Egyptian hieroglyph /U010/ that renders as &#x1333e;.
pattern U010 :: Char
pattern U010 = '\x1333e'

-- | The Egyptian hieroglyph /U011/ that renders as &#x1333f;.
pattern U011 :: Char
pattern U011 = '\x1333f'

-- | The Egyptian hieroglyph /U012/ that renders as &#x13340;.
pattern U012 :: Char
pattern U012 = '\x13340'

-- | The Egyptian hieroglyph /U013/ that renders as &#x13341;.
pattern U013 :: Char
pattern U013 = '\x13341'

-- | The Egyptian hieroglyph /U014/ that renders as &#x13342;.
pattern U014 :: Char
pattern U014 = '\x13342'

-- | The Egyptian hieroglyph /U015/ that renders as &#x13343;.
pattern U015 :: Char
pattern U015 = '\x13343'

-- | The Egyptian hieroglyph /U016/ that renders as &#x13344;.
pattern U016 :: Char
pattern U016 = '\x13344'

-- | The Egyptian hieroglyph /U017/ that renders as &#x13345;.
pattern U017 :: Char
pattern U017 = '\x13345'

-- | The Egyptian hieroglyph /U018/ that renders as &#x13346;.
pattern U018 :: Char
pattern U018 = '\x13346'

-- | The Egyptian hieroglyph /U019/ that renders as &#x13347;.
pattern U019 :: Char
pattern U019 = '\x13347'

-- | The Egyptian hieroglyph /U020/ that renders as &#x13348;.
pattern U020 :: Char
pattern U020 = '\x13348'

-- | The Egyptian hieroglyph /U021/ that renders as &#x13349;.
pattern U021 :: Char
pattern U021 = '\x13349'

-- | The Egyptian hieroglyph /U022/ that renders as &#x1334a;.
pattern U022 :: Char
pattern U022 = '\x1334a'

-- | The Egyptian hieroglyph /U023/ that renders as &#x1334b;.
pattern U023 :: Char
pattern U023 = '\x1334b'

-- | The Egyptian hieroglyph /U023A/ that renders as &#x1334c;.
pattern U023A :: Char
pattern U023A = '\x1334c'

-- | The Egyptian hieroglyph /U024/ that renders as &#x1334d;.
pattern U024 :: Char
pattern U024 = '\x1334d'

-- | The Egyptian hieroglyph /U025/ that renders as &#x1334e;.
pattern U025 :: Char
pattern U025 = '\x1334e'

-- | The Egyptian hieroglyph /U026/ that renders as &#x1334f;.
pattern U026 :: Char
pattern U026 = '\x1334f'

-- | The Egyptian hieroglyph /U027/ that renders as &#x13350;.
pattern U027 :: Char
pattern U027 = '\x13350'

-- | The Egyptian hieroglyph /U028/ that renders as &#x13351;.
pattern U028 :: Char
pattern U028 = '\x13351'

-- | The Egyptian hieroglyph /U029/ that renders as &#x13352;.
pattern U029 :: Char
pattern U029 = '\x13352'

-- | The Egyptian hieroglyph /U029A/ that renders as &#x13353;.
pattern U029A :: Char
pattern U029A = '\x13353'

-- | The Egyptian hieroglyph /U030/ that renders as &#x13354;.
pattern U030 :: Char
pattern U030 = '\x13354'

-- | The Egyptian hieroglyph /U031/ that renders as &#x13355;.
pattern U031 :: Char
pattern U031 = '\x13355'

-- | The Egyptian hieroglyph /U032/ that renders as &#x13356;.
pattern U032 :: Char
pattern U032 = '\x13356'

-- | The Egyptian hieroglyph /U032A/ that renders as &#x13357;.
pattern U032A :: Char
pattern U032A = '\x13357'

-- | The Egyptian hieroglyph /U033/ that renders as &#x13358;.
pattern U033 :: Char
pattern U033 = '\x13358'

-- | The Egyptian hieroglyph /U034/ that renders as &#x13359;.
pattern U034 :: Char
pattern U034 = '\x13359'

-- | The Egyptian hieroglyph /U035/ that renders as &#x1335a;.
pattern U035 :: Char
pattern U035 = '\x1335a'

-- | The Egyptian hieroglyph /U036/ that renders as &#x1335b;.
pattern U036 :: Char
pattern U036 = '\x1335b'

-- | The Egyptian hieroglyph /U037/ that renders as &#x1335c;.
pattern U037 :: Char
pattern U037 = '\x1335c'

-- | The Egyptian hieroglyph /U038/ that renders as &#x1335d;.
pattern U038 :: Char
pattern U038 = '\x1335d'

-- | The Egyptian hieroglyph /U039/ that renders as &#x1335e;.
pattern U039 :: Char
pattern U039 = '\x1335e'

-- | The Egyptian hieroglyph /U040/ that renders as &#x1335f;.
pattern U040 :: Char
pattern U040 = '\x1335f'

-- | The Egyptian hieroglyph /U041/ that renders as &#x13360;.
pattern U041 :: Char
pattern U041 = '\x13360'

-- | The Egyptian hieroglyph /U042/ that renders as &#x13361;.
pattern U042 :: Char
pattern U042 = '\x13361'

-- | The Egyptian hieroglyph /V001/ that renders as &#x13362;.
pattern V001 :: Char
pattern V001 = '\x13362'

-- | The Egyptian hieroglyph /V001A/ that renders as &#x13363;.
pattern V001A :: Char
pattern V001A = '\x13363'

-- | The Egyptian hieroglyph /V001B/ that renders as &#x13364;.
pattern V001B :: Char
pattern V001B = '\x13364'

-- | The Egyptian hieroglyph /V001C/ that renders as &#x13365;.
pattern V001C :: Char
pattern V001C = '\x13365'

-- | The Egyptian hieroglyph /V001D/ that renders as &#x13366;.
pattern V001D :: Char
pattern V001D = '\x13366'

-- | The Egyptian hieroglyph /V001E/ that renders as &#x13367;.
pattern V001E :: Char
pattern V001E = '\x13367'

-- | The Egyptian hieroglyph /V001F/ that renders as &#x13368;.
pattern V001F :: Char
pattern V001F = '\x13368'

-- | The Egyptian hieroglyph /V001G/ that renders as &#x13369;.
pattern V001G :: Char
pattern V001G = '\x13369'

-- | The Egyptian hieroglyph /V001H/ that renders as &#x1336a;.
pattern V001H :: Char
pattern V001H = '\x1336a'

-- | The Egyptian hieroglyph /V001I/ that renders as &#x1336b;.
pattern V001I :: Char
pattern V001I = '\x1336b'

-- | The Egyptian hieroglyph /V002/ that renders as &#x1336c;.
pattern V002 :: Char
pattern V002 = '\x1336c'

-- | The Egyptian hieroglyph /V002A/ that renders as &#x1336d;.
pattern V002A :: Char
pattern V002A = '\x1336d'

-- | The Egyptian hieroglyph /V003/ that renders as &#x1336e;.
pattern V003 :: Char
pattern V003 = '\x1336e'

-- | The Egyptian hieroglyph /V004/ that renders as &#x1336f;.
pattern V004 :: Char
pattern V004 = '\x1336f'

-- | The Egyptian hieroglyph /V005/ that renders as &#x13370;.
pattern V005 :: Char
pattern V005 = '\x13370'

-- | The Egyptian hieroglyph /V006/ that renders as &#x13371;.
pattern V006 :: Char
pattern V006 = '\x13371'

-- | The Egyptian hieroglyph /V007/ that renders as &#x13372;.
pattern V007 :: Char
pattern V007 = '\x13372'

-- | The Egyptian hieroglyph /V007A/ that renders as &#x13373;.
pattern V007A :: Char
pattern V007A = '\x13373'

-- | The Egyptian hieroglyph /V007B/ that renders as &#x13374;.
pattern V007B :: Char
pattern V007B = '\x13374'

-- | The Egyptian hieroglyph /V008/ that renders as &#x13375;.
pattern V008 :: Char
pattern V008 = '\x13375'

-- | The Egyptian hieroglyph /V009/ that renders as &#x13376;.
pattern V009 :: Char
pattern V009 = '\x13376'

-- | The Egyptian hieroglyph /V010/ that renders as &#x13377;.
pattern V010 :: Char
pattern V010 = '\x13377'

-- | The Egyptian hieroglyph /V011/ that renders as &#x13378;.
pattern V011 :: Char
pattern V011 = '\x13378'

-- | The Egyptian hieroglyph /V011A/ that renders as &#x13379;.
pattern V011A :: Char
pattern V011A = '\x13379'

-- | The Egyptian hieroglyph /V011B/ that renders as &#x1337a;.
pattern V011B :: Char
pattern V011B = '\x1337a'

-- | The Egyptian hieroglyph /V011C/ that renders as &#x1337b;.
pattern V011C :: Char
pattern V011C = '\x1337b'

-- | The Egyptian hieroglyph /V012/ that renders as &#x1337c;.
pattern V012 :: Char
pattern V012 = '\x1337c'

-- | The Egyptian hieroglyph /V012A/ that renders as &#x1337d;.
pattern V012A :: Char
pattern V012A = '\x1337d'

-- | The Egyptian hieroglyph /V012B/ that renders as &#x1337e;.
pattern V012B :: Char
pattern V012B = '\x1337e'

-- | The Egyptian hieroglyph /V013/ that renders as &#x1337f;.
pattern V013 :: Char
pattern V013 = '\x1337f'

-- | The Egyptian hieroglyph /V014/ that renders as &#x13380;.
pattern V014 :: Char
pattern V014 = '\x13380'

-- | The Egyptian hieroglyph /V015/ that renders as &#x13381;.
pattern V015 :: Char
pattern V015 = '\x13381'

-- | The Egyptian hieroglyph /V016/ that renders as &#x13382;.
pattern V016 :: Char
pattern V016 = '\x13382'

-- | The Egyptian hieroglyph /V017/ that renders as &#x13383;.
pattern V017 :: Char
pattern V017 = '\x13383'

-- | The Egyptian hieroglyph /V018/ that renders as &#x13384;.
pattern V018 :: Char
pattern V018 = '\x13384'

-- | The Egyptian hieroglyph /V019/ that renders as &#x13385;.
pattern V019 :: Char
pattern V019 = '\x13385'

-- | The Egyptian hieroglyph /V020/ that renders as &#x13386;.
pattern V020 :: Char
pattern V020 = '\x13386'

-- | The Egyptian hieroglyph /V020A/ that renders as &#x13387;.
pattern V020A :: Char
pattern V020A = '\x13387'

-- | The Egyptian hieroglyph /V020B/ that renders as &#x13388;.
pattern V020B :: Char
pattern V020B = '\x13388'

-- | The Egyptian hieroglyph /V020C/ that renders as &#x13389;.
pattern V020C :: Char
pattern V020C = '\x13389'

-- | The Egyptian hieroglyph /V020D/ that renders as &#x1338a;.
pattern V020D :: Char
pattern V020D = '\x1338a'

-- | The Egyptian hieroglyph /V020E/ that renders as &#x1338b;.
pattern V020E :: Char
pattern V020E = '\x1338b'

-- | The Egyptian hieroglyph /V020F/ that renders as &#x1338c;.
pattern V020F :: Char
pattern V020F = '\x1338c'

-- | The Egyptian hieroglyph /V020G/ that renders as &#x1338d;.
pattern V020G :: Char
pattern V020G = '\x1338d'

-- | The Egyptian hieroglyph /V020H/ that renders as &#x1338e;.
pattern V020H :: Char
pattern V020H = '\x1338e'

-- | The Egyptian hieroglyph /V020I/ that renders as &#x1338f;.
pattern V020I :: Char
pattern V020I = '\x1338f'

-- | The Egyptian hieroglyph /V020J/ that renders as &#x13390;.
pattern V020J :: Char
pattern V020J = '\x13390'

-- | The Egyptian hieroglyph /V020K/ that renders as &#x13391;.
pattern V020K :: Char
pattern V020K = '\x13391'

-- | The Egyptian hieroglyph /V020L/ that renders as &#x13392;.
pattern V020L :: Char
pattern V020L = '\x13392'

-- | The Egyptian hieroglyph /V021/ that renders as &#x13393;.
pattern V021 :: Char
pattern V021 = '\x13393'

-- | The Egyptian hieroglyph /V022/ that renders as &#x13394;.
pattern V022 :: Char
pattern V022 = '\x13394'

-- | The Egyptian hieroglyph /V023/ that renders as &#x13395;.
pattern V023 :: Char
pattern V023 = '\x13395'

-- | The Egyptian hieroglyph /V023A/ that renders as &#x13396;.
pattern V023A :: Char
pattern V023A = '\x13396'

-- | The Egyptian hieroglyph /V024/ that renders as &#x13397;.
pattern V024 :: Char
pattern V024 = '\x13397'

-- | The Egyptian hieroglyph /V025/ that renders as &#x13398;.
pattern V025 :: Char
pattern V025 = '\x13398'

-- | The Egyptian hieroglyph /V026/ that renders as &#x13399;.
pattern V026 :: Char
pattern V026 = '\x13399'

-- | The Egyptian hieroglyph /V027/ that renders as &#x1339a;.
pattern V027 :: Char
pattern V027 = '\x1339a'

-- | The Egyptian hieroglyph /V028/ that renders as &#x1339b;.
pattern V028 :: Char
pattern V028 = '\x1339b'

-- | The Egyptian hieroglyph /V028A/ that renders as &#x1339c;.
pattern V028A :: Char
pattern V028A = '\x1339c'

-- | The Egyptian hieroglyph /V029/ that renders as &#x1339d;.
pattern V029 :: Char
pattern V029 = '\x1339d'

-- | The Egyptian hieroglyph /V029A/ that renders as &#x1339e;.
pattern V029A :: Char
pattern V029A = '\x1339e'

-- | The Egyptian hieroglyph /V030/ that renders as &#x1339f;.
pattern V030 :: Char
pattern V030 = '\x1339f'

-- | The Egyptian hieroglyph /V030A/ that renders as &#x133a0;.
pattern V030A :: Char
pattern V030A = '\x133a0'

-- | The Egyptian hieroglyph /V031/ that renders as &#x133a1;.
pattern V031 :: Char
pattern V031 = '\x133a1'

-- | The Egyptian hieroglyph /V031A/ that renders as &#x133a2;.
pattern V031A :: Char
pattern V031A = '\x133a2'

-- | The Egyptian hieroglyph /V032/ that renders as &#x133a3;.
pattern V032 :: Char
pattern V032 = '\x133a3'

-- | The Egyptian hieroglyph /V033/ that renders as &#x133a4;.
pattern V033 :: Char
pattern V033 = '\x133a4'

-- | The Egyptian hieroglyph /V033A/ that renders as &#x133a5;.
pattern V033A :: Char
pattern V033A = '\x133a5'

-- | The Egyptian hieroglyph /V034/ that renders as &#x133a6;.
pattern V034 :: Char
pattern V034 = '\x133a6'

-- | The Egyptian hieroglyph /V035/ that renders as &#x133a7;.
pattern V035 :: Char
pattern V035 = '\x133a7'

-- | The Egyptian hieroglyph /V036/ that renders as &#x133a8;.
pattern V036 :: Char
pattern V036 = '\x133a8'

-- | The Egyptian hieroglyph /V037/ that renders as &#x133a9;.
pattern V037 :: Char
pattern V037 = '\x133a9'

-- | The Egyptian hieroglyph /V037A/ that renders as &#x133aa;.
pattern V037A :: Char
pattern V037A = '\x133aa'

-- | The Egyptian hieroglyph /V038/ that renders as &#x133ab;.
pattern V038 :: Char
pattern V038 = '\x133ab'

-- | The Egyptian hieroglyph /V039/ that renders as &#x133ac;.
pattern V039 :: Char
pattern V039 = '\x133ac'

-- | The Egyptian hieroglyph /V040/ that renders as &#x133ad;.
pattern V040 :: Char
pattern V040 = '\x133ad'

-- | The Egyptian hieroglyph /V040A/ that renders as &#x133ae;.
pattern V040A :: Char
pattern V040A = '\x133ae'

-- | The Egyptian hieroglyph /W001/ that renders as &#x133af;.
pattern W001 :: Char
pattern W001 = '\x133af'

-- | The Egyptian hieroglyph /W002/ that renders as &#x133b0;.
pattern W002 :: Char
pattern W002 = '\x133b0'

-- | The Egyptian hieroglyph /W003/ that renders as &#x133b1;.
pattern W003 :: Char
pattern W003 = '\x133b1'

-- | The Egyptian hieroglyph /W003A/ that renders as &#x133b2;.
pattern W003A :: Char
pattern W003A = '\x133b2'

-- | The Egyptian hieroglyph /W004/ that renders as &#x133b3;.
pattern W004 :: Char
pattern W004 = '\x133b3'

-- | The Egyptian hieroglyph /W005/ that renders as &#x133b4;.
pattern W005 :: Char
pattern W005 = '\x133b4'

-- | The Egyptian hieroglyph /W006/ that renders as &#x133b5;.
pattern W006 :: Char
pattern W006 = '\x133b5'

-- | The Egyptian hieroglyph /W007/ that renders as &#x133b6;.
pattern W007 :: Char
pattern W007 = '\x133b6'

-- | The Egyptian hieroglyph /W008/ that renders as &#x133b7;.
pattern W008 :: Char
pattern W008 = '\x133b7'

-- | The Egyptian hieroglyph /W009/ that renders as &#x133b8;.
pattern W009 :: Char
pattern W009 = '\x133b8'

-- | The Egyptian hieroglyph /W009A/ that renders as &#x133b9;.
pattern W009A :: Char
pattern W009A = '\x133b9'

-- | The Egyptian hieroglyph /W010/ that renders as &#x133ba;.
pattern W010 :: Char
pattern W010 = '\x133ba'

-- | The Egyptian hieroglyph /W010A/ that renders as &#x133bb;.
pattern W010A :: Char
pattern W010A = '\x133bb'

-- | The Egyptian hieroglyph /W011/ that renders as &#x133bc;.
pattern W011 :: Char
pattern W011 = '\x133bc'

-- | The Egyptian hieroglyph /W012/ that renders as &#x133bd;.
pattern W012 :: Char
pattern W012 = '\x133bd'

-- | The Egyptian hieroglyph /W013/ that renders as &#x133be;.
pattern W013 :: Char
pattern W013 = '\x133be'

-- | The Egyptian hieroglyph /W014/ that renders as &#x133bf;.
pattern W014 :: Char
pattern W014 = '\x133bf'

-- | The Egyptian hieroglyph /W014A/ that renders as &#x133c0;.
pattern W014A :: Char
pattern W014A = '\x133c0'

-- | The Egyptian hieroglyph /W015/ that renders as &#x133c1;.
pattern W015 :: Char
pattern W015 = '\x133c1'

-- | The Egyptian hieroglyph /W016/ that renders as &#x133c2;.
pattern W016 :: Char
pattern W016 = '\x133c2'

-- | The Egyptian hieroglyph /W017/ that renders as &#x133c3;.
pattern W017 :: Char
pattern W017 = '\x133c3'

-- | The Egyptian hieroglyph /W017A/ that renders as &#x133c4;.
pattern W017A :: Char
pattern W017A = '\x133c4'

-- | The Egyptian hieroglyph /W018/ that renders as &#x133c5;.
pattern W018 :: Char
pattern W018 = '\x133c5'

-- | The Egyptian hieroglyph /W018A/ that renders as &#x133c6;.
pattern W018A :: Char
pattern W018A = '\x133c6'

-- | The Egyptian hieroglyph /W019/ that renders as &#x133c7;.
pattern W019 :: Char
pattern W019 = '\x133c7'

-- | The Egyptian hieroglyph /W020/ that renders as &#x133c8;.
pattern W020 :: Char
pattern W020 = '\x133c8'

-- | The Egyptian hieroglyph /W021/ that renders as &#x133c9;.
pattern W021 :: Char
pattern W021 = '\x133c9'

-- | The Egyptian hieroglyph /W022/ that renders as &#x133ca;.
pattern W022 :: Char
pattern W022 = '\x133ca'

-- | The Egyptian hieroglyph /W023/ that renders as &#x133cb;.
pattern W023 :: Char
pattern W023 = '\x133cb'

-- | The Egyptian hieroglyph /W024/ that renders as &#x133cc;.
pattern W024 :: Char
pattern W024 = '\x133cc'

-- | The Egyptian hieroglyph /W024A/ that renders as &#x133cd;.
pattern W024A :: Char
pattern W024A = '\x133cd'

-- | The Egyptian hieroglyph /W025/ that renders as &#x133ce;.
pattern W025 :: Char
pattern W025 = '\x133ce'

-- | The Egyptian hieroglyph /X001/ that renders as &#x133cf;.
pattern X001 :: Char
pattern X001 = '\x133cf'

-- | The Egyptian hieroglyph /X002/ that renders as &#x133d0;.
pattern X002 :: Char
pattern X002 = '\x133d0'

-- | The Egyptian hieroglyph /X003/ that renders as &#x133d1;.
pattern X003 :: Char
pattern X003 = '\x133d1'

-- | The Egyptian hieroglyph /X004/ that renders as &#x133d2;.
pattern X004 :: Char
pattern X004 = '\x133d2'

-- | The Egyptian hieroglyph /X004A/ that renders as &#x133d3;.
pattern X004A :: Char
pattern X004A = '\x133d3'

-- | The Egyptian hieroglyph /X004B/ that renders as &#x133d4;.
pattern X004B :: Char
pattern X004B = '\x133d4'

-- | The Egyptian hieroglyph /X005/ that renders as &#x133d5;.
pattern X005 :: Char
pattern X005 = '\x133d5'

-- | The Egyptian hieroglyph /X006/ that renders as &#x133d6;.
pattern X006 :: Char
pattern X006 = '\x133d6'

-- | The Egyptian hieroglyph /X006A/ that renders as &#x133d7;.
pattern X006A :: Char
pattern X006A = '\x133d7'

-- | The Egyptian hieroglyph /X007/ that renders as &#x133d8;.
pattern X007 :: Char
pattern X007 = '\x133d8'

-- | The Egyptian hieroglyph /X008/ that renders as &#x133d9;.
pattern X008 :: Char
pattern X008 = '\x133d9'

-- | The Egyptian hieroglyph /X008A/ that renders as &#x133da;.
pattern X008A :: Char
pattern X008A = '\x133da'

-- | The Egyptian hieroglyph /Y001/ that renders as &#x133db;.
pattern Y001 :: Char
pattern Y001 = '\x133db'

-- | The Egyptian hieroglyph /Y001A/ that renders as &#x133dc;.
pattern Y001A :: Char
pattern Y001A = '\x133dc'

-- | The Egyptian hieroglyph /Y002/ that renders as &#x133dd;.
pattern Y002 :: Char
pattern Y002 = '\x133dd'

-- | The Egyptian hieroglyph /Y003/ that renders as &#x133de;.
pattern Y003 :: Char
pattern Y003 = '\x133de'

-- | The Egyptian hieroglyph /Y004/ that renders as &#x133df;.
pattern Y004 :: Char
pattern Y004 = '\x133df'

-- | The Egyptian hieroglyph /Y005/ that renders as &#x133e0;.
pattern Y005 :: Char
pattern Y005 = '\x133e0'

-- | The Egyptian hieroglyph /Y006/ that renders as &#x133e1;.
pattern Y006 :: Char
pattern Y006 = '\x133e1'

-- | The Egyptian hieroglyph /Y007/ that renders as &#x133e2;.
pattern Y007 :: Char
pattern Y007 = '\x133e2'

-- | The Egyptian hieroglyph /Y008/ that renders as &#x133e3;.
pattern Y008 :: Char
pattern Y008 = '\x133e3'

-- | The Egyptian hieroglyph /Z001/ that renders as &#x133e4;.
pattern Z001 :: Char
pattern Z001 = '\x133e4'

-- | The Egyptian hieroglyph /Z002/ that renders as &#x133e5;.
pattern Z002 :: Char
pattern Z002 = '\x133e5'

-- | The Egyptian hieroglyph /Z002A/ that renders as &#x133e6;.
pattern Z002A :: Char
pattern Z002A = '\x133e6'

-- | The Egyptian hieroglyph /Z002B/ that renders as &#x133e7;.
pattern Z002B :: Char
pattern Z002B = '\x133e7'

-- | The Egyptian hieroglyph /Z002C/ that renders as &#x133e8;.
pattern Z002C :: Char
pattern Z002C = '\x133e8'

-- | The Egyptian hieroglyph /Z002D/ that renders as &#x133e9;.
pattern Z002D :: Char
pattern Z002D = '\x133e9'

-- | The Egyptian hieroglyph /Z003/ that renders as &#x133ea;.
pattern Z003 :: Char
pattern Z003 = '\x133ea'

-- | The Egyptian hieroglyph /Z003A/ that renders as &#x133eb;.
pattern Z003A :: Char
pattern Z003A = '\x133eb'

-- | The Egyptian hieroglyph /Z003B/ that renders as &#x133ec;.
pattern Z003B :: Char
pattern Z003B = '\x133ec'

-- | The Egyptian hieroglyph /Z004/ that renders as &#x133ed;.
pattern Z004 :: Char
pattern Z004 = '\x133ed'

-- | The Egyptian hieroglyph /Z004A/ that renders as &#x133ee;.
pattern Z004A :: Char
pattern Z004A = '\x133ee'

-- | The Egyptian hieroglyph /Z005/ that renders as &#x133ef;.
pattern Z005 :: Char
pattern Z005 = '\x133ef'

-- | The Egyptian hieroglyph /Z005A/ that renders as &#x133f0;.
pattern Z005A :: Char
pattern Z005A = '\x133f0'

-- | The Egyptian hieroglyph /Z006/ that renders as &#x133f1;.
pattern Z006 :: Char
pattern Z006 = '\x133f1'

-- | The Egyptian hieroglyph /Z007/ that renders as &#x133f2;.
pattern Z007 :: Char
pattern Z007 = '\x133f2'

-- | The Egyptian hieroglyph /Z008/ that renders as &#x133f3;.
pattern Z008 :: Char
pattern Z008 = '\x133f3'

-- | The Egyptian hieroglyph /Z009/ that renders as &#x133f4;.
pattern Z009 :: Char
pattern Z009 = '\x133f4'

-- | The Egyptian hieroglyph /Z010/ that renders as &#x133f5;.
pattern Z010 :: Char
pattern Z010 = '\x133f5'

-- | The Egyptian hieroglyph /Z011/ that renders as &#x133f6;.
pattern Z011 :: Char
pattern Z011 = '\x133f6'

-- | The Egyptian hieroglyph /Z012/ that renders as &#x133f7;.
pattern Z012 :: Char
pattern Z012 = '\x133f7'

-- | The Egyptian hieroglyph /Z013/ that renders as &#x133f8;.
pattern Z013 :: Char
pattern Z013 = '\x133f8'

-- | The Egyptian hieroglyph /Z014/ that renders as &#x133f9;.
pattern Z014 :: Char
pattern Z014 = '\x133f9'

-- | The Egyptian hieroglyph /Z015/ that renders as &#x133fa;.
pattern Z015 :: Char
pattern Z015 = '\x133fa'

-- | The Egyptian hieroglyph /Z015A/ that renders as &#x133fb;.
pattern Z015A :: Char
pattern Z015A = '\x133fb'

-- | The Egyptian hieroglyph /Z015B/ that renders as &#x133fc;.
pattern Z015B :: Char
pattern Z015B = '\x133fc'

-- | The Egyptian hieroglyph /Z015C/ that renders as &#x133fd;.
pattern Z015C :: Char
pattern Z015C = '\x133fd'

-- | The Egyptian hieroglyph /Z015D/ that renders as &#x133fe;.
pattern Z015D :: Char
pattern Z015D = '\x133fe'

-- | The Egyptian hieroglyph /Z015E/ that renders as &#x133ff;.
pattern Z015E :: Char
pattern Z015E = '\x133ff'

-- | The Egyptian hieroglyph /Z015F/ that renders as &#x13400;.
pattern Z015F :: Char
pattern Z015F = '\x13400'

-- | The Egyptian hieroglyph /Z015G/ that renders as &#x13401;.
pattern Z015G :: Char
pattern Z015G = '\x13401'

-- | The Egyptian hieroglyph /Z015H/ that renders as &#x13402;.
pattern Z015H :: Char
pattern Z015H = '\x13402'

-- | The Egyptian hieroglyph /Z015I/ that renders as &#x13403;.
pattern Z015I :: Char
pattern Z015I = '\x13403'

-- | The Egyptian hieroglyph /Z016/ that renders as &#x13404;.
pattern Z016 :: Char
pattern Z016 = '\x13404'

-- | The Egyptian hieroglyph /Z016A/ that renders as &#x13405;.
pattern Z016A :: Char
pattern Z016A = '\x13405'

-- | The Egyptian hieroglyph /Z016B/ that renders as &#x13406;.
pattern Z016B :: Char
pattern Z016B = '\x13406'

-- | The Egyptian hieroglyph /Z016C/ that renders as &#x13407;.
pattern Z016C :: Char
pattern Z016C = '\x13407'

-- | The Egyptian hieroglyph /Z016D/ that renders as &#x13408;.
pattern Z016D :: Char
pattern Z016D = '\x13408'

-- | The Egyptian hieroglyph /Z016E/ that renders as &#x13409;.
pattern Z016E :: Char
pattern Z016E = '\x13409'

-- | The Egyptian hieroglyph /Z016F/ that renders as &#x1340a;.
pattern Z016F :: Char
pattern Z016F = '\x1340a'

-- | The Egyptian hieroglyph /Z016G/ that renders as &#x1340b;.
pattern Z016G :: Char
pattern Z016G = '\x1340b'

-- | The Egyptian hieroglyph /Z016H/ that renders as &#x1340c;.
pattern Z016H :: Char
pattern Z016H = '\x1340c'

-- | The Egyptian hieroglyph /AA001/ that renders as &#x1340d;.
pattern AA001 :: Char
pattern AA001 = '\x1340d'

-- | The Egyptian hieroglyph /AA002/ that renders as &#x1340e;.
pattern AA002 :: Char
pattern AA002 = '\x1340e'

-- | The Egyptian hieroglyph /AA003/ that renders as &#x1340f;.
pattern AA003 :: Char
pattern AA003 = '\x1340f'

-- | The Egyptian hieroglyph /AA004/ that renders as &#x13410;.
pattern AA004 :: Char
pattern AA004 = '\x13410'

-- | The Egyptian hieroglyph /AA005/ that renders as &#x13411;.
pattern AA005 :: Char
pattern AA005 = '\x13411'

-- | The Egyptian hieroglyph /AA006/ that renders as &#x13412;.
pattern AA006 :: Char
pattern AA006 = '\x13412'

-- | The Egyptian hieroglyph /AA007/ that renders as &#x13413;.
pattern AA007 :: Char
pattern AA007 = '\x13413'

-- | The Egyptian hieroglyph /AA007A/ that renders as &#x13414;.
pattern AA007A :: Char
pattern AA007A = '\x13414'

-- | The Egyptian hieroglyph /AA007B/ that renders as &#x13415;.
pattern AA007B :: Char
pattern AA007B = '\x13415'

-- | The Egyptian hieroglyph /AA008/ that renders as &#x13416;.
pattern AA008 :: Char
pattern AA008 = '\x13416'

-- | The Egyptian hieroglyph /AA009/ that renders as &#x13417;.
pattern AA009 :: Char
pattern AA009 = '\x13417'

-- | The Egyptian hieroglyph /AA010/ that renders as &#x13418;.
pattern AA010 :: Char
pattern AA010 = '\x13418'

-- | The Egyptian hieroglyph /AA011/ that renders as &#x13419;.
pattern AA011 :: Char
pattern AA011 = '\x13419'

-- | The Egyptian hieroglyph /AA012/ that renders as &#x1341a;.
pattern AA012 :: Char
pattern AA012 = '\x1341a'

-- | The Egyptian hieroglyph /AA013/ that renders as &#x1341b;.
pattern AA013 :: Char
pattern AA013 = '\x1341b'

-- | The Egyptian hieroglyph /AA014/ that renders as &#x1341c;.
pattern AA014 :: Char
pattern AA014 = '\x1341c'

-- | The Egyptian hieroglyph /AA015/ that renders as &#x1341d;.
pattern AA015 :: Char
pattern AA015 = '\x1341d'

-- | The Egyptian hieroglyph /AA016/ that renders as &#x1341e;.
pattern AA016 :: Char
pattern AA016 = '\x1341e'

-- | The Egyptian hieroglyph /AA017/ that renders as &#x1341f;.
pattern AA017 :: Char
pattern AA017 = '\x1341f'

-- | The Egyptian hieroglyph /AA018/ that renders as &#x13420;.
pattern AA018 :: Char
pattern AA018 = '\x13420'

-- | The Egyptian hieroglyph /AA019/ that renders as &#x13421;.
pattern AA019 :: Char
pattern AA019 = '\x13421'

-- | The Egyptian hieroglyph /AA020/ that renders as &#x13422;.
pattern AA020 :: Char
pattern AA020 = '\x13422'

-- | The Egyptian hieroglyph /AA021/ that renders as &#x13423;.
pattern AA021 :: Char
pattern AA021 = '\x13423'

-- | The Egyptian hieroglyph /AA022/ that renders as &#x13424;.
pattern AA022 :: Char
pattern AA022 = '\x13424'

-- | The Egyptian hieroglyph /AA023/ that renders as &#x13425;.
pattern AA023 :: Char
pattern AA023 = '\x13425'

-- | The Egyptian hieroglyph /AA024/ that renders as &#x13426;.
pattern AA024 :: Char
pattern AA024 = '\x13426'

-- | The Egyptian hieroglyph /AA025/ that renders as &#x13427;.
pattern AA025 :: Char
pattern AA025 = '\x13427'

-- | The Egyptian hieroglyph /AA026/ that renders as &#x13428;.
pattern AA026 :: Char
pattern AA026 = '\x13428'

-- | The Egyptian hieroglyph /AA027/ that renders as &#x13429;.
pattern AA027 :: Char
pattern AA027 = '\x13429'

-- | The Egyptian hieroglyph /AA028/ that renders as &#x1342a;.
pattern AA028 :: Char
pattern AA028 = '\x1342a'

-- | The Egyptian hieroglyph /AA029/ that renders as &#x1342b;.
pattern AA029 :: Char
pattern AA029 = '\x1342b'

-- | The Egyptian hieroglyph /AA030/ that renders as &#x1342c;.
pattern AA030 :: Char
pattern AA030 = '\x1342c'

-- | The Egyptian hieroglyph /AA031/ that renders as &#x1342d;.
pattern AA031 :: Char
pattern AA031 = '\x1342d'

-- | The Egyptian hieroglyph /AA032/ that renders as &#x1342e;.
pattern AA032 :: Char
pattern AA032 = '\x1342e'
