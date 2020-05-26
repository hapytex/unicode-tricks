{-# LANGUAGE FlexibleInstances, FunctionalDependencies, PatternSynonyms, Safe #-}

{-|
Module      : Data.Char.Combining
Description : A module to work with combining characters in Unicode.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

In Unicode a total of 839 codepoints are defined as /combining/ characters. These characters are put after another character, to manipulate the previous one. For example by putting a /grave accent/ on an @a@.

This module aims to make it more convenient to work with combining characters. It provides functions to map the 'CombiningCharacter's to 'Char's and vice versa. It furthermore defines an operator '(*^)' that can
be used to apply one, or multiple 'CombiningCharacter's to a given character. We use a typeclass for this to allow working with different types. This allows "stacking" combining characters like:

> 'a' *^ CombiningGraveAccent *^ CombiningPlusSignBelow

This will first combine 'CombiningGraveAccent' on the @a@ character and then 'CombiningPlusSignBelow', the result is a string @"a\\768\\799"@.

The module contains a set of pattern synonyms to make working with the 'CombiningCharacter's less verbose.
-}

module Data.Char.Combining (
    -- * Combining characters
    CombiningCharacter(..), CombiningChar
    -- * A set of combining characters
  , CombiningSequence(..)
    -- * Conversions from and to 'CombiningCharacter'
  , combiningToUnicode, combiningCharacter, combiningCharacter', isCombiningCharacter, stripCombiningSequence, stripCombinings
    -- * Applying a 'CombiningCharacter' to a 'Char'
  , ApplyCombine((*^), (*^!))
    -- * Decompose characters
  , decomposeCombining, decomposeCombiningSequence
    -- * Compose 'Char'acters and 'CombiningCharacter's to a dedicated 'Char'
  , composeCombining, composeCombining', composeCombiningSequence, composeCombiningSequence'
    -- * Pattern synonyms for the combining characters
  ,  pattern GraveAccent                                   , pattern AcuteAccent                                   , pattern CircumflexAccent                              , pattern Tilde
  ,  pattern Macron                                        , pattern Overline                                      , pattern Breve                                         , pattern DotAbove
  ,  pattern Diaeresis                                     , pattern HookAbove                                     , pattern RingAbove                                     , pattern DoubleAcuteAccent
  ,  pattern Caron                                         , pattern VerticalLineAbove                             , pattern DoubleVerticalLineAbove                       , pattern DoubleGraveAccent
  ,  pattern Candrabindu                                   , pattern InvertedBreve                                 , pattern TurnedCommaAbove                              , pattern CommaAbove
  ,  pattern ReversedCommaAbove                            , pattern CommaAboveRight                               , pattern GraveAccentBelow                              , pattern AcuteAccentBelow
  ,  pattern LeftTackBelow                                 , pattern RightTackBelow                                , pattern LeftAngleAbove                                , pattern Horn
  ,  pattern LeftHalfRingBelow                             , pattern UpTackBelow                                   , pattern DownTackBelow                                 , pattern PlusSignBelow
  ,  pattern MinusSignBelow                                , pattern PalatalizedHookBelow                          , pattern RetroflexHookBelow                            , pattern DotBelow
  ,  pattern DiaeresisBelow                                , pattern RingBelow                                     , pattern CommaBelow                                    , pattern Cedilla
  ,  pattern Ogonek                                        , pattern VerticalLineBelow                             , pattern BridgeBelow                                   , pattern InvertedDoubleArchBelow
  ,  pattern CaronBelow                                    , pattern CircumflexAccentBelow                         , pattern BreveBelow                                    , pattern InvertedBreveBelow
  ,  pattern TildeBelow                                    , pattern MacronBelow                                   , pattern LowLine                                       , pattern DoubleLowLine
  ,  pattern TildeOverlay                                  , pattern ShortStrokeOverlay                            , pattern LongStrokeOverlay                             , pattern ShortSolidusOverlay
  ,  pattern LongSolidusOverlay                            , pattern RightHalfRingBelow                            , pattern InvertedBridgeBelow                           , pattern SquareBelow
  ,  pattern SeagullBelow                                  , pattern XAbove                                        , pattern VerticalTilde                                 , pattern DoubleOverline
  ,  pattern GraveToneMark                                 , pattern AcuteToneMark                                 , pattern GreekPerispomeni                              , pattern GreekKoronis
  ,  pattern GreekDialytikaTonos                           , pattern GreekYpogegrammeni                            , pattern BridgeAbove                                   , pattern EqualsSignBelow
  ,  pattern DoubleVerticalLineBelow                       , pattern LeftAngleBelow                                , pattern NotTildeAbove                                 , pattern HomotheticAbove
  ,  pattern AlmostEqualToAbove                            , pattern LeftRightArrowBelow                           , pattern UpwardsArrowBelow                             , pattern RightArrowheadAbove
  ,  pattern LeftHalfRingAbove                             , pattern Fermata                                       , pattern XBelow                                        , pattern LeftArrowheadBelow
  ,  pattern RightArrowheadBelow                           , pattern RightArrowheadAndUpArrowheadBelow             , pattern RightHalfRingAbove                            , pattern DotAboveRight
  ,  pattern AsteriskBelow                                 , pattern DoubleRingBelow                               , pattern ZigzagAbove                                   , pattern DoubleBreveBelow
  ,  pattern DoubleBreve                                   , pattern DoubleMacron                                  , pattern DoubleMacronBelow                             , pattern DoubleTilde
  ,  pattern DoubleInvertedBreve                           , pattern DoubleRightwardsArrowBelow                    , pattern LatinSmallLetterA                             , pattern LatinSmallLetterE
  ,  pattern LatinSmallLetterI                             , pattern LatinSmallLetterO                             , pattern LatinSmallLetterU                             , pattern LatinSmallLetterC
  ,  pattern LatinSmallLetterD                             , pattern LatinSmallLetterH                             , pattern LatinSmallLetterM                             , pattern LatinSmallLetterR
  ,  pattern LatinSmallLetterT                             , pattern LatinSmallLetterV                             , pattern LatinSmallLetterX                             , pattern CyrillicTitlo
  ,  pattern CyrillicPalatalization                        , pattern CyrillicDasiaPneumata                         , pattern CyrillicPsiliPneumata                         , pattern CyrillicPokrytie
  ,  pattern NkoShortHighTone                              , pattern NkoShortLowTone                               , pattern NkoShortRisingTone                            , pattern NkoLongDescendingTone
  ,  pattern NkoLongHighTone                               , pattern NkoLongLowTone                                , pattern NkoLongRisingTone                             , pattern NkoNasalizationMark
  ,  pattern NkoDoubleDotAbove                             , pattern EthiopicGeminationAndVowelLengthMark          , pattern EthiopicVowelLengthMark                       , pattern EthiopicGeminationMark
  ,  pattern TaiThamCryptogrammicDot                       , pattern DoubledCircumflexAccent                       , pattern DiaeresisRing                                 , pattern Infinity
  ,  pattern DownwardsArrow                                , pattern TripleDot                                     , pattern XXBelow                                       , pattern WigglyLineBelow
  ,  pattern OpenMarkBelow                                 , pattern DoubleOpenMarkBelow                           , pattern LightCentralizationStrokeBelow                , pattern StrongCentralizationStrokeBelow
  ,  pattern ParenthesesAbove                              , pattern DoubleParenthesesAbove                        , pattern ParenthesesBelow                              , pattern BalineseMusicalSymbolTegeh
  ,  pattern BalineseMusicalSymbolEndep                    , pattern BalineseMusicalSymbolKempul                   , pattern BalineseMusicalSymbolKempli                   , pattern BalineseMusicalSymbolJegogan
  ,  pattern BalineseMusicalSymbolKempulWithJegogan        , pattern BalineseMusicalSymbolKempliWithJegogan        , pattern BalineseMusicalSymbolBende                    , pattern BalineseMusicalSymbolGong
  ,  pattern DottedGraveAccent                             , pattern DottedAcuteAccent                             , pattern SnakeBelow                                    , pattern SuspensionMark
  ,  pattern MacronAcute                                   , pattern GraveMacron                                   , pattern MacronGrave                                   , pattern AcuteMacron
  ,  pattern GraveAcuteGrave                               , pattern AcuteGraveAcute                               , pattern LatinSmallLetterRBelow                        , pattern BreveMacron
  ,  pattern MacronBreve                                   , pattern DoubleCircumflexAbove                         , pattern OgonekAbove                                   , pattern ZigzagBelow
  ,  pattern IsBelow                                       , pattern UrAbove                                       , pattern UsAbove                                       , pattern LatinSmallLetterFlattenedOpenAAbove
  ,  pattern LatinSmallLetterAe                            , pattern LatinSmallLetterAo                            , pattern LatinSmallLetterAv                            , pattern LatinSmallLetterCCedilla
  ,  pattern LatinSmallLetterInsularD                      , pattern LatinSmallLetterEth                           , pattern LatinSmallLetterG                             , pattern LatinLetterSmallCapitalG
  ,  pattern LatinSmallLetterK                             , pattern LatinSmallLetterL                             , pattern LatinLetterSmallCapitalL                      , pattern LatinLetterSmallCapitalM
  ,  pattern LatinSmallLetterN                             , pattern LatinLetterSmallCapitalN                      , pattern LatinLetterSmallCapitalR                      , pattern LatinSmallLetterRRotunda
  ,  pattern LatinSmallLetterS                             , pattern LatinSmallLetterLongS                         , pattern LatinSmallLetterZ                             , pattern LatinSmallLetterAlpha
  ,  pattern LatinSmallLetterB                             , pattern LatinSmallLetterBeta                          , pattern LatinSmallLetterSchwa                         , pattern LatinSmallLetterF
  ,  pattern LatinSmallLetterLWithDoubleMiddleTilde        , pattern LatinSmallLetterOWithLightCentralizationStroke, pattern LatinSmallLetterP                             , pattern LatinSmallLetterEsh
  ,  pattern LatinSmallLetterUWithLightCentralizationStroke, pattern LatinSmallLetterW                             , pattern LatinSmallLetterAWithDiaeresis                , pattern LatinSmallLetterOWithDiaeresis
  ,  pattern LatinSmallLetterUWithDiaeresis                , pattern UpTackAbove                                   , pattern DeletionMark                                  , pattern DoubleInvertedBreveBelow
  ,  pattern AlmostEqualToBelow                            , pattern LeftArrowheadAbove                            , pattern RightArrowheadAndDownArrowheadBelow           , pattern LeftHarpoonAbove
  ,  pattern RightHarpoonAbove                             , pattern LongVerticalLineOverlay                       , pattern ShortVerticalLineOverlay                      , pattern AnticlockwiseArrowAbove
  ,  pattern ClockwiseArrowAbove                           , pattern LeftArrowAbove                                , pattern RightArrowAbove                               , pattern RingOverlay
  ,  pattern ClockwiseRingOverlay                          , pattern AnticlockwiseRingOverlay                      , pattern ThreeDotsAbove                                , pattern FourDotsAbove
  ,  pattern LeftRightArrowAbove                           , pattern ReverseSolidusOverlay                         , pattern DoubleVerticalStrokeOverlay                   , pattern AnnuitySymbol
  ,  pattern TripleUnderdot                                , pattern WideBridgeAbove                               , pattern LeftwardsArrowOverlay                         , pattern LongDoubleSolidusOverlay
  ,  pattern RightwardsHarpoonWithBarbDownwards            , pattern LeftwardsHarpoonWithBarbDownwards             , pattern LeftArrowBelow                                , pattern RightArrowBelow
  ,  pattern AsteriskAbove                                 , pattern CopticNiAbove                                 , pattern CopticSpiritusAsper                           , pattern CopticSpiritusLenis
  ,  pattern CyrillicLetterBe                              , pattern CyrillicLetterVe                              , pattern CyrillicLetterGhe                             , pattern CyrillicLetterDe
  ,  pattern CyrillicLetterZhe                             , pattern CyrillicLetterZe                              , pattern CyrillicLetterKa                              , pattern CyrillicLetterEl
  ,  pattern CyrillicLetterEm                              , pattern CyrillicLetterEn                              , pattern CyrillicLetterO                               , pattern CyrillicLetterPe
  ,  pattern CyrillicLetterEr                              , pattern CyrillicLetterEs                              , pattern CyrillicLetterTe                              , pattern CyrillicLetterHa
  ,  pattern CyrillicLetterTse                             , pattern CyrillicLetterChe                             , pattern CyrillicLetterSha                             , pattern CyrillicLetterShcha
  ,  pattern CyrillicLetterFita                            , pattern CyrillicLetterEsTe                            , pattern CyrillicLetterA                               , pattern CyrillicLetterIe
  ,  pattern CyrillicLetterDjerv                           , pattern CyrillicLetterMonographUk                     , pattern CyrillicLetterYat                             , pattern CyrillicLetterYu
  ,  pattern CyrillicLetterIotifiedA                       , pattern CyrillicLetterLittleYus                       , pattern CyrillicLetterBigYus                          , pattern CyrillicLetterIotifiedBigYus
  ,  pattern KatakanaHiraganaVoicedSoundMark               , pattern KatakanaHiraganaSemiVoicedSoundMark           , pattern CyrillicVzmet                                 , pattern CyrillicLetterUkrainianIe
  ,  pattern CyrillicLetterI                               , pattern CyrillicLetterYi                              , pattern CyrillicLetterU                               , pattern CyrillicLetterHardSign
  ,  pattern CyrillicLetterYeru                            , pattern CyrillicLetterSoftSign                        , pattern CyrillicLetterOmega                           , pattern CyrillicKavyka
  ,  pattern CyrillicPayerok                               , pattern CyrillicLetterEf                              , pattern CyrillicLetterIotifiedE                       , pattern BamumMarkKoqndon
  ,  pattern BamumMarkTukwentis                            , pattern DevanagariDigitZero                           , pattern DevanagariDigitOne                            , pattern DevanagariDigitTwo
  ,  pattern DevanagariDigitThree                          , pattern DevanagariDigitFour                           , pattern DevanagariDigitFive                           , pattern DevanagariDigitSix
  ,  pattern DevanagariDigitSeven                          , pattern DevanagariDigitEight                          , pattern DevanagariDigitNine                           , pattern DevanagariLetterA
  ,  pattern DevanagariLetterU                             , pattern DevanagariLetterKa                            , pattern DevanagariLetterNa                            , pattern DevanagariLetterPa
  ,  pattern DevanagariLetterRa                            , pattern DevanagariLetterVi                            , pattern DevanagariSignAvagraha                        , pattern LigatureLeftHalf
  ,  pattern LigatureRightHalf                             , pattern DoubleTildeLeftHalf                           , pattern DoubleTildeRightHalf                          , pattern MacronLeftHalf
  ,  pattern MacronRightHalf                               , pattern ConjoiningMacron                              , pattern LigatureLeftHalfBelow                         , pattern LigatureRightHalfBelow
  ,  pattern TildeLeftHalfBelow                            , pattern TildeRightHalfBelow                           , pattern MacronLeftHalfBelow                           , pattern MacronRightHalfBelow
  ,  pattern ConjoiningMacronBelow                         , pattern CyrillicTitloLeftHalf                         , pattern CyrillicTitloRightHalf                        , pattern PhaistosDiscSignObliqueStroke
  ,  pattern OldPermicLetterAn                             , pattern OldPermicLetterDoi                            , pattern OldPermicLetterZata                           , pattern OldPermicLetterNenoe
  ,  pattern OldPermicLetterSii                            , pattern GranthaDigitZero                              , pattern GranthaDigitOne                               , pattern GranthaDigitTwo
  ,  pattern GranthaDigitThree                             , pattern GranthaDigitFour                              , pattern GranthaDigitFive                              , pattern GranthaDigitSix
  ,  pattern GranthaLetterA                                , pattern GranthaLetterKa                               , pattern GranthaLetterNa                               , pattern GranthaLetterVi
  ,  pattern GranthaLetterPa                               , pattern BassaVahHighTone                              , pattern BassaVahLowTone                               , pattern BassaVahMidTone
  ,  pattern BassaVahLowMidTone                            , pattern BassaVahHighLowTone                           , pattern MusicalSymbolStem                             , pattern MusicalSymbolSprechgesangStem
  ,  pattern MusicalSymbolTremolo1                         , pattern MusicalSymbolTremolo2                         , pattern MusicalSymbolTremolo3                         , pattern MusicalSymbolAugmentationDot
  ,  pattern MusicalSymbolFlag1                            , pattern MusicalSymbolFlag2                            , pattern MusicalSymbolFlag3                            , pattern MusicalSymbolFlag4
  ,  pattern MusicalSymbolFlag5                            , pattern MusicalSymbolAccent                           , pattern MusicalSymbolStaccato                         , pattern MusicalSymbolTenuto
  ,  pattern MusicalSymbolStaccatissimo                    , pattern MusicalSymbolMarcato                          , pattern MusicalSymbolMarcatoStaccato                  , pattern MusicalSymbolAccentStaccato
  ,  pattern MusicalSymbolLoure                            , pattern MusicalSymbolDoit                             , pattern MusicalSymbolRip                              , pattern MusicalSymbolFlip
  ,  pattern MusicalSymbolSmear                            , pattern MusicalSymbolBend                             , pattern MusicalSymbolDoubleTongue                     , pattern MusicalSymbolTripleTongue
  ,  pattern MusicalSymbolDownBow                          , pattern MusicalSymbolUpBow                            , pattern MusicalSymbolHarmonic                         , pattern MusicalSymbolSnapPizzicato
  ,  pattern GreekMusicalTriseme                           , pattern GreekMusicalTetraseme                         , pattern GreekMusicalPentaseme                         , pattern GlagoliticLetterAzu
  ,  pattern GlagoliticLetterBuky                          , pattern GlagoliticLetterVede                          , pattern GlagoliticLetterGlagoli                       , pattern GlagoliticLetterDobro
  ,  pattern GlagoliticLetterYestu                         , pattern GlagoliticLetterZhivete                       , pattern GlagoliticLetterZemlja                        , pattern GlagoliticLetterIzhe
  ,  pattern GlagoliticLetterInitialIzhe                   , pattern GlagoliticLetterI                             , pattern GlagoliticLetterDjervi                        , pattern GlagoliticLetterKako
  ,  pattern GlagoliticLetterLjudije                       , pattern GlagoliticLetterMyslite                       , pattern GlagoliticLetterNashi                         , pattern GlagoliticLetterOnu
  ,  pattern GlagoliticLetterPokoji                        , pattern GlagoliticLetterRitsi                         , pattern GlagoliticLetterSlovo                         , pattern GlagoliticLetterTvrido
  ,  pattern GlagoliticLetterUku                           , pattern GlagoliticLetterFritu                         , pattern GlagoliticLetterHeru                          , pattern GlagoliticLetterShta
  ,  pattern GlagoliticLetterTsi                           , pattern GlagoliticLetterChrivi                        , pattern GlagoliticLetterSha                           , pattern GlagoliticLetterYeru
  ,  pattern GlagoliticLetterYeri                          , pattern GlagoliticLetterYati                          , pattern GlagoliticLetterYu                            , pattern GlagoliticLetterSmallYus
  ,  pattern GlagoliticLetterYo                            , pattern GlagoliticLetterIotatedSmallYus               , pattern GlagoliticLetterBigYus                        , pattern GlagoliticLetterIotatedBigYus
  ,  pattern GlagoliticLetterFita                          , pattern MendeKikakuiNumberTeens                       , pattern MendeKikakuiNumberTens                        , pattern MendeKikakuiNumberHundreds
  ,  pattern MendeKikakuiNumberThousands                   , pattern MendeKikakuiNumberTenThousands                , pattern MendeKikakuiNumberHundredThousands            , pattern MendeKikakuiNumberMillions
  ) where

import Data.List.NonEmpty(NonEmpty((:|)), (<|), toList)
import Data.String(IsString(fromString))
import Data.Text(Text, cons, pack, singleton)
import qualified Data.Text as T

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

infixr 8 *^
infixr 8 *^!

-- | The list of possible combining characters. In the documentation of the
-- combining characters, the characters are demonstrated on the /bullet/ symbol (&#x2022;).
data CombiningCharacter
  = CombiningGraveAccent  -- ^ The combining character @COMBINING GRAVE ACCENT@ from the Unicode standard, defined by @'\\x0300'@ (&#x2022;&#x0300;).
  | CombiningAcuteAccent  -- ^ The combining character @COMBINING ACUTE ACCENT@ from the Unicode standard, defined by @'\\x0301'@ (&#x2022;&#x0301;).
  | CombiningCircumflexAccent  -- ^ The combining character @COMBINING CIRCUMFLEX ACCENT@ from the Unicode standard, defined by @'\\x0302'@ (&#x2022;&#x0302;).
  | CombiningTilde  -- ^ The combining character @COMBINING TILDE@ from the Unicode standard, defined by @'\\x0303'@ (&#x2022;&#x0303;).
  | CombiningMacron  -- ^ The combining character @COMBINING MACRON@ from the Unicode standard, defined by @'\\x0304'@ (&#x2022;&#x0304;).
  | CombiningOverline  -- ^ The combining character @COMBINING OVERLINE@ from the Unicode standard, defined by @'\\x0305'@ (&#x2022;&#x0305;).
  | CombiningBreve  -- ^ The combining character @COMBINING BREVE@ from the Unicode standard, defined by @'\\x0306'@ (&#x2022;&#x0306;).
  | CombiningDotAbove  -- ^ The combining character @COMBINING DOT ABOVE@ from the Unicode standard, defined by @'\\x0307'@ (&#x2022;&#x0307;).
  | CombiningDiaeresis  -- ^ The combining character @COMBINING DIAERESIS@ from the Unicode standard, defined by @'\\x0308'@ (&#x2022;&#x0308;).
  | CombiningHookAbove  -- ^ The combining character @COMBINING HOOK ABOVE@ from the Unicode standard, defined by @'\\x0309'@ (&#x2022;&#x0309;).
  | CombiningRingAbove  -- ^ The combining character @COMBINING RING ABOVE@ from the Unicode standard, defined by @'\\x030a'@ (&#x2022;&#x030a;).
  | CombiningDoubleAcuteAccent  -- ^ The combining character @COMBINING DOUBLE ACUTE ACCENT@ from the Unicode standard, defined by @'\\x030b'@ (&#x2022;&#x030b;).
  | CombiningCaron  -- ^ The combining character @COMBINING CARON@ from the Unicode standard, defined by @'\\x030c'@ (&#x2022;&#x030c;).
  | CombiningVerticalLineAbove  -- ^ The combining character @COMBINING VERTICAL LINE ABOVE@ from the Unicode standard, defined by @'\\x030d'@ (&#x2022;&#x030d;).
  | CombiningDoubleVerticalLineAbove  -- ^ The combining character @COMBINING DOUBLE VERTICAL LINE ABOVE@ from the Unicode standard, defined by @'\\x030e'@ (&#x2022;&#x030e;).
  | CombiningDoubleGraveAccent  -- ^ The combining character @COMBINING DOUBLE GRAVE ACCENT@ from the Unicode standard, defined by @'\\x030f'@ (&#x2022;&#x030f;).
  | CombiningCandrabindu  -- ^ The combining character @COMBINING CANDRABINDU@ from the Unicode standard, defined by @'\\x0310'@ (&#x2022;&#x0310;).
  | CombiningInvertedBreve  -- ^ The combining character @COMBINING INVERTED BREVE@ from the Unicode standard, defined by @'\\x0311'@ (&#x2022;&#x0311;).
  | CombiningTurnedCommaAbove  -- ^ The combining character @COMBINING TURNED COMMA ABOVE@ from the Unicode standard, defined by @'\\x0312'@ (&#x2022;&#x0312;).
  | CombiningCommaAbove  -- ^ The combining character @COMBINING COMMA ABOVE@ from the Unicode standard, defined by @'\\x0313'@ (&#x2022;&#x0313;).
  | CombiningReversedCommaAbove  -- ^ The combining character @COMBINING REVERSED COMMA ABOVE@ from the Unicode standard, defined by @'\\x0314'@ (&#x2022;&#x0314;).
  | CombiningCommaAboveRight  -- ^ The combining character @COMBINING COMMA ABOVE RIGHT@ from the Unicode standard, defined by @'\\x0315'@ (&#x2022;&#x0315;).
  | CombiningGraveAccentBelow  -- ^ The combining character @COMBINING GRAVE ACCENT BELOW@ from the Unicode standard, defined by @'\\x0316'@ (&#x2022;&#x0316;).
  | CombiningAcuteAccentBelow  -- ^ The combining character @COMBINING ACUTE ACCENT BELOW@ from the Unicode standard, defined by @'\\x0317'@ (&#x2022;&#x0317;).
  | CombiningLeftTackBelow  -- ^ The combining character @COMBINING LEFT TACK BELOW@ from the Unicode standard, defined by @'\\x0318'@ (&#x2022;&#x0318;).
  | CombiningRightTackBelow  -- ^ The combining character @COMBINING RIGHT TACK BELOW@ from the Unicode standard, defined by @'\\x0319'@ (&#x2022;&#x0319;).
  | CombiningLeftAngleAbove  -- ^ The combining character @COMBINING LEFT ANGLE ABOVE@ from the Unicode standard, defined by @'\\x031a'@ (&#x2022;&#x031a;).
  | CombiningHorn  -- ^ The combining character @COMBINING HORN@ from the Unicode standard, defined by @'\\x031b'@ (&#x2022;&#x031b;).
  | CombiningLeftHalfRingBelow  -- ^ The combining character @COMBINING LEFT HALF RING BELOW@ from the Unicode standard, defined by @'\\x031c'@ (&#x2022;&#x031c;).
  | CombiningUpTackBelow  -- ^ The combining character @COMBINING UP TACK BELOW@ from the Unicode standard, defined by @'\\x031d'@ (&#x2022;&#x031d;).
  | CombiningDownTackBelow  -- ^ The combining character @COMBINING DOWN TACK BELOW@ from the Unicode standard, defined by @'\\x031e'@ (&#x2022;&#x031e;).
  | CombiningPlusSignBelow  -- ^ The combining character @COMBINING PLUS SIGN BELOW@ from the Unicode standard, defined by @'\\x031f'@ (&#x2022;&#x031f;).
  | CombiningMinusSignBelow  -- ^ The combining character @COMBINING MINUS SIGN BELOW@ from the Unicode standard, defined by @'\\x0320'@ (&#x2022;&#x0320;).
  | CombiningPalatalizedHookBelow  -- ^ The combining character @COMBINING PALATALIZED HOOK BELOW@ from the Unicode standard, defined by @'\\x0321'@ (&#x2022;&#x0321;).
  | CombiningRetroflexHookBelow  -- ^ The combining character @COMBINING RETROFLEX HOOK BELOW@ from the Unicode standard, defined by @'\\x0322'@ (&#x2022;&#x0322;).
  | CombiningDotBelow  -- ^ The combining character @COMBINING DOT BELOW@ from the Unicode standard, defined by @'\\x0323'@ (&#x2022;&#x0323;).
  | CombiningDiaeresisBelow  -- ^ The combining character @COMBINING DIAERESIS BELOW@ from the Unicode standard, defined by @'\\x0324'@ (&#x2022;&#x0324;).
  | CombiningRingBelow  -- ^ The combining character @COMBINING RING BELOW@ from the Unicode standard, defined by @'\\x0325'@ (&#x2022;&#x0325;).
  | CombiningCommaBelow  -- ^ The combining character @COMBINING COMMA BELOW@ from the Unicode standard, defined by @'\\x0326'@ (&#x2022;&#x0326;).
  | CombiningCedilla  -- ^ The combining character @COMBINING CEDILLA@ from the Unicode standard, defined by @'\\x0327'@ (&#x2022;&#x0327;).
  | CombiningOgonek  -- ^ The combining character @COMBINING OGONEK@ from the Unicode standard, defined by @'\\x0328'@ (&#x2022;&#x0328;).
  | CombiningVerticalLineBelow  -- ^ The combining character @COMBINING VERTICAL LINE BELOW@ from the Unicode standard, defined by @'\\x0329'@ (&#x2022;&#x0329;).
  | CombiningBridgeBelow  -- ^ The combining character @COMBINING BRIDGE BELOW@ from the Unicode standard, defined by @'\\x032a'@ (&#x2022;&#x032a;).
  | CombiningInvertedDoubleArchBelow  -- ^ The combining character @COMBINING INVERTED DOUBLE ARCH BELOW@ from the Unicode standard, defined by @'\\x032b'@ (&#x2022;&#x032b;).
  | CombiningCaronBelow  -- ^ The combining character @COMBINING CARON BELOW@ from the Unicode standard, defined by @'\\x032c'@ (&#x2022;&#x032c;).
  | CombiningCircumflexAccentBelow  -- ^ The combining character @COMBINING CIRCUMFLEX ACCENT BELOW@ from the Unicode standard, defined by @'\\x032d'@ (&#x2022;&#x032d;).
  | CombiningBreveBelow  -- ^ The combining character @COMBINING BREVE BELOW@ from the Unicode standard, defined by @'\\x032e'@ (&#x2022;&#x032e;).
  | CombiningInvertedBreveBelow  -- ^ The combining character @COMBINING INVERTED BREVE BELOW@ from the Unicode standard, defined by @'\\x032f'@ (&#x2022;&#x032f;).
  | CombiningTildeBelow  -- ^ The combining character @COMBINING TILDE BELOW@ from the Unicode standard, defined by @'\\x0330'@ (&#x2022;&#x0330;).
  | CombiningMacronBelow  -- ^ The combining character @COMBINING MACRON BELOW@ from the Unicode standard, defined by @'\\x0331'@ (&#x2022;&#x0331;).
  | CombiningLowLine  -- ^ The combining character @COMBINING LOW LINE@ from the Unicode standard, defined by @'\\x0332'@ (&#x2022;&#x0332;).
  | CombiningDoubleLowLine  -- ^ The combining character @COMBINING DOUBLE LOW LINE@ from the Unicode standard, defined by @'\\x0333'@ (&#x2022;&#x0333;).
  | CombiningTildeOverlay  -- ^ The combining character @COMBINING TILDE OVERLAY@ from the Unicode standard, defined by @'\\x0334'@ (&#x2022;&#x0334;).
  | CombiningShortStrokeOverlay  -- ^ The combining character @COMBINING SHORT STROKE OVERLAY@ from the Unicode standard, defined by @'\\x0335'@ (&#x2022;&#x0335;).
  | CombiningLongStrokeOverlay  -- ^ The combining character @COMBINING LONG STROKE OVERLAY@ from the Unicode standard, defined by @'\\x0336'@ (&#x2022;&#x0336;).
  | CombiningShortSolidusOverlay  -- ^ The combining character @COMBINING SHORT SOLIDUS OVERLAY@ from the Unicode standard, defined by @'\\x0337'@ (&#x2022;&#x0337;).
  | CombiningLongSolidusOverlay  -- ^ The combining character @COMBINING LONG SOLIDUS OVERLAY@ from the Unicode standard, defined by @'\\x0338'@ (&#x2022;&#x0338;).
  | CombiningRightHalfRingBelow  -- ^ The combining character @COMBINING RIGHT HALF RING BELOW@ from the Unicode standard, defined by @'\\x0339'@ (&#x2022;&#x0339;).
  | CombiningInvertedBridgeBelow  -- ^ The combining character @COMBINING INVERTED BRIDGE BELOW@ from the Unicode standard, defined by @'\\x033a'@ (&#x2022;&#x033a;).
  | CombiningSquareBelow  -- ^ The combining character @COMBINING SQUARE BELOW@ from the Unicode standard, defined by @'\\x033b'@ (&#x2022;&#x033b;).
  | CombiningSeagullBelow  -- ^ The combining character @COMBINING SEAGULL BELOW@ from the Unicode standard, defined by @'\\x033c'@ (&#x2022;&#x033c;).
  | CombiningXAbove  -- ^ The combining character @COMBINING X ABOVE@ from the Unicode standard, defined by @'\\x033d'@ (&#x2022;&#x033d;).
  | CombiningVerticalTilde  -- ^ The combining character @COMBINING VERTICAL TILDE@ from the Unicode standard, defined by @'\\x033e'@ (&#x2022;&#x033e;).
  | CombiningDoubleOverline  -- ^ The combining character @COMBINING DOUBLE OVERLINE@ from the Unicode standard, defined by @'\\x033f'@ (&#x2022;&#x033f;).
  | CombiningGraveToneMark  -- ^ The combining character @COMBINING GRAVE TONE MARK@ from the Unicode standard, defined by @'\\x0340'@ (&#x2022;&#x0340;).
  | CombiningAcuteToneMark  -- ^ The combining character @COMBINING ACUTE TONE MARK@ from the Unicode standard, defined by @'\\x0341'@ (&#x2022;&#x0341;).
  | CombiningGreekPerispomeni  -- ^ The combining character @COMBINING GREEK PERISPOMENI@ from the Unicode standard, defined by @'\\x0342'@ (&#x2022;&#x0342;).
  | CombiningGreekKoronis  -- ^ The combining character @COMBINING GREEK KORONIS@ from the Unicode standard, defined by @'\\x0343'@ (&#x2022;&#x0343;).
  | CombiningGreekDialytikaTonos  -- ^ The combining character @COMBINING GREEK DIALYTIKA TONOS@ from the Unicode standard, defined by @'\\x0344'@ (&#x2022;&#x0344;).
  | CombiningGreekYpogegrammeni  -- ^ The combining character @COMBINING GREEK YPOGEGRAMMENI@ from the Unicode standard, defined by @'\\x0345'@ (&#x2022;&#x0345;).
  | CombiningBridgeAbove  -- ^ The combining character @COMBINING BRIDGE ABOVE@ from the Unicode standard, defined by @'\\x0346'@ (&#x2022;&#x0346;).
  | CombiningEqualsSignBelow  -- ^ The combining character @COMBINING EQUALS SIGN BELOW@ from the Unicode standard, defined by @'\\x0347'@ (&#x2022;&#x0347;).
  | CombiningDoubleVerticalLineBelow  -- ^ The combining character @COMBINING DOUBLE VERTICAL LINE BELOW@ from the Unicode standard, defined by @'\\x0348'@ (&#x2022;&#x0348;).
  | CombiningLeftAngleBelow  -- ^ The combining character @COMBINING LEFT ANGLE BELOW@ from the Unicode standard, defined by @'\\x0349'@ (&#x2022;&#x0349;).
  | CombiningNotTildeAbove  -- ^ The combining character @COMBINING NOT TILDE ABOVE@ from the Unicode standard, defined by @'\\x034a'@ (&#x2022;&#x034a;).
  | CombiningHomotheticAbove  -- ^ The combining character @COMBINING HOMOTHETIC ABOVE@ from the Unicode standard, defined by @'\\x034b'@ (&#x2022;&#x034b;).
  | CombiningAlmostEqualToAbove  -- ^ The combining character @COMBINING ALMOST EQUAL TO ABOVE@ from the Unicode standard, defined by @'\\x034c'@ (&#x2022;&#x034c;).
  | CombiningLeftRightArrowBelow  -- ^ The combining character @COMBINING LEFT RIGHT ARROW BELOW@ from the Unicode standard, defined by @'\\x034d'@ (&#x2022;&#x034d;).
  | CombiningUpwardsArrowBelow  -- ^ The combining character @COMBINING UPWARDS ARROW BELOW@ from the Unicode standard, defined by @'\\x034e'@ (&#x2022;&#x034e;).
  | CombiningRightArrowheadAbove  -- ^ The combining character @COMBINING RIGHT ARROWHEAD ABOVE@ from the Unicode standard, defined by @'\\x0350'@ (&#x2022;&#x0350;).
  | CombiningLeftHalfRingAbove  -- ^ The combining character @COMBINING LEFT HALF RING ABOVE@ from the Unicode standard, defined by @'\\x0351'@ (&#x2022;&#x0351;).
  | CombiningFermata  -- ^ The combining character @COMBINING FERMATA@ from the Unicode standard, defined by @'\\x0352'@ (&#x2022;&#x0352;).
  | CombiningXBelow  -- ^ The combining character @COMBINING X BELOW@ from the Unicode standard, defined by @'\\x0353'@ (&#x2022;&#x0353;).
  | CombiningLeftArrowheadBelow  -- ^ The combining character @COMBINING LEFT ARROWHEAD BELOW@ from the Unicode standard, defined by @'\\x0354'@ (&#x2022;&#x0354;).
  | CombiningRightArrowheadBelow  -- ^ The combining character @COMBINING RIGHT ARROWHEAD BELOW@ from the Unicode standard, defined by @'\\x0355'@ (&#x2022;&#x0355;).
  | CombiningRightArrowheadAndUpArrowheadBelow  -- ^ The combining character @COMBINING RIGHT ARROWHEAD AND UP ARROWHEAD BELOW@ from the Unicode standard, defined by @'\\x0356'@ (&#x2022;&#x0356;).
  | CombiningRightHalfRingAbove  -- ^ The combining character @COMBINING RIGHT HALF RING ABOVE@ from the Unicode standard, defined by @'\\x0357'@ (&#x2022;&#x0357;).
  | CombiningDotAboveRight  -- ^ The combining character @COMBINING DOT ABOVE RIGHT@ from the Unicode standard, defined by @'\\x0358'@ (&#x2022;&#x0358;).
  | CombiningAsteriskBelow  -- ^ The combining character @COMBINING ASTERISK BELOW@ from the Unicode standard, defined by @'\\x0359'@ (&#x2022;&#x0359;).
  | CombiningDoubleRingBelow  -- ^ The combining character @COMBINING DOUBLE RING BELOW@ from the Unicode standard, defined by @'\\x035a'@ (&#x2022;&#x035a;).
  | CombiningZigzagAbove  -- ^ The combining character @COMBINING ZIGZAG ABOVE@ from the Unicode standard, defined by @'\\x035b'@ (&#x2022;&#x035b;).
  | CombiningDoubleBreveBelow  -- ^ The combining character @COMBINING DOUBLE BREVE BELOW@ from the Unicode standard, defined by @'\\x035c'@ (&#x2022;&#x035c;).
  | CombiningDoubleBreve  -- ^ The combining character @COMBINING DOUBLE BREVE@ from the Unicode standard, defined by @'\\x035d'@ (&#x2022;&#x035d;).
  | CombiningDoubleMacron  -- ^ The combining character @COMBINING DOUBLE MACRON@ from the Unicode standard, defined by @'\\x035e'@ (&#x2022;&#x035e;).
  | CombiningDoubleMacronBelow  -- ^ The combining character @COMBINING DOUBLE MACRON BELOW@ from the Unicode standard, defined by @'\\x035f'@ (&#x2022;&#x035f;).
  | CombiningDoubleTilde  -- ^ The combining character @COMBINING DOUBLE TILDE@ from the Unicode standard, defined by @'\\x0360'@ (&#x2022;&#x0360;).
  | CombiningDoubleInvertedBreve  -- ^ The combining character @COMBINING DOUBLE INVERTED BREVE@ from the Unicode standard, defined by @'\\x0361'@ (&#x2022;&#x0361;).
  | CombiningDoubleRightwardsArrowBelow  -- ^ The combining character @COMBINING DOUBLE RIGHTWARDS ARROW BELOW@ from the Unicode standard, defined by @'\\x0362'@ (&#x2022;&#x0362;).
  | CombiningLatinSmallLetterA  -- ^ The combining character @COMBINING LATIN SMALL LETTER A@ from the Unicode standard, defined by @'\\x0363'@ (&#x2022;&#x0363;).
  | CombiningLatinSmallLetterE  -- ^ The combining character @COMBINING LATIN SMALL LETTER E@ from the Unicode standard, defined by @'\\x0364'@ (&#x2022;&#x0364;).
  | CombiningLatinSmallLetterI  -- ^ The combining character @COMBINING LATIN SMALL LETTER I@ from the Unicode standard, defined by @'\\x0365'@ (&#x2022;&#x0365;).
  | CombiningLatinSmallLetterO  -- ^ The combining character @COMBINING LATIN SMALL LETTER O@ from the Unicode standard, defined by @'\\x0366'@ (&#x2022;&#x0366;).
  | CombiningLatinSmallLetterU  -- ^ The combining character @COMBINING LATIN SMALL LETTER U@ from the Unicode standard, defined by @'\\x0367'@ (&#x2022;&#x0367;).
  | CombiningLatinSmallLetterC  -- ^ The combining character @COMBINING LATIN SMALL LETTER C@ from the Unicode standard, defined by @'\\x0368'@ (&#x2022;&#x0368;).
  | CombiningLatinSmallLetterD  -- ^ The combining character @COMBINING LATIN SMALL LETTER D@ from the Unicode standard, defined by @'\\x0369'@ (&#x2022;&#x0369;).
  | CombiningLatinSmallLetterH  -- ^ The combining character @COMBINING LATIN SMALL LETTER H@ from the Unicode standard, defined by @'\\x036a'@ (&#x2022;&#x036a;).
  | CombiningLatinSmallLetterM  -- ^ The combining character @COMBINING LATIN SMALL LETTER M@ from the Unicode standard, defined by @'\\x036b'@ (&#x2022;&#x036b;).
  | CombiningLatinSmallLetterR  -- ^ The combining character @COMBINING LATIN SMALL LETTER R@ from the Unicode standard, defined by @'\\x036c'@ (&#x2022;&#x036c;).
  | CombiningLatinSmallLetterT  -- ^ The combining character @COMBINING LATIN SMALL LETTER T@ from the Unicode standard, defined by @'\\x036d'@ (&#x2022;&#x036d;).
  | CombiningLatinSmallLetterV  -- ^ The combining character @COMBINING LATIN SMALL LETTER V@ from the Unicode standard, defined by @'\\x036e'@ (&#x2022;&#x036e;).
  | CombiningLatinSmallLetterX  -- ^ The combining character @COMBINING LATIN SMALL LETTER X@ from the Unicode standard, defined by @'\\x036f'@ (&#x2022;&#x036f;).
  | CombiningCyrillicTitlo  -- ^ The combining character @COMBINING CYRILLIC TITLO@ from the Unicode standard, defined by @'\\x0483'@ (&#x2022;&#x0483;).
  | CombiningCyrillicPalatalization  -- ^ The combining character @COMBINING CYRILLIC PALATALIZATION@ from the Unicode standard, defined by @'\\x0484'@ (&#x2022;&#x0484;).
  | CombiningCyrillicDasiaPneumata  -- ^ The combining character @COMBINING CYRILLIC DASIA PNEUMATA@ from the Unicode standard, defined by @'\\x0485'@ (&#x2022;&#x0485;).
  | CombiningCyrillicPsiliPneumata  -- ^ The combining character @COMBINING CYRILLIC PSILI PNEUMATA@ from the Unicode standard, defined by @'\\x0486'@ (&#x2022;&#x0486;).
  | CombiningCyrillicPokrytie  -- ^ The combining character @COMBINING CYRILLIC POKRYTIE@ from the Unicode standard, defined by @'\\x0487'@ (&#x2022;&#x0487;).
  | HebrewAccentEtnahta  -- ^ The combining character @HEBREW ACCENT ETNAHTA@ from the Unicode standard, defined by @'\\x0591'@ (&#x2022;&#x0591;).
  | HebrewAccentSegol  -- ^ The combining character @HEBREW ACCENT SEGOL@ from the Unicode standard, defined by @'\\x0592'@ (&#x2022;&#x0592;).
  | HebrewAccentShalshelet  -- ^ The combining character @HEBREW ACCENT SHALSHELET@ from the Unicode standard, defined by @'\\x0593'@ (&#x2022;&#x0593;).
  | HebrewAccentZaqefQatan  -- ^ The combining character @HEBREW ACCENT ZAQEF QATAN@ from the Unicode standard, defined by @'\\x0594'@ (&#x2022;&#x0594;).
  | HebrewAccentZaqefGadol  -- ^ The combining character @HEBREW ACCENT ZAQEF GADOL@ from the Unicode standard, defined by @'\\x0595'@ (&#x2022;&#x0595;).
  | HebrewAccentTipeha  -- ^ The combining character @HEBREW ACCENT TIPEHA@ from the Unicode standard, defined by @'\\x0596'@ (&#x2022;&#x0596;).
  | HebrewAccentRevia  -- ^ The combining character @HEBREW ACCENT REVIA@ from the Unicode standard, defined by @'\\x0597'@ (&#x2022;&#x0597;).
  | HebrewAccentZarqa  -- ^ The combining character @HEBREW ACCENT ZARQA@ from the Unicode standard, defined by @'\\x0598'@ (&#x2022;&#x0598;).
  | HebrewAccentPashta  -- ^ The combining character @HEBREW ACCENT PASHTA@ from the Unicode standard, defined by @'\\x0599'@ (&#x2022;&#x0599;).
  | HebrewAccentYetiv  -- ^ The combining character @HEBREW ACCENT YETIV@ from the Unicode standard, defined by @'\\x059a'@ (&#x2022;&#x059a;).
  | HebrewAccentTevir  -- ^ The combining character @HEBREW ACCENT TEVIR@ from the Unicode standard, defined by @'\\x059b'@ (&#x2022;&#x059b;).
  | HebrewAccentGeresh  -- ^ The combining character @HEBREW ACCENT GERESH@ from the Unicode standard, defined by @'\\x059c'@ (&#x2022;&#x059c;).
  | HebrewAccentGereshMuqdam  -- ^ The combining character @HEBREW ACCENT GERESH MUQDAM@ from the Unicode standard, defined by @'\\x059d'@ (&#x2022;&#x059d;).
  | HebrewAccentGershayim  -- ^ The combining character @HEBREW ACCENT GERSHAYIM@ from the Unicode standard, defined by @'\\x059e'@ (&#x2022;&#x059e;).
  | HebrewAccentQarneyPara  -- ^ The combining character @HEBREW ACCENT QARNEY PARA@ from the Unicode standard, defined by @'\\x059f'@ (&#x2022;&#x059f;).
  | HebrewAccentTelishaGedola  -- ^ The combining character @HEBREW ACCENT TELISHA GEDOLA@ from the Unicode standard, defined by @'\\x05a0'@ (&#x2022;&#x05a0;).
  | HebrewAccentPazer  -- ^ The combining character @HEBREW ACCENT PAZER@ from the Unicode standard, defined by @'\\x05a1'@ (&#x2022;&#x05a1;).
  | HebrewAccentAtnahHafukh  -- ^ The combining character @HEBREW ACCENT ATNAH HAFUKH@ from the Unicode standard, defined by @'\\x05a2'@ (&#x2022;&#x05a2;).
  | HebrewAccentMunah  -- ^ The combining character @HEBREW ACCENT MUNAH@ from the Unicode standard, defined by @'\\x05a3'@ (&#x2022;&#x05a3;).
  | HebrewAccentMahapakh  -- ^ The combining character @HEBREW ACCENT MAHAPAKH@ from the Unicode standard, defined by @'\\x05a4'@ (&#x2022;&#x05a4;).
  | HebrewAccentMerkha  -- ^ The combining character @HEBREW ACCENT MERKHA@ from the Unicode standard, defined by @'\\x05a5'@ (&#x2022;&#x05a5;).
  | HebrewAccentMerkhaKefula  -- ^ The combining character @HEBREW ACCENT MERKHA KEFULA@ from the Unicode standard, defined by @'\\x05a6'@ (&#x2022;&#x05a6;).
  | HebrewAccentDarga  -- ^ The combining character @HEBREW ACCENT DARGA@ from the Unicode standard, defined by @'\\x05a7'@ (&#x2022;&#x05a7;).
  | HebrewAccentQadma  -- ^ The combining character @HEBREW ACCENT QADMA@ from the Unicode standard, defined by @'\\x05a8'@ (&#x2022;&#x05a8;).
  | HebrewAccentTelishaQetana  -- ^ The combining character @HEBREW ACCENT TELISHA QETANA@ from the Unicode standard, defined by @'\\x05a9'@ (&#x2022;&#x05a9;).
  | HebrewAccentYerahBenYomo  -- ^ The combining character @HEBREW ACCENT YERAH BEN YOMO@ from the Unicode standard, defined by @'\\x05aa'@ (&#x2022;&#x05aa;).
  | HebrewAccentOle  -- ^ The combining character @HEBREW ACCENT OLE@ from the Unicode standard, defined by @'\\x05ab'@ (&#x2022;&#x05ab;).
  | HebrewAccentIluy  -- ^ The combining character @HEBREW ACCENT ILUY@ from the Unicode standard, defined by @'\\x05ac'@ (&#x2022;&#x05ac;).
  | HebrewAccentDehi  -- ^ The combining character @HEBREW ACCENT DEHI@ from the Unicode standard, defined by @'\\x05ad'@ (&#x2022;&#x05ad;).
  | HebrewAccentZinor  -- ^ The combining character @HEBREW ACCENT ZINOR@ from the Unicode standard, defined by @'\\x05ae'@ (&#x2022;&#x05ae;).
  | HebrewMarkMasoraCircle  -- ^ The combining character @HEBREW MARK MASORA CIRCLE@ from the Unicode standard, defined by @'\\x05af'@ (&#x2022;&#x05af;).
  | HebrewPointSheva  -- ^ The combining character @HEBREW POINT SHEVA@ from the Unicode standard, defined by @'\\x05b0'@ (&#x2022;&#x05b0;).
  | HebrewPointHatafSegol  -- ^ The combining character @HEBREW POINT HATAF SEGOL@ from the Unicode standard, defined by @'\\x05b1'@ (&#x2022;&#x05b1;).
  | HebrewPointHatafPatah  -- ^ The combining character @HEBREW POINT HATAF PATAH@ from the Unicode standard, defined by @'\\x05b2'@ (&#x2022;&#x05b2;).
  | HebrewPointHatafQamats  -- ^ The combining character @HEBREW POINT HATAF QAMATS@ from the Unicode standard, defined by @'\\x05b3'@ (&#x2022;&#x05b3;).
  | HebrewPointHiriq  -- ^ The combining character @HEBREW POINT HIRIQ@ from the Unicode standard, defined by @'\\x05b4'@ (&#x2022;&#x05b4;).
  | HebrewPointTsere  -- ^ The combining character @HEBREW POINT TSERE@ from the Unicode standard, defined by @'\\x05b5'@ (&#x2022;&#x05b5;).
  | HebrewPointSegol  -- ^ The combining character @HEBREW POINT SEGOL@ from the Unicode standard, defined by @'\\x05b6'@ (&#x2022;&#x05b6;).
  | HebrewPointPatah  -- ^ The combining character @HEBREW POINT PATAH@ from the Unicode standard, defined by @'\\x05b7'@ (&#x2022;&#x05b7;).
  | HebrewPointQamats  -- ^ The combining character @HEBREW POINT QAMATS@ from the Unicode standard, defined by @'\\x05b8'@ (&#x2022;&#x05b8;).
  | HebrewPointHolam  -- ^ The combining character @HEBREW POINT HOLAM@ from the Unicode standard, defined by @'\\x05b9'@ (&#x2022;&#x05b9;).
  | HebrewPointHolamHaserForVav  -- ^ The combining character @HEBREW POINT HOLAM HASER FOR VAV@ from the Unicode standard, defined by @'\\x05ba'@ (&#x2022;&#x05ba;).
  | HebrewPointQubuts  -- ^ The combining character @HEBREW POINT QUBUTS@ from the Unicode standard, defined by @'\\x05bb'@ (&#x2022;&#x05bb;).
  | HebrewPointDageshOrMapiq  -- ^ The combining character @HEBREW POINT DAGESH OR MAPIQ@ from the Unicode standard, defined by @'\\x05bc'@ (&#x2022;&#x05bc;).
  | HebrewPointMeteg  -- ^ The combining character @HEBREW POINT METEG@ from the Unicode standard, defined by @'\\x05bd'@ (&#x2022;&#x05bd;).
  | HebrewPointRafe  -- ^ The combining character @HEBREW POINT RAFE@ from the Unicode standard, defined by @'\\x05bf'@ (&#x2022;&#x05bf;).
  | HebrewPointShinDot  -- ^ The combining character @HEBREW POINT SHIN DOT@ from the Unicode standard, defined by @'\\x05c1'@ (&#x2022;&#x05c1;).
  | HebrewPointSinDot  -- ^ The combining character @HEBREW POINT SIN DOT@ from the Unicode standard, defined by @'\\x05c2'@ (&#x2022;&#x05c2;).
  | HebrewMarkUpperDot  -- ^ The combining character @HEBREW MARK UPPER DOT@ from the Unicode standard, defined by @'\\x05c4'@ (&#x2022;&#x05c4;).
  | HebrewMarkLowerDot  -- ^ The combining character @HEBREW MARK LOWER DOT@ from the Unicode standard, defined by @'\\x05c5'@ (&#x2022;&#x05c5;).
  | HebrewPointQamatsQatan  -- ^ The combining character @HEBREW POINT QAMATS QATAN@ from the Unicode standard, defined by @'\\x05c7'@ (&#x2022;&#x05c7;).
  | ArabicSignSallallahouAlayheWassallam  -- ^ The combining character @ARABIC SIGN SALLALLAHOU ALAYHE WASSALLAM@ from the Unicode standard, defined by @'\\x0610'@ (&#x2022;&#x0610;).
  | ArabicSignAlayheAssallam  -- ^ The combining character @ARABIC SIGN ALAYHE ASSALLAM@ from the Unicode standard, defined by @'\\x0611'@ (&#x2022;&#x0611;).
  | ArabicSignRahmatullahAlayhe  -- ^ The combining character @ARABIC SIGN RAHMATULLAH ALAYHE@ from the Unicode standard, defined by @'\\x0612'@ (&#x2022;&#x0612;).
  | ArabicSignRadiAllahouAnhu  -- ^ The combining character @ARABIC SIGN RADI ALLAHOU ANHU@ from the Unicode standard, defined by @'\\x0613'@ (&#x2022;&#x0613;).
  | ArabicSignTakhallus  -- ^ The combining character @ARABIC SIGN TAKHALLUS@ from the Unicode standard, defined by @'\\x0614'@ (&#x2022;&#x0614;).
  | ArabicSmallHighTah  -- ^ The combining character @ARABIC SMALL HIGH TAH@ from the Unicode standard, defined by @'\\x0615'@ (&#x2022;&#x0615;).
  | ArabicSmallHighLigatureAlefWithLamWithYeh  -- ^ The combining character @ARABIC SMALL HIGH LIGATURE ALEF WITH LAM WITH YEH@ from the Unicode standard, defined by @'\\x0616'@ (&#x2022;&#x0616;).
  | ArabicSmallHighZain  -- ^ The combining character @ARABIC SMALL HIGH ZAIN@ from the Unicode standard, defined by @'\\x0617'@ (&#x2022;&#x0617;).
  | ArabicSmallFatha  -- ^ The combining character @ARABIC SMALL FATHA@ from the Unicode standard, defined by @'\\x0618'@ (&#x2022;&#x0618;).
  | ArabicSmallDamma  -- ^ The combining character @ARABIC SMALL DAMMA@ from the Unicode standard, defined by @'\\x0619'@ (&#x2022;&#x0619;).
  | ArabicSmallKasra  -- ^ The combining character @ARABIC SMALL KASRA@ from the Unicode standard, defined by @'\\x061a'@ (&#x2022;&#x061a;).
  | ArabicFathatan  -- ^ The combining character @ARABIC FATHATAN@ from the Unicode standard, defined by @'\\x064b'@ (&#x2022;&#x064b;).
  | ArabicDammatan  -- ^ The combining character @ARABIC DAMMATAN@ from the Unicode standard, defined by @'\\x064c'@ (&#x2022;&#x064c;).
  | ArabicKasratan  -- ^ The combining character @ARABIC KASRATAN@ from the Unicode standard, defined by @'\\x064d'@ (&#x2022;&#x064d;).
  | ArabicFatha  -- ^ The combining character @ARABIC FATHA@ from the Unicode standard, defined by @'\\x064e'@ (&#x2022;&#x064e;).
  | ArabicDamma  -- ^ The combining character @ARABIC DAMMA@ from the Unicode standard, defined by @'\\x064f'@ (&#x2022;&#x064f;).
  | ArabicKasra  -- ^ The combining character @ARABIC KASRA@ from the Unicode standard, defined by @'\\x0650'@ (&#x2022;&#x0650;).
  | ArabicShadda  -- ^ The combining character @ARABIC SHADDA@ from the Unicode standard, defined by @'\\x0651'@ (&#x2022;&#x0651;).
  | ArabicSukun  -- ^ The combining character @ARABIC SUKUN@ from the Unicode standard, defined by @'\\x0652'@ (&#x2022;&#x0652;).
  | ArabicMaddahAbove  -- ^ The combining character @ARABIC MADDAH ABOVE@ from the Unicode standard, defined by @'\\x0653'@ (&#x2022;&#x0653;).
  | ArabicHamzaAbove  -- ^ The combining character @ARABIC HAMZA ABOVE@ from the Unicode standard, defined by @'\\x0654'@ (&#x2022;&#x0654;).
  | ArabicHamzaBelow  -- ^ The combining character @ARABIC HAMZA BELOW@ from the Unicode standard, defined by @'\\x0655'@ (&#x2022;&#x0655;).
  | ArabicSubscriptAlef  -- ^ The combining character @ARABIC SUBSCRIPT ALEF@ from the Unicode standard, defined by @'\\x0656'@ (&#x2022;&#x0656;).
  | ArabicInvertedDamma  -- ^ The combining character @ARABIC INVERTED DAMMA@ from the Unicode standard, defined by @'\\x0657'@ (&#x2022;&#x0657;).
  | ArabicMarkNoonGhunna  -- ^ The combining character @ARABIC MARK NOON GHUNNA@ from the Unicode standard, defined by @'\\x0658'@ (&#x2022;&#x0658;).
  | ArabicZwarakay  -- ^ The combining character @ARABIC ZWARAKAY@ from the Unicode standard, defined by @'\\x0659'@ (&#x2022;&#x0659;).
  | ArabicVowelSignSmallVAbove  -- ^ The combining character @ARABIC VOWEL SIGN SMALL V ABOVE@ from the Unicode standard, defined by @'\\x065a'@ (&#x2022;&#x065a;).
  | ArabicVowelSignInvertedSmallVAbove  -- ^ The combining character @ARABIC VOWEL SIGN INVERTED SMALL V ABOVE@ from the Unicode standard, defined by @'\\x065b'@ (&#x2022;&#x065b;).
  | ArabicVowelSignDotBelow  -- ^ The combining character @ARABIC VOWEL SIGN DOT BELOW@ from the Unicode standard, defined by @'\\x065c'@ (&#x2022;&#x065c;).
  | ArabicReversedDamma  -- ^ The combining character @ARABIC REVERSED DAMMA@ from the Unicode standard, defined by @'\\x065d'@ (&#x2022;&#x065d;).
  | ArabicFathaWithTwoDots  -- ^ The combining character @ARABIC FATHA WITH TWO DOTS@ from the Unicode standard, defined by @'\\x065e'@ (&#x2022;&#x065e;).
  | ArabicWavyHamzaBelow  -- ^ The combining character @ARABIC WAVY HAMZA BELOW@ from the Unicode standard, defined by @'\\x065f'@ (&#x2022;&#x065f;).
  | ArabicLetterSuperscriptAlef  -- ^ The combining character @ARABIC LETTER SUPERSCRIPT ALEF@ from the Unicode standard, defined by @'\\x0670'@ (&#x2022;&#x0670;).
  | ArabicSmallHighLigatureSadWithLamWithAlefMaksura  -- ^ The combining character @ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA@ from the Unicode standard, defined by @'\\x06d6'@ (&#x2022;&#x06d6;).
  | ArabicSmallHighLigatureQafWithLamWithAlefMaksura  -- ^ The combining character @ARABIC SMALL HIGH LIGATURE QAF WITH LAM WITH ALEF MAKSURA@ from the Unicode standard, defined by @'\\x06d7'@ (&#x2022;&#x06d7;).
  | ArabicSmallHighMeemInitialForm  -- ^ The combining character @ARABIC SMALL HIGH MEEM INITIAL FORM@ from the Unicode standard, defined by @'\\x06d8'@ (&#x2022;&#x06d8;).
  | ArabicSmallHighLamAlef  -- ^ The combining character @ARABIC SMALL HIGH LAM ALEF@ from the Unicode standard, defined by @'\\x06d9'@ (&#x2022;&#x06d9;).
  | ArabicSmallHighJeem  -- ^ The combining character @ARABIC SMALL HIGH JEEM@ from the Unicode standard, defined by @'\\x06da'@ (&#x2022;&#x06da;).
  | ArabicSmallHighThreeDots  -- ^ The combining character @ARABIC SMALL HIGH THREE DOTS@ from the Unicode standard, defined by @'\\x06db'@ (&#x2022;&#x06db;).
  | ArabicSmallHighSeen  -- ^ The combining character @ARABIC SMALL HIGH SEEN@ from the Unicode standard, defined by @'\\x06dc'@ (&#x2022;&#x06dc;).
  | ArabicSmallHighRoundedZero  -- ^ The combining character @ARABIC SMALL HIGH ROUNDED ZERO@ from the Unicode standard, defined by @'\\x06df'@ (&#x2022;&#x06df;).
  | ArabicSmallHighUprightRectangularZero  -- ^ The combining character @ARABIC SMALL HIGH UPRIGHT RECTANGULAR ZERO@ from the Unicode standard, defined by @'\\x06e0'@ (&#x2022;&#x06e0;).
  | ArabicSmallHighDotlessHeadOfKhah  -- ^ The combining character @ARABIC SMALL HIGH DOTLESS HEAD OF KHAH@ from the Unicode standard, defined by @'\\x06e1'@ (&#x2022;&#x06e1;).
  | ArabicSmallHighMeemIsolatedForm  -- ^ The combining character @ARABIC SMALL HIGH MEEM ISOLATED FORM@ from the Unicode standard, defined by @'\\x06e2'@ (&#x2022;&#x06e2;).
  | ArabicSmallLowSeen  -- ^ The combining character @ARABIC SMALL LOW SEEN@ from the Unicode standard, defined by @'\\x06e3'@ (&#x2022;&#x06e3;).
  | ArabicSmallHighMadda  -- ^ The combining character @ARABIC SMALL HIGH MADDA@ from the Unicode standard, defined by @'\\x06e4'@ (&#x2022;&#x06e4;).
  | ArabicSmallHighYeh  -- ^ The combining character @ARABIC SMALL HIGH YEH@ from the Unicode standard, defined by @'\\x06e7'@ (&#x2022;&#x06e7;).
  | ArabicSmallHighNoon  -- ^ The combining character @ARABIC SMALL HIGH NOON@ from the Unicode standard, defined by @'\\x06e8'@ (&#x2022;&#x06e8;).
  | ArabicEmptyCentreLowStop  -- ^ The combining character @ARABIC EMPTY CENTRE LOW STOP@ from the Unicode standard, defined by @'\\x06ea'@ (&#x2022;&#x06ea;).
  | ArabicEmptyCentreHighStop  -- ^ The combining character @ARABIC EMPTY CENTRE HIGH STOP@ from the Unicode standard, defined by @'\\x06eb'@ (&#x2022;&#x06eb;).
  | ArabicRoundedHighStopWithFilledCentre  -- ^ The combining character @ARABIC ROUNDED HIGH STOP WITH FILLED CENTRE@ from the Unicode standard, defined by @'\\x06ec'@ (&#x2022;&#x06ec;).
  | ArabicSmallLowMeem  -- ^ The combining character @ARABIC SMALL LOW MEEM@ from the Unicode standard, defined by @'\\x06ed'@ (&#x2022;&#x06ed;).
  | SyriacLetterSuperscriptAlaph  -- ^ The combining character @SYRIAC LETTER SUPERSCRIPT ALAPH@ from the Unicode standard, defined by @'\\x0711'@ (&#x2022;&#x0711;).
  | SyriacPthahaAbove  -- ^ The combining character @SYRIAC PTHAHA ABOVE@ from the Unicode standard, defined by @'\\x0730'@ (&#x2022;&#x0730;).
  | SyriacPthahaBelow  -- ^ The combining character @SYRIAC PTHAHA BELOW@ from the Unicode standard, defined by @'\\x0731'@ (&#x2022;&#x0731;).
  | SyriacPthahaDotted  -- ^ The combining character @SYRIAC PTHAHA DOTTED@ from the Unicode standard, defined by @'\\x0732'@ (&#x2022;&#x0732;).
  | SyriacZqaphaAbove  -- ^ The combining character @SYRIAC ZQAPHA ABOVE@ from the Unicode standard, defined by @'\\x0733'@ (&#x2022;&#x0733;).
  | SyriacZqaphaBelow  -- ^ The combining character @SYRIAC ZQAPHA BELOW@ from the Unicode standard, defined by @'\\x0734'@ (&#x2022;&#x0734;).
  | SyriacZqaphaDotted  -- ^ The combining character @SYRIAC ZQAPHA DOTTED@ from the Unicode standard, defined by @'\\x0735'@ (&#x2022;&#x0735;).
  | SyriacRbasaAbove  -- ^ The combining character @SYRIAC RBASA ABOVE@ from the Unicode standard, defined by @'\\x0736'@ (&#x2022;&#x0736;).
  | SyriacRbasaBelow  -- ^ The combining character @SYRIAC RBASA BELOW@ from the Unicode standard, defined by @'\\x0737'@ (&#x2022;&#x0737;).
  | SyriacDottedZlamaHorizontal  -- ^ The combining character @SYRIAC DOTTED ZLAMA HORIZONTAL@ from the Unicode standard, defined by @'\\x0738'@ (&#x2022;&#x0738;).
  | SyriacDottedZlamaAngular  -- ^ The combining character @SYRIAC DOTTED ZLAMA ANGULAR@ from the Unicode standard, defined by @'\\x0739'@ (&#x2022;&#x0739;).
  | SyriacHbasaAbove  -- ^ The combining character @SYRIAC HBASA ABOVE@ from the Unicode standard, defined by @'\\x073a'@ (&#x2022;&#x073a;).
  | SyriacHbasaBelow  -- ^ The combining character @SYRIAC HBASA BELOW@ from the Unicode standard, defined by @'\\x073b'@ (&#x2022;&#x073b;).
  | SyriacHbasaEsasaDotted  -- ^ The combining character @SYRIAC HBASA-ESASA DOTTED@ from the Unicode standard, defined by @'\\x073c'@ (&#x2022;&#x073c;).
  | SyriacEsasaAbove  -- ^ The combining character @SYRIAC ESASA ABOVE@ from the Unicode standard, defined by @'\\x073d'@ (&#x2022;&#x073d;).
  | SyriacEsasaBelow  -- ^ The combining character @SYRIAC ESASA BELOW@ from the Unicode standard, defined by @'\\x073e'@ (&#x2022;&#x073e;).
  | SyriacRwaha  -- ^ The combining character @SYRIAC RWAHA@ from the Unicode standard, defined by @'\\x073f'@ (&#x2022;&#x073f;).
  | SyriacFeminineDot  -- ^ The combining character @SYRIAC FEMININE DOT@ from the Unicode standard, defined by @'\\x0740'@ (&#x2022;&#x0740;).
  | SyriacQushshaya  -- ^ The combining character @SYRIAC QUSHSHAYA@ from the Unicode standard, defined by @'\\x0741'@ (&#x2022;&#x0741;).
  | SyriacRukkakha  -- ^ The combining character @SYRIAC RUKKAKHA@ from the Unicode standard, defined by @'\\x0742'@ (&#x2022;&#x0742;).
  | SyriacTwoVerticalDotsAbove  -- ^ The combining character @SYRIAC TWO VERTICAL DOTS ABOVE@ from the Unicode standard, defined by @'\\x0743'@ (&#x2022;&#x0743;).
  | SyriacTwoVerticalDotsBelow  -- ^ The combining character @SYRIAC TWO VERTICAL DOTS BELOW@ from the Unicode standard, defined by @'\\x0744'@ (&#x2022;&#x0744;).
  | SyriacThreeDotsAbove  -- ^ The combining character @SYRIAC THREE DOTS ABOVE@ from the Unicode standard, defined by @'\\x0745'@ (&#x2022;&#x0745;).
  | SyriacThreeDotsBelow  -- ^ The combining character @SYRIAC THREE DOTS BELOW@ from the Unicode standard, defined by @'\\x0746'@ (&#x2022;&#x0746;).
  | SyriacObliqueLineAbove  -- ^ The combining character @SYRIAC OBLIQUE LINE ABOVE@ from the Unicode standard, defined by @'\\x0747'@ (&#x2022;&#x0747;).
  | SyriacObliqueLineBelow  -- ^ The combining character @SYRIAC OBLIQUE LINE BELOW@ from the Unicode standard, defined by @'\\x0748'@ (&#x2022;&#x0748;).
  | SyriacMusic  -- ^ The combining character @SYRIAC MUSIC@ from the Unicode standard, defined by @'\\x0749'@ (&#x2022;&#x0749;).
  | SyriacBarrekh  -- ^ The combining character @SYRIAC BARREKH@ from the Unicode standard, defined by @'\\x074a'@ (&#x2022;&#x074a;).
  | NkoCombiningShortHighTone  -- ^ The combining character @NKO COMBINING SHORT HIGH TONE@ from the Unicode standard, defined by @'\\x07eb'@ (&#x2022;&#x07eb;).
  | NkoCombiningShortLowTone  -- ^ The combining character @NKO COMBINING SHORT LOW TONE@ from the Unicode standard, defined by @'\\x07ec'@ (&#x2022;&#x07ec;).
  | NkoCombiningShortRisingTone  -- ^ The combining character @NKO COMBINING SHORT RISING TONE@ from the Unicode standard, defined by @'\\x07ed'@ (&#x2022;&#x07ed;).
  | NkoCombiningLongDescendingTone  -- ^ The combining character @NKO COMBINING LONG DESCENDING TONE@ from the Unicode standard, defined by @'\\x07ee'@ (&#x2022;&#x07ee;).
  | NkoCombiningLongHighTone  -- ^ The combining character @NKO COMBINING LONG HIGH TONE@ from the Unicode standard, defined by @'\\x07ef'@ (&#x2022;&#x07ef;).
  | NkoCombiningLongLowTone  -- ^ The combining character @NKO COMBINING LONG LOW TONE@ from the Unicode standard, defined by @'\\x07f0'@ (&#x2022;&#x07f0;).
  | NkoCombiningLongRisingTone  -- ^ The combining character @NKO COMBINING LONG RISING TONE@ from the Unicode standard, defined by @'\\x07f1'@ (&#x2022;&#x07f1;).
  | NkoCombiningNasalizationMark  -- ^ The combining character @NKO COMBINING NASALIZATION MARK@ from the Unicode standard, defined by @'\\x07f2'@ (&#x2022;&#x07f2;).
  | NkoCombiningDoubleDotAbove  -- ^ The combining character @NKO COMBINING DOUBLE DOT ABOVE@ from the Unicode standard, defined by @'\\x07f3'@ (&#x2022;&#x07f3;).
  | SamaritanMarkIn  -- ^ The combining character @SAMARITAN MARK IN@ from the Unicode standard, defined by @'\\x0816'@ (&#x2022;&#x0816;).
  | SamaritanMarkInAlaf  -- ^ The combining character @SAMARITAN MARK IN-ALAF@ from the Unicode standard, defined by @'\\x0817'@ (&#x2022;&#x0817;).
  | SamaritanMarkOcclusion  -- ^ The combining character @SAMARITAN MARK OCCLUSION@ from the Unicode standard, defined by @'\\x0818'@ (&#x2022;&#x0818;).
  | SamaritanMarkDagesh  -- ^ The combining character @SAMARITAN MARK DAGESH@ from the Unicode standard, defined by @'\\x0819'@ (&#x2022;&#x0819;).
  | SamaritanMarkEpentheticYut  -- ^ The combining character @SAMARITAN MARK EPENTHETIC YUT@ from the Unicode standard, defined by @'\\x081b'@ (&#x2022;&#x081b;).
  | SamaritanVowelSignLongE  -- ^ The combining character @SAMARITAN VOWEL SIGN LONG E@ from the Unicode standard, defined by @'\\x081c'@ (&#x2022;&#x081c;).
  | SamaritanVowelSignE  -- ^ The combining character @SAMARITAN VOWEL SIGN E@ from the Unicode standard, defined by @'\\x081d'@ (&#x2022;&#x081d;).
  | SamaritanVowelSignOverlongAa  -- ^ The combining character @SAMARITAN VOWEL SIGN OVERLONG AA@ from the Unicode standard, defined by @'\\x081e'@ (&#x2022;&#x081e;).
  | SamaritanVowelSignLongAa  -- ^ The combining character @SAMARITAN VOWEL SIGN LONG AA@ from the Unicode standard, defined by @'\\x081f'@ (&#x2022;&#x081f;).
  | SamaritanVowelSignAa  -- ^ The combining character @SAMARITAN VOWEL SIGN AA@ from the Unicode standard, defined by @'\\x0820'@ (&#x2022;&#x0820;).
  | SamaritanVowelSignOverlongA  -- ^ The combining character @SAMARITAN VOWEL SIGN OVERLONG A@ from the Unicode standard, defined by @'\\x0821'@ (&#x2022;&#x0821;).
  | SamaritanVowelSignLongA  -- ^ The combining character @SAMARITAN VOWEL SIGN LONG A@ from the Unicode standard, defined by @'\\x0822'@ (&#x2022;&#x0822;).
  | SamaritanVowelSignA  -- ^ The combining character @SAMARITAN VOWEL SIGN A@ from the Unicode standard, defined by @'\\x0823'@ (&#x2022;&#x0823;).
  | SamaritanVowelSignShortA  -- ^ The combining character @SAMARITAN VOWEL SIGN SHORT A@ from the Unicode standard, defined by @'\\x0825'@ (&#x2022;&#x0825;).
  | SamaritanVowelSignLongU  -- ^ The combining character @SAMARITAN VOWEL SIGN LONG U@ from the Unicode standard, defined by @'\\x0826'@ (&#x2022;&#x0826;).
  | SamaritanVowelSignU  -- ^ The combining character @SAMARITAN VOWEL SIGN U@ from the Unicode standard, defined by @'\\x0827'@ (&#x2022;&#x0827;).
  | SamaritanVowelSignLongI  -- ^ The combining character @SAMARITAN VOWEL SIGN LONG I@ from the Unicode standard, defined by @'\\x0829'@ (&#x2022;&#x0829;).
  | SamaritanVowelSignI  -- ^ The combining character @SAMARITAN VOWEL SIGN I@ from the Unicode standard, defined by @'\\x082a'@ (&#x2022;&#x082a;).
  | SamaritanVowelSignO  -- ^ The combining character @SAMARITAN VOWEL SIGN O@ from the Unicode standard, defined by @'\\x082b'@ (&#x2022;&#x082b;).
  | SamaritanVowelSignSukun  -- ^ The combining character @SAMARITAN VOWEL SIGN SUKUN@ from the Unicode standard, defined by @'\\x082c'@ (&#x2022;&#x082c;).
  | SamaritanMarkNequdaa  -- ^ The combining character @SAMARITAN MARK NEQUDAA@ from the Unicode standard, defined by @'\\x082d'@ (&#x2022;&#x082d;).
  | MandaicAffricationMark  -- ^ The combining character @MANDAIC AFFRICATION MARK@ from the Unicode standard, defined by @'\\x0859'@ (&#x2022;&#x0859;).
  | MandaicVocalizationMark  -- ^ The combining character @MANDAIC VOCALIZATION MARK@ from the Unicode standard, defined by @'\\x085a'@ (&#x2022;&#x085a;).
  | MandaicGeminationMark  -- ^ The combining character @MANDAIC GEMINATION MARK@ from the Unicode standard, defined by @'\\x085b'@ (&#x2022;&#x085b;).
  | ArabicSmallHighWordArRub  -- ^ The combining character @ARABIC SMALL HIGH WORD AR-RUB@ from the Unicode standard, defined by @'\\x08d4'@ (&#x2022;&#x08d4;).
  | ArabicSmallHighSad  -- ^ The combining character @ARABIC SMALL HIGH SAD@ from the Unicode standard, defined by @'\\x08d5'@ (&#x2022;&#x08d5;).
  | ArabicSmallHighAin  -- ^ The combining character @ARABIC SMALL HIGH AIN@ from the Unicode standard, defined by @'\\x08d6'@ (&#x2022;&#x08d6;).
  | ArabicSmallHighQaf  -- ^ The combining character @ARABIC SMALL HIGH QAF@ from the Unicode standard, defined by @'\\x08d7'@ (&#x2022;&#x08d7;).
  | ArabicSmallHighNoonWithKasra  -- ^ The combining character @ARABIC SMALL HIGH NOON WITH KASRA@ from the Unicode standard, defined by @'\\x08d8'@ (&#x2022;&#x08d8;).
  | ArabicSmallLowNoonWithKasra  -- ^ The combining character @ARABIC SMALL LOW NOON WITH KASRA@ from the Unicode standard, defined by @'\\x08d9'@ (&#x2022;&#x08d9;).
  | ArabicSmallHighWordAthThalatha  -- ^ The combining character @ARABIC SMALL HIGH WORD ATH-THALATHA@ from the Unicode standard, defined by @'\\x08da'@ (&#x2022;&#x08da;).
  | ArabicSmallHighWordAsSajda  -- ^ The combining character @ARABIC SMALL HIGH WORD AS-SAJDA@ from the Unicode standard, defined by @'\\x08db'@ (&#x2022;&#x08db;).
  | ArabicSmallHighWordAnNisf  -- ^ The combining character @ARABIC SMALL HIGH WORD AN-NISF@ from the Unicode standard, defined by @'\\x08dc'@ (&#x2022;&#x08dc;).
  | ArabicSmallHighWordSakta  -- ^ The combining character @ARABIC SMALL HIGH WORD SAKTA@ from the Unicode standard, defined by @'\\x08dd'@ (&#x2022;&#x08dd;).
  | ArabicSmallHighWordQif  -- ^ The combining character @ARABIC SMALL HIGH WORD QIF@ from the Unicode standard, defined by @'\\x08de'@ (&#x2022;&#x08de;).
  | ArabicSmallHighWordWaqfa  -- ^ The combining character @ARABIC SMALL HIGH WORD WAQFA@ from the Unicode standard, defined by @'\\x08df'@ (&#x2022;&#x08df;).
  | ArabicSmallHighFootnoteMarker  -- ^ The combining character @ARABIC SMALL HIGH FOOTNOTE MARKER@ from the Unicode standard, defined by @'\\x08e0'@ (&#x2022;&#x08e0;).
  | ArabicSmallHighSignSafha  -- ^ The combining character @ARABIC SMALL HIGH SIGN SAFHA@ from the Unicode standard, defined by @'\\x08e1'@ (&#x2022;&#x08e1;).
  | ArabicTurnedDammaBelow  -- ^ The combining character @ARABIC TURNED DAMMA BELOW@ from the Unicode standard, defined by @'\\x08e3'@ (&#x2022;&#x08e3;).
  | ArabicCurlyFatha  -- ^ The combining character @ARABIC CURLY FATHA@ from the Unicode standard, defined by @'\\x08e4'@ (&#x2022;&#x08e4;).
  | ArabicCurlyDamma  -- ^ The combining character @ARABIC CURLY DAMMA@ from the Unicode standard, defined by @'\\x08e5'@ (&#x2022;&#x08e5;).
  | ArabicCurlyKasra  -- ^ The combining character @ARABIC CURLY KASRA@ from the Unicode standard, defined by @'\\x08e6'@ (&#x2022;&#x08e6;).
  | ArabicCurlyFathatan  -- ^ The combining character @ARABIC CURLY FATHATAN@ from the Unicode standard, defined by @'\\x08e7'@ (&#x2022;&#x08e7;).
  | ArabicCurlyDammatan  -- ^ The combining character @ARABIC CURLY DAMMATAN@ from the Unicode standard, defined by @'\\x08e8'@ (&#x2022;&#x08e8;).
  | ArabicCurlyKasratan  -- ^ The combining character @ARABIC CURLY KASRATAN@ from the Unicode standard, defined by @'\\x08e9'@ (&#x2022;&#x08e9;).
  | ArabicToneOneDotAbove  -- ^ The combining character @ARABIC TONE ONE DOT ABOVE@ from the Unicode standard, defined by @'\\x08ea'@ (&#x2022;&#x08ea;).
  | ArabicToneTwoDotsAbove  -- ^ The combining character @ARABIC TONE TWO DOTS ABOVE@ from the Unicode standard, defined by @'\\x08eb'@ (&#x2022;&#x08eb;).
  | ArabicToneLoopAbove  -- ^ The combining character @ARABIC TONE LOOP ABOVE@ from the Unicode standard, defined by @'\\x08ec'@ (&#x2022;&#x08ec;).
  | ArabicToneOneDotBelow  -- ^ The combining character @ARABIC TONE ONE DOT BELOW@ from the Unicode standard, defined by @'\\x08ed'@ (&#x2022;&#x08ed;).
  | ArabicToneTwoDotsBelow  -- ^ The combining character @ARABIC TONE TWO DOTS BELOW@ from the Unicode standard, defined by @'\\x08ee'@ (&#x2022;&#x08ee;).
  | ArabicToneLoopBelow  -- ^ The combining character @ARABIC TONE LOOP BELOW@ from the Unicode standard, defined by @'\\x08ef'@ (&#x2022;&#x08ef;).
  | ArabicOpenFathatan  -- ^ The combining character @ARABIC OPEN FATHATAN@ from the Unicode standard, defined by @'\\x08f0'@ (&#x2022;&#x08f0;).
  | ArabicOpenDammatan  -- ^ The combining character @ARABIC OPEN DAMMATAN@ from the Unicode standard, defined by @'\\x08f1'@ (&#x2022;&#x08f1;).
  | ArabicOpenKasratan  -- ^ The combining character @ARABIC OPEN KASRATAN@ from the Unicode standard, defined by @'\\x08f2'@ (&#x2022;&#x08f2;).
  | ArabicSmallHighWaw  -- ^ The combining character @ARABIC SMALL HIGH WAW@ from the Unicode standard, defined by @'\\x08f3'@ (&#x2022;&#x08f3;).
  | ArabicFathaWithRing  -- ^ The combining character @ARABIC FATHA WITH RING@ from the Unicode standard, defined by @'\\x08f4'@ (&#x2022;&#x08f4;).
  | ArabicFathaWithDotAbove  -- ^ The combining character @ARABIC FATHA WITH DOT ABOVE@ from the Unicode standard, defined by @'\\x08f5'@ (&#x2022;&#x08f5;).
  | ArabicKasraWithDotBelow  -- ^ The combining character @ARABIC KASRA WITH DOT BELOW@ from the Unicode standard, defined by @'\\x08f6'@ (&#x2022;&#x08f6;).
  | ArabicLeftArrowheadAbove  -- ^ The combining character @ARABIC LEFT ARROWHEAD ABOVE@ from the Unicode standard, defined by @'\\x08f7'@ (&#x2022;&#x08f7;).
  | ArabicRightArrowheadAbove  -- ^ The combining character @ARABIC RIGHT ARROWHEAD ABOVE@ from the Unicode standard, defined by @'\\x08f8'@ (&#x2022;&#x08f8;).
  | ArabicLeftArrowheadBelow  -- ^ The combining character @ARABIC LEFT ARROWHEAD BELOW@ from the Unicode standard, defined by @'\\x08f9'@ (&#x2022;&#x08f9;).
  | ArabicRightArrowheadBelow  -- ^ The combining character @ARABIC RIGHT ARROWHEAD BELOW@ from the Unicode standard, defined by @'\\x08fa'@ (&#x2022;&#x08fa;).
  | ArabicDoubleRightArrowheadAbove  -- ^ The combining character @ARABIC DOUBLE RIGHT ARROWHEAD ABOVE@ from the Unicode standard, defined by @'\\x08fb'@ (&#x2022;&#x08fb;).
  | ArabicDoubleRightArrowheadAboveWithDot  -- ^ The combining character @ARABIC DOUBLE RIGHT ARROWHEAD ABOVE WITH DOT@ from the Unicode standard, defined by @'\\x08fc'@ (&#x2022;&#x08fc;).
  | ArabicRightArrowheadAboveWithDot  -- ^ The combining character @ARABIC RIGHT ARROWHEAD ABOVE WITH DOT@ from the Unicode standard, defined by @'\\x08fd'@ (&#x2022;&#x08fd;).
  | ArabicDammaWithDot  -- ^ The combining character @ARABIC DAMMA WITH DOT@ from the Unicode standard, defined by @'\\x08fe'@ (&#x2022;&#x08fe;).
  | ArabicMarkSidewaysNoonGhunna  -- ^ The combining character @ARABIC MARK SIDEWAYS NOON GHUNNA@ from the Unicode standard, defined by @'\\x08ff'@ (&#x2022;&#x08ff;).
  | DevanagariSignNukta  -- ^ The combining character @DEVANAGARI SIGN NUKTA@ from the Unicode standard, defined by @'\\x093c'@ (&#x2022;&#x093c;).
  | DevanagariSignVirama  -- ^ The combining character @DEVANAGARI SIGN VIRAMA@ from the Unicode standard, defined by @'\\x094d'@ (&#x2022;&#x094d;).
  | DevanagariStressSignUdatta  -- ^ The combining character @DEVANAGARI STRESS SIGN UDATTA@ from the Unicode standard, defined by @'\\x0951'@ (&#x2022;&#x0951;).
  | DevanagariStressSignAnudatta  -- ^ The combining character @DEVANAGARI STRESS SIGN ANUDATTA@ from the Unicode standard, defined by @'\\x0952'@ (&#x2022;&#x0952;).
  | DevanagariGraveAccent  -- ^ The combining character @DEVANAGARI GRAVE ACCENT@ from the Unicode standard, defined by @'\\x0953'@ (&#x2022;&#x0953;).
  | DevanagariAcuteAccent  -- ^ The combining character @DEVANAGARI ACUTE ACCENT@ from the Unicode standard, defined by @'\\x0954'@ (&#x2022;&#x0954;).
  | BengaliSignNukta  -- ^ The combining character @BENGALI SIGN NUKTA@ from the Unicode standard, defined by @'\\x09bc'@ (&#x2022;&#x09bc;).
  | BengaliVowelSignAa  -- ^ The combining character @BENGALI VOWEL SIGN AA@ from the Unicode standard, defined by @'\\x09be'@ (&#x2022;&#x09be;).
  | BengaliSignVirama  -- ^ The combining character @BENGALI SIGN VIRAMA@ from the Unicode standard, defined by @'\\x09cd'@ (&#x2022;&#x09cd;).
  | BengaliAuLengthMark  -- ^ The combining character @BENGALI AU LENGTH MARK@ from the Unicode standard, defined by @'\\x09d7'@ (&#x2022;&#x09d7;).
  | GurmukhiSignNukta  -- ^ The combining character @GURMUKHI SIGN NUKTA@ from the Unicode standard, defined by @'\\x0a3c'@ (&#x2022;&#x0a3c;).
  | GurmukhiSignVirama  -- ^ The combining character @GURMUKHI SIGN VIRAMA@ from the Unicode standard, defined by @'\\x0a4d'@ (&#x2022;&#x0a4d;).
  | GujaratiSignNukta  -- ^ The combining character @GUJARATI SIGN NUKTA@ from the Unicode standard, defined by @'\\x0abc'@ (&#x2022;&#x0abc;).
  | GujaratiSignVirama  -- ^ The combining character @GUJARATI SIGN VIRAMA@ from the Unicode standard, defined by @'\\x0acd'@ (&#x2022;&#x0acd;).
  | OriyaSignNukta  -- ^ The combining character @ORIYA SIGN NUKTA@ from the Unicode standard, defined by @'\\x0b3c'@ (&#x2022;&#x0b3c;).
  | OriyaVowelSignAa  -- ^ The combining character @ORIYA VOWEL SIGN AA@ from the Unicode standard, defined by @'\\x0b3e'@ (&#x2022;&#x0b3e;).
  | OriyaSignVirama  -- ^ The combining character @ORIYA SIGN VIRAMA@ from the Unicode standard, defined by @'\\x0b4d'@ (&#x2022;&#x0b4d;).
  | OriyaAiLengthMark  -- ^ The combining character @ORIYA AI LENGTH MARK@ from the Unicode standard, defined by @'\\x0b56'@ (&#x2022;&#x0b56;).
  | OriyaAuLengthMark  -- ^ The combining character @ORIYA AU LENGTH MARK@ from the Unicode standard, defined by @'\\x0b57'@ (&#x2022;&#x0b57;).
  | TamilVowelSignAa  -- ^ The combining character @TAMIL VOWEL SIGN AA@ from the Unicode standard, defined by @'\\x0bbe'@ (&#x2022;&#x0bbe;).
  | TamilSignVirama  -- ^ The combining character @TAMIL SIGN VIRAMA@ from the Unicode standard, defined by @'\\x0bcd'@ (&#x2022;&#x0bcd;).
  | TamilAuLengthMark  -- ^ The combining character @TAMIL AU LENGTH MARK@ from the Unicode standard, defined by @'\\x0bd7'@ (&#x2022;&#x0bd7;).
  | TeluguSignVirama  -- ^ The combining character @TELUGU SIGN VIRAMA@ from the Unicode standard, defined by @'\\x0c4d'@ (&#x2022;&#x0c4d;).
  | TeluguLengthMark  -- ^ The combining character @TELUGU LENGTH MARK@ from the Unicode standard, defined by @'\\x0c55'@ (&#x2022;&#x0c55;).
  | TeluguAiLengthMark  -- ^ The combining character @TELUGU AI LENGTH MARK@ from the Unicode standard, defined by @'\\x0c56'@ (&#x2022;&#x0c56;).
  | KannadaSignNukta  -- ^ The combining character @KANNADA SIGN NUKTA@ from the Unicode standard, defined by @'\\x0cbc'@ (&#x2022;&#x0cbc;).
  | KannadaVowelSignUu  -- ^ The combining character @KANNADA VOWEL SIGN UU@ from the Unicode standard, defined by @'\\x0cc2'@ (&#x2022;&#x0cc2;).
  | KannadaSignVirama  -- ^ The combining character @KANNADA SIGN VIRAMA@ from the Unicode standard, defined by @'\\x0ccd'@ (&#x2022;&#x0ccd;).
  | KannadaLengthMark  -- ^ The combining character @KANNADA LENGTH MARK@ from the Unicode standard, defined by @'\\x0cd5'@ (&#x2022;&#x0cd5;).
  | KannadaAiLengthMark  -- ^ The combining character @KANNADA AI LENGTH MARK@ from the Unicode standard, defined by @'\\x0cd6'@ (&#x2022;&#x0cd6;).
  | MalayalamVowelSignAa  -- ^ The combining character @MALAYALAM VOWEL SIGN AA@ from the Unicode standard, defined by @'\\x0d3e'@ (&#x2022;&#x0d3e;).
  | MalayalamSignVirama  -- ^ The combining character @MALAYALAM SIGN VIRAMA@ from the Unicode standard, defined by @'\\x0d4d'@ (&#x2022;&#x0d4d;).
  | MalayalamAuLengthMark  -- ^ The combining character @MALAYALAM AU LENGTH MARK@ from the Unicode standard, defined by @'\\x0d57'@ (&#x2022;&#x0d57;).
  | SinhalaSignAlLakuna  -- ^ The combining character @SINHALA SIGN AL-LAKUNA@ from the Unicode standard, defined by @'\\x0dca'@ (&#x2022;&#x0dca;).
  | SinhalaVowelSignAelaPilla  -- ^ The combining character @SINHALA VOWEL SIGN AELA-PILLA@ from the Unicode standard, defined by @'\\x0dcf'@ (&#x2022;&#x0dcf;).
  | SinhalaVowelSignGayanukitta  -- ^ The combining character @SINHALA VOWEL SIGN GAYANUKITTA@ from the Unicode standard, defined by @'\\x0ddf'@ (&#x2022;&#x0ddf;).
  | ThaiCharacterSaraU  -- ^ The combining character @THAI CHARACTER SARA U@ from the Unicode standard, defined by @'\\x0e38'@ (&#x2022;&#x0e38;).
  | ThaiCharacterSaraUu  -- ^ The combining character @THAI CHARACTER SARA UU@ from the Unicode standard, defined by @'\\x0e39'@ (&#x2022;&#x0e39;).
  | ThaiCharacterPhinthu  -- ^ The combining character @THAI CHARACTER PHINTHU@ from the Unicode standard, defined by @'\\x0e3a'@ (&#x2022;&#x0e3a;).
  | ThaiCharacterMaiEk  -- ^ The combining character @THAI CHARACTER MAI EK@ from the Unicode standard, defined by @'\\x0e48'@ (&#x2022;&#x0e48;).
  | ThaiCharacterMaiTho  -- ^ The combining character @THAI CHARACTER MAI THO@ from the Unicode standard, defined by @'\\x0e49'@ (&#x2022;&#x0e49;).
  | ThaiCharacterMaiTri  -- ^ The combining character @THAI CHARACTER MAI TRI@ from the Unicode standard, defined by @'\\x0e4a'@ (&#x2022;&#x0e4a;).
  | ThaiCharacterMaiChattawa  -- ^ The combining character @THAI CHARACTER MAI CHATTAWA@ from the Unicode standard, defined by @'\\x0e4b'@ (&#x2022;&#x0e4b;).
  | LaoVowelSignU  -- ^ The combining character @LAO VOWEL SIGN U@ from the Unicode standard, defined by @'\\x0eb8'@ (&#x2022;&#x0eb8;).
  | LaoVowelSignUu  -- ^ The combining character @LAO VOWEL SIGN UU@ from the Unicode standard, defined by @'\\x0eb9'@ (&#x2022;&#x0eb9;).
  | LaoToneMaiEk  -- ^ The combining character @LAO TONE MAI EK@ from the Unicode standard, defined by @'\\x0ec8'@ (&#x2022;&#x0ec8;).
  | LaoToneMaiTho  -- ^ The combining character @LAO TONE MAI THO@ from the Unicode standard, defined by @'\\x0ec9'@ (&#x2022;&#x0ec9;).
  | LaoToneMaiTi  -- ^ The combining character @LAO TONE MAI TI@ from the Unicode standard, defined by @'\\x0eca'@ (&#x2022;&#x0eca;).
  | LaoToneMaiCatawa  -- ^ The combining character @LAO TONE MAI CATAWA@ from the Unicode standard, defined by @'\\x0ecb'@ (&#x2022;&#x0ecb;).
  | TibetanAstrologicalSignKhyudPa  -- ^ The combining character @TIBETAN ASTROLOGICAL SIGN -KHYUD PA@ from the Unicode standard, defined by @'\\x0f18'@ (&#x2022;&#x0f18;).
  | TibetanAstrologicalSignSdongTshugs  -- ^ The combining character @TIBETAN ASTROLOGICAL SIGN SDONG TSHUGS@ from the Unicode standard, defined by @'\\x0f19'@ (&#x2022;&#x0f19;).
  | TibetanMarkNgasBzungNyiZla  -- ^ The combining character @TIBETAN MARK NGAS BZUNG NYI ZLA@ from the Unicode standard, defined by @'\\x0f35'@ (&#x2022;&#x0f35;).
  | TibetanMarkNgasBzungSgorRtags  -- ^ The combining character @TIBETAN MARK NGAS BZUNG SGOR RTAGS@ from the Unicode standard, defined by @'\\x0f37'@ (&#x2022;&#x0f37;).
  | TibetanMarkTsaPhru  -- ^ The combining character @TIBETAN MARK TSA -PHRU@ from the Unicode standard, defined by @'\\x0f39'@ (&#x2022;&#x0f39;).
  | TibetanVowelSignAa  -- ^ The combining character @TIBETAN VOWEL SIGN AA@ from the Unicode standard, defined by @'\\x0f71'@ (&#x2022;&#x0f71;).
  | TibetanVowelSignI  -- ^ The combining character @TIBETAN VOWEL SIGN I@ from the Unicode standard, defined by @'\\x0f72'@ (&#x2022;&#x0f72;).
  | TibetanVowelSignU  -- ^ The combining character @TIBETAN VOWEL SIGN U@ from the Unicode standard, defined by @'\\x0f74'@ (&#x2022;&#x0f74;).
  | TibetanVowelSignE  -- ^ The combining character @TIBETAN VOWEL SIGN E@ from the Unicode standard, defined by @'\\x0f7a'@ (&#x2022;&#x0f7a;).
  | TibetanVowelSignEe  -- ^ The combining character @TIBETAN VOWEL SIGN EE@ from the Unicode standard, defined by @'\\x0f7b'@ (&#x2022;&#x0f7b;).
  | TibetanVowelSignO  -- ^ The combining character @TIBETAN VOWEL SIGN O@ from the Unicode standard, defined by @'\\x0f7c'@ (&#x2022;&#x0f7c;).
  | TibetanVowelSignOo  -- ^ The combining character @TIBETAN VOWEL SIGN OO@ from the Unicode standard, defined by @'\\x0f7d'@ (&#x2022;&#x0f7d;).
  | TibetanVowelSignReversedI  -- ^ The combining character @TIBETAN VOWEL SIGN REVERSED I@ from the Unicode standard, defined by @'\\x0f80'@ (&#x2022;&#x0f80;).
  | TibetanSignNyiZlaNaaDa  -- ^ The combining character @TIBETAN SIGN NYI ZLA NAA DA@ from the Unicode standard, defined by @'\\x0f82'@ (&#x2022;&#x0f82;).
  | TibetanSignSnaLdan  -- ^ The combining character @TIBETAN SIGN SNA LDAN@ from the Unicode standard, defined by @'\\x0f83'@ (&#x2022;&#x0f83;).
  | TibetanMarkHalanta  -- ^ The combining character @TIBETAN MARK HALANTA@ from the Unicode standard, defined by @'\\x0f84'@ (&#x2022;&#x0f84;).
  | TibetanSignLciRtags  -- ^ The combining character @TIBETAN SIGN LCI RTAGS@ from the Unicode standard, defined by @'\\x0f86'@ (&#x2022;&#x0f86;).
  | TibetanSignYangRtags  -- ^ The combining character @TIBETAN SIGN YANG RTAGS@ from the Unicode standard, defined by @'\\x0f87'@ (&#x2022;&#x0f87;).
  | TibetanSubjoinedLetterSsa  -- ^ The combining character @TIBETAN SUBJOINED LETTER SSA@ from the Unicode standard, defined by @'\\x0fb5'@ (&#x2022;&#x0fb5;).
  | TibetanSubjoinedLetterHa  -- ^ The combining character @TIBETAN SUBJOINED LETTER HA@ from the Unicode standard, defined by @'\\x0fb7'@ (&#x2022;&#x0fb7;).
  | TibetanSymbolPadmaGdan  -- ^ The combining character @TIBETAN SYMBOL PADMA GDAN@ from the Unicode standard, defined by @'\\x0fc6'@ (&#x2022;&#x0fc6;).
  | MyanmarVowelSignIi  -- ^ The combining character @MYANMAR VOWEL SIGN II@ from the Unicode standard, defined by @'\\x102e'@ (&#x2022;&#x102e;).
  | MyanmarSignDotBelow  -- ^ The combining character @MYANMAR SIGN DOT BELOW@ from the Unicode standard, defined by @'\\x1037'@ (&#x2022;&#x1037;).
  | MyanmarSignVirama  -- ^ The combining character @MYANMAR SIGN VIRAMA@ from the Unicode standard, defined by @'\\x1039'@ (&#x2022;&#x1039;).
  | MyanmarSignAsat  -- ^ The combining character @MYANMAR SIGN ASAT@ from the Unicode standard, defined by @'\\x103a'@ (&#x2022;&#x103a;).
  | MyanmarSignShanCouncilEmphaticTone  -- ^ The combining character @MYANMAR SIGN SHAN COUNCIL EMPHATIC TONE@ from the Unicode standard, defined by @'\\x108d'@ (&#x2022;&#x108d;).
  | EthiopicCombiningGeminationAndVowelLengthMark  -- ^ The combining character @ETHIOPIC COMBINING GEMINATION AND VOWEL LENGTH MARK@ from the Unicode standard, defined by @'\\x135d'@ (&#x2022;&#x135d;).
  | EthiopicCombiningVowelLengthMark  -- ^ The combining character @ETHIOPIC COMBINING VOWEL LENGTH MARK@ from the Unicode standard, defined by @'\\x135e'@ (&#x2022;&#x135e;).
  | EthiopicCombiningGeminationMark  -- ^ The combining character @ETHIOPIC COMBINING GEMINATION MARK@ from the Unicode standard, defined by @'\\x135f'@ (&#x2022;&#x135f;).
  | TagalogSignVirama  -- ^ The combining character @TAGALOG SIGN VIRAMA@ from the Unicode standard, defined by @'\\x1714'@ (&#x2022;&#x1714;).
  | HanunooSignPamudpod  -- ^ The combining character @HANUNOO SIGN PAMUDPOD@ from the Unicode standard, defined by @'\\x1734'@ (&#x2022;&#x1734;).
  | KhmerSignCoeng  -- ^ The combining character @KHMER SIGN COENG@ from the Unicode standard, defined by @'\\x17d2'@ (&#x2022;&#x17d2;).
  | KhmerSignAtthacan  -- ^ The combining character @KHMER SIGN ATTHACAN@ from the Unicode standard, defined by @'\\x17dd'@ (&#x2022;&#x17dd;).
  | MongolianLetterAliGaliDagalga  -- ^ The combining character @MONGOLIAN LETTER ALI GALI DAGALGA@ from the Unicode standard, defined by @'\\x18a9'@ (&#x2022;&#x18a9;).
  | LimbuSignMukphreng  -- ^ The combining character @LIMBU SIGN MUKPHRENG@ from the Unicode standard, defined by @'\\x1939'@ (&#x2022;&#x1939;).
  | LimbuSignKemphreng  -- ^ The combining character @LIMBU SIGN KEMPHRENG@ from the Unicode standard, defined by @'\\x193a'@ (&#x2022;&#x193a;).
  | LimbuSignSaI  -- ^ The combining character @LIMBU SIGN SA-I@ from the Unicode standard, defined by @'\\x193b'@ (&#x2022;&#x193b;).
  | BugineseVowelSignI  -- ^ The combining character @BUGINESE VOWEL SIGN I@ from the Unicode standard, defined by @'\\x1a17'@ (&#x2022;&#x1a17;).
  | BugineseVowelSignU  -- ^ The combining character @BUGINESE VOWEL SIGN U@ from the Unicode standard, defined by @'\\x1a18'@ (&#x2022;&#x1a18;).
  | TaiThamSignSakot  -- ^ The combining character @TAI THAM SIGN SAKOT@ from the Unicode standard, defined by @'\\x1a60'@ (&#x2022;&#x1a60;).
  | TaiThamSignTone1  -- ^ The combining character @TAI THAM SIGN TONE-1@ from the Unicode standard, defined by @'\\x1a75'@ (&#x2022;&#x1a75;).
  | TaiThamSignTone2  -- ^ The combining character @TAI THAM SIGN TONE-2@ from the Unicode standard, defined by @'\\x1a76'@ (&#x2022;&#x1a76;).
  | TaiThamSignKhuenTone3  -- ^ The combining character @TAI THAM SIGN KHUEN TONE-3@ from the Unicode standard, defined by @'\\x1a77'@ (&#x2022;&#x1a77;).
  | TaiThamSignKhuenTone4  -- ^ The combining character @TAI THAM SIGN KHUEN TONE-4@ from the Unicode standard, defined by @'\\x1a78'@ (&#x2022;&#x1a78;).
  | TaiThamSignKhuenTone5  -- ^ The combining character @TAI THAM SIGN KHUEN TONE-5@ from the Unicode standard, defined by @'\\x1a79'@ (&#x2022;&#x1a79;).
  | TaiThamSignRaHaam  -- ^ The combining character @TAI THAM SIGN RA HAAM@ from the Unicode standard, defined by @'\\x1a7a'@ (&#x2022;&#x1a7a;).
  | TaiThamSignMaiSam  -- ^ The combining character @TAI THAM SIGN MAI SAM@ from the Unicode standard, defined by @'\\x1a7b'@ (&#x2022;&#x1a7b;).
  | TaiThamSignKhuenLueKaran  -- ^ The combining character @TAI THAM SIGN KHUEN-LUE KARAN@ from the Unicode standard, defined by @'\\x1a7c'@ (&#x2022;&#x1a7c;).
  | TaiThamCombiningCryptogrammicDot  -- ^ The combining character @TAI THAM COMBINING CRYPTOGRAMMIC DOT@ from the Unicode standard, defined by @'\\x1a7f'@ (&#x2022;&#x1a7f;).
  | CombiningDoubledCircumflexAccent  -- ^ The combining character @COMBINING DOUBLED CIRCUMFLEX ACCENT@ from the Unicode standard, defined by @'\\x1ab0'@ (&#x2022;&#x1ab0;).
  | CombiningDiaeresisRing  -- ^ The combining character @COMBINING DIAERESIS-RING@ from the Unicode standard, defined by @'\\x1ab1'@ (&#x2022;&#x1ab1;).
  | CombiningInfinity  -- ^ The combining character @COMBINING INFINITY@ from the Unicode standard, defined by @'\\x1ab2'@ (&#x2022;&#x1ab2;).
  | CombiningDownwardsArrow  -- ^ The combining character @COMBINING DOWNWARDS ARROW@ from the Unicode standard, defined by @'\\x1ab3'@ (&#x2022;&#x1ab3;).
  | CombiningTripleDot  -- ^ The combining character @COMBINING TRIPLE DOT@ from the Unicode standard, defined by @'\\x1ab4'@ (&#x2022;&#x1ab4;).
  | CombiningXXBelow  -- ^ The combining character @COMBINING X-X BELOW@ from the Unicode standard, defined by @'\\x1ab5'@ (&#x2022;&#x1ab5;).
  | CombiningWigglyLineBelow  -- ^ The combining character @COMBINING WIGGLY LINE BELOW@ from the Unicode standard, defined by @'\\x1ab6'@ (&#x2022;&#x1ab6;).
  | CombiningOpenMarkBelow  -- ^ The combining character @COMBINING OPEN MARK BELOW@ from the Unicode standard, defined by @'\\x1ab7'@ (&#x2022;&#x1ab7;).
  | CombiningDoubleOpenMarkBelow  -- ^ The combining character @COMBINING DOUBLE OPEN MARK BELOW@ from the Unicode standard, defined by @'\\x1ab8'@ (&#x2022;&#x1ab8;).
  | CombiningLightCentralizationStrokeBelow  -- ^ The combining character @COMBINING LIGHT CENTRALIZATION STROKE BELOW@ from the Unicode standard, defined by @'\\x1ab9'@ (&#x2022;&#x1ab9;).
  | CombiningStrongCentralizationStrokeBelow  -- ^ The combining character @COMBINING STRONG CENTRALIZATION STROKE BELOW@ from the Unicode standard, defined by @'\\x1aba'@ (&#x2022;&#x1aba;).
  | CombiningParenthesesAbove  -- ^ The combining character @COMBINING PARENTHESES ABOVE@ from the Unicode standard, defined by @'\\x1abb'@ (&#x2022;&#x1abb;).
  | CombiningDoubleParenthesesAbove  -- ^ The combining character @COMBINING DOUBLE PARENTHESES ABOVE@ from the Unicode standard, defined by @'\\x1abc'@ (&#x2022;&#x1abc;).
  | CombiningParenthesesBelow  -- ^ The combining character @COMBINING PARENTHESES BELOW@ from the Unicode standard, defined by @'\\x1abd'@ (&#x2022;&#x1abd;).
  | BalineseSignRerekan  -- ^ The combining character @BALINESE SIGN REREKAN@ from the Unicode standard, defined by @'\\x1b34'@ (&#x2022;&#x1b34;).
  | BalineseVowelSignTedung  -- ^ The combining character @BALINESE VOWEL SIGN TEDUNG@ from the Unicode standard, defined by @'\\x1b35'@ (&#x2022;&#x1b35;).
  | BalineseAdegAdeg  -- ^ The combining character @BALINESE ADEG ADEG@ from the Unicode standard, defined by @'\\x1b44'@ (&#x2022;&#x1b44;).
  | BalineseMusicalSymbolCombiningTegeh  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING TEGEH@ from the Unicode standard, defined by @'\\x1b6b'@ (&#x2022;&#x1b6b;).
  | BalineseMusicalSymbolCombiningEndep  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING ENDEP@ from the Unicode standard, defined by @'\\x1b6c'@ (&#x2022;&#x1b6c;).
  | BalineseMusicalSymbolCombiningKempul  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING KEMPUL@ from the Unicode standard, defined by @'\\x1b6d'@ (&#x2022;&#x1b6d;).
  | BalineseMusicalSymbolCombiningKempli  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING KEMPLI@ from the Unicode standard, defined by @'\\x1b6e'@ (&#x2022;&#x1b6e;).
  | BalineseMusicalSymbolCombiningJegogan  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING JEGOGAN@ from the Unicode standard, defined by @'\\x1b6f'@ (&#x2022;&#x1b6f;).
  | BalineseMusicalSymbolCombiningKempulWithJegogan  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING KEMPUL WITH JEGOGAN@ from the Unicode standard, defined by @'\\x1b70'@ (&#x2022;&#x1b70;).
  | BalineseMusicalSymbolCombiningKempliWithJegogan  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING KEMPLI WITH JEGOGAN@ from the Unicode standard, defined by @'\\x1b71'@ (&#x2022;&#x1b71;).
  | BalineseMusicalSymbolCombiningBende  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING BENDE@ from the Unicode standard, defined by @'\\x1b72'@ (&#x2022;&#x1b72;).
  | BalineseMusicalSymbolCombiningGong  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING GONG@ from the Unicode standard, defined by @'\\x1b73'@ (&#x2022;&#x1b73;).
  | SundaneseSignPamaaeh  -- ^ The combining character @SUNDANESE SIGN PAMAAEH@ from the Unicode standard, defined by @'\\x1baa'@ (&#x2022;&#x1baa;).
  | SundaneseSignVirama  -- ^ The combining character @SUNDANESE SIGN VIRAMA@ from the Unicode standard, defined by @'\\x1bab'@ (&#x2022;&#x1bab;).
  | BatakSignTompi  -- ^ The combining character @BATAK SIGN TOMPI@ from the Unicode standard, defined by @'\\x1be6'@ (&#x2022;&#x1be6;).
  | BatakPangolat  -- ^ The combining character @BATAK PANGOLAT@ from the Unicode standard, defined by @'\\x1bf2'@ (&#x2022;&#x1bf2;).
  | BatakPanongonan  -- ^ The combining character @BATAK PANONGONAN@ from the Unicode standard, defined by @'\\x1bf3'@ (&#x2022;&#x1bf3;).
  | LepchaSignNukta  -- ^ The combining character @LEPCHA SIGN NUKTA@ from the Unicode standard, defined by @'\\x1c37'@ (&#x2022;&#x1c37;).
  | VedicToneKarshana  -- ^ The combining character @VEDIC TONE KARSHANA@ from the Unicode standard, defined by @'\\x1cd0'@ (&#x2022;&#x1cd0;).
  | VedicToneShara  -- ^ The combining character @VEDIC TONE SHARA@ from the Unicode standard, defined by @'\\x1cd1'@ (&#x2022;&#x1cd1;).
  | VedicTonePrenkha  -- ^ The combining character @VEDIC TONE PRENKHA@ from the Unicode standard, defined by @'\\x1cd2'@ (&#x2022;&#x1cd2;).
  | VedicSignYajurvedicMidlineSvarita  -- ^ The combining character @VEDIC SIGN YAJURVEDIC MIDLINE SVARITA@ from the Unicode standard, defined by @'\\x1cd4'@ (&#x2022;&#x1cd4;).
  | VedicToneYajurvedicAggravatedIndependentSvarita  -- ^ The combining character @VEDIC TONE YAJURVEDIC AGGRAVATED INDEPENDENT SVARITA@ from the Unicode standard, defined by @'\\x1cd5'@ (&#x2022;&#x1cd5;).
  | VedicToneYajurvedicIndependentSvarita  -- ^ The combining character @VEDIC TONE YAJURVEDIC INDEPENDENT SVARITA@ from the Unicode standard, defined by @'\\x1cd6'@ (&#x2022;&#x1cd6;).
  | VedicToneYajurvedicKathakaIndependentSvarita  -- ^ The combining character @VEDIC TONE YAJURVEDIC KATHAKA INDEPENDENT SVARITA@ from the Unicode standard, defined by @'\\x1cd7'@ (&#x2022;&#x1cd7;).
  | VedicToneCandraBelow  -- ^ The combining character @VEDIC TONE CANDRA BELOW@ from the Unicode standard, defined by @'\\x1cd8'@ (&#x2022;&#x1cd8;).
  | VedicToneYajurvedicKathakaIndependentSvaritaSchroeder  -- ^ The combining character @VEDIC TONE YAJURVEDIC KATHAKA INDEPENDENT SVARITA SCHROEDER@ from the Unicode standard, defined by @'\\x1cd9'@ (&#x2022;&#x1cd9;).
  | VedicToneDoubleSvarita  -- ^ The combining character @VEDIC TONE DOUBLE SVARITA@ from the Unicode standard, defined by @'\\x1cda'@ (&#x2022;&#x1cda;).
  | VedicToneTripleSvarita  -- ^ The combining character @VEDIC TONE TRIPLE SVARITA@ from the Unicode standard, defined by @'\\x1cdb'@ (&#x2022;&#x1cdb;).
  | VedicToneKathakaAnudatta  -- ^ The combining character @VEDIC TONE KATHAKA ANUDATTA@ from the Unicode standard, defined by @'\\x1cdc'@ (&#x2022;&#x1cdc;).
  | VedicToneDotBelow  -- ^ The combining character @VEDIC TONE DOT BELOW@ from the Unicode standard, defined by @'\\x1cdd'@ (&#x2022;&#x1cdd;).
  | VedicToneTwoDotsBelow  -- ^ The combining character @VEDIC TONE TWO DOTS BELOW@ from the Unicode standard, defined by @'\\x1cde'@ (&#x2022;&#x1cde;).
  | VedicToneThreeDotsBelow  -- ^ The combining character @VEDIC TONE THREE DOTS BELOW@ from the Unicode standard, defined by @'\\x1cdf'@ (&#x2022;&#x1cdf;).
  | VedicToneRigvedicKashmiriIndependentSvarita  -- ^ The combining character @VEDIC TONE RIGVEDIC KASHMIRI INDEPENDENT SVARITA@ from the Unicode standard, defined by @'\\x1ce0'@ (&#x2022;&#x1ce0;).
  | VedicSignVisargaSvarita  -- ^ The combining character @VEDIC SIGN VISARGA SVARITA@ from the Unicode standard, defined by @'\\x1ce2'@ (&#x2022;&#x1ce2;).
  | VedicSignVisargaUdatta  -- ^ The combining character @VEDIC SIGN VISARGA UDATTA@ from the Unicode standard, defined by @'\\x1ce3'@ (&#x2022;&#x1ce3;).
  | VedicSignReversedVisargaUdatta  -- ^ The combining character @VEDIC SIGN REVERSED VISARGA UDATTA@ from the Unicode standard, defined by @'\\x1ce4'@ (&#x2022;&#x1ce4;).
  | VedicSignVisargaAnudatta  -- ^ The combining character @VEDIC SIGN VISARGA ANUDATTA@ from the Unicode standard, defined by @'\\x1ce5'@ (&#x2022;&#x1ce5;).
  | VedicSignReversedVisargaAnudatta  -- ^ The combining character @VEDIC SIGN REVERSED VISARGA ANUDATTA@ from the Unicode standard, defined by @'\\x1ce6'@ (&#x2022;&#x1ce6;).
  | VedicSignVisargaUdattaWithTail  -- ^ The combining character @VEDIC SIGN VISARGA UDATTA WITH TAIL@ from the Unicode standard, defined by @'\\x1ce7'@ (&#x2022;&#x1ce7;).
  | VedicSignVisargaAnudattaWithTail  -- ^ The combining character @VEDIC SIGN VISARGA ANUDATTA WITH TAIL@ from the Unicode standard, defined by @'\\x1ce8'@ (&#x2022;&#x1ce8;).
  | VedicSignTiryak  -- ^ The combining character @VEDIC SIGN TIRYAK@ from the Unicode standard, defined by @'\\x1ced'@ (&#x2022;&#x1ced;).
  | VedicToneCandraAbove  -- ^ The combining character @VEDIC TONE CANDRA ABOVE@ from the Unicode standard, defined by @'\\x1cf4'@ (&#x2022;&#x1cf4;).
  | VedicToneRingAbove  -- ^ The combining character @VEDIC TONE RING ABOVE@ from the Unicode standard, defined by @'\\x1cf8'@ (&#x2022;&#x1cf8;).
  | VedicToneDoubleRingAbove  -- ^ The combining character @VEDIC TONE DOUBLE RING ABOVE@ from the Unicode standard, defined by @'\\x1cf9'@ (&#x2022;&#x1cf9;).
  | CombiningDottedGraveAccent  -- ^ The combining character @COMBINING DOTTED GRAVE ACCENT@ from the Unicode standard, defined by @'\\x1dc0'@ (&#x2022;&#x1dc0;).
  | CombiningDottedAcuteAccent  -- ^ The combining character @COMBINING DOTTED ACUTE ACCENT@ from the Unicode standard, defined by @'\\x1dc1'@ (&#x2022;&#x1dc1;).
  | CombiningSnakeBelow  -- ^ The combining character @COMBINING SNAKE BELOW@ from the Unicode standard, defined by @'\\x1dc2'@ (&#x2022;&#x1dc2;).
  | CombiningSuspensionMark  -- ^ The combining character @COMBINING SUSPENSION MARK@ from the Unicode standard, defined by @'\\x1dc3'@ (&#x2022;&#x1dc3;).
  | CombiningMacronAcute  -- ^ The combining character @COMBINING MACRON-ACUTE@ from the Unicode standard, defined by @'\\x1dc4'@ (&#x2022;&#x1dc4;).
  | CombiningGraveMacron  -- ^ The combining character @COMBINING GRAVE-MACRON@ from the Unicode standard, defined by @'\\x1dc5'@ (&#x2022;&#x1dc5;).
  | CombiningMacronGrave  -- ^ The combining character @COMBINING MACRON-GRAVE@ from the Unicode standard, defined by @'\\x1dc6'@ (&#x2022;&#x1dc6;).
  | CombiningAcuteMacron  -- ^ The combining character @COMBINING ACUTE-MACRON@ from the Unicode standard, defined by @'\\x1dc7'@ (&#x2022;&#x1dc7;).
  | CombiningGraveAcuteGrave  -- ^ The combining character @COMBINING GRAVE-ACUTE-GRAVE@ from the Unicode standard, defined by @'\\x1dc8'@ (&#x2022;&#x1dc8;).
  | CombiningAcuteGraveAcute  -- ^ The combining character @COMBINING ACUTE-GRAVE-ACUTE@ from the Unicode standard, defined by @'\\x1dc9'@ (&#x2022;&#x1dc9;).
  | CombiningLatinSmallLetterRBelow  -- ^ The combining character @COMBINING LATIN SMALL LETTER R BELOW@ from the Unicode standard, defined by @'\\x1dca'@ (&#x2022;&#x1dca;).
  | CombiningBreveMacron  -- ^ The combining character @COMBINING BREVE-MACRON@ from the Unicode standard, defined by @'\\x1dcb'@ (&#x2022;&#x1dcb;).
  | CombiningMacronBreve  -- ^ The combining character @COMBINING MACRON-BREVE@ from the Unicode standard, defined by @'\\x1dcc'@ (&#x2022;&#x1dcc;).
  | CombiningDoubleCircumflexAbove  -- ^ The combining character @COMBINING DOUBLE CIRCUMFLEX ABOVE@ from the Unicode standard, defined by @'\\x1dcd'@ (&#x2022;&#x1dcd;).
  | CombiningOgonekAbove  -- ^ The combining character @COMBINING OGONEK ABOVE@ from the Unicode standard, defined by @'\\x1dce'@ (&#x2022;&#x1dce;).
  | CombiningZigzagBelow  -- ^ The combining character @COMBINING ZIGZAG BELOW@ from the Unicode standard, defined by @'\\x1dcf'@ (&#x2022;&#x1dcf;).
  | CombiningIsBelow  -- ^ The combining character @COMBINING IS BELOW@ from the Unicode standard, defined by @'\\x1dd0'@ (&#x2022;&#x1dd0;).
  | CombiningUrAbove  -- ^ The combining character @COMBINING UR ABOVE@ from the Unicode standard, defined by @'\\x1dd1'@ (&#x2022;&#x1dd1;).
  | CombiningUsAbove  -- ^ The combining character @COMBINING US ABOVE@ from the Unicode standard, defined by @'\\x1dd2'@ (&#x2022;&#x1dd2;).
  | CombiningLatinSmallLetterFlattenedOpenAAbove  -- ^ The combining character @COMBINING LATIN SMALL LETTER FLATTENED OPEN A ABOVE@ from the Unicode standard, defined by @'\\x1dd3'@ (&#x2022;&#x1dd3;).
  | CombiningLatinSmallLetterAe  -- ^ The combining character @COMBINING LATIN SMALL LETTER AE@ from the Unicode standard, defined by @'\\x1dd4'@ (&#x2022;&#x1dd4;).
  | CombiningLatinSmallLetterAo  -- ^ The combining character @COMBINING LATIN SMALL LETTER AO@ from the Unicode standard, defined by @'\\x1dd5'@ (&#x2022;&#x1dd5;).
  | CombiningLatinSmallLetterAv  -- ^ The combining character @COMBINING LATIN SMALL LETTER AV@ from the Unicode standard, defined by @'\\x1dd6'@ (&#x2022;&#x1dd6;).
  | CombiningLatinSmallLetterCCedilla  -- ^ The combining character @COMBINING LATIN SMALL LETTER C CEDILLA@ from the Unicode standard, defined by @'\\x1dd7'@ (&#x2022;&#x1dd7;).
  | CombiningLatinSmallLetterInsularD  -- ^ The combining character @COMBINING LATIN SMALL LETTER INSULAR D@ from the Unicode standard, defined by @'\\x1dd8'@ (&#x2022;&#x1dd8;).
  | CombiningLatinSmallLetterEth  -- ^ The combining character @COMBINING LATIN SMALL LETTER ETH@ from the Unicode standard, defined by @'\\x1dd9'@ (&#x2022;&#x1dd9;).
  | CombiningLatinSmallLetterG  -- ^ The combining character @COMBINING LATIN SMALL LETTER G@ from the Unicode standard, defined by @'\\x1dda'@ (&#x2022;&#x1dda;).
  | CombiningLatinLetterSmallCapitalG  -- ^ The combining character @COMBINING LATIN LETTER SMALL CAPITAL G@ from the Unicode standard, defined by @'\\x1ddb'@ (&#x2022;&#x1ddb;).
  | CombiningLatinSmallLetterK  -- ^ The combining character @COMBINING LATIN SMALL LETTER K@ from the Unicode standard, defined by @'\\x1ddc'@ (&#x2022;&#x1ddc;).
  | CombiningLatinSmallLetterL  -- ^ The combining character @COMBINING LATIN SMALL LETTER L@ from the Unicode standard, defined by @'\\x1ddd'@ (&#x2022;&#x1ddd;).
  | CombiningLatinLetterSmallCapitalL  -- ^ The combining character @COMBINING LATIN LETTER SMALL CAPITAL L@ from the Unicode standard, defined by @'\\x1dde'@ (&#x2022;&#x1dde;).
  | CombiningLatinLetterSmallCapitalM  -- ^ The combining character @COMBINING LATIN LETTER SMALL CAPITAL M@ from the Unicode standard, defined by @'\\x1ddf'@ (&#x2022;&#x1ddf;).
  | CombiningLatinSmallLetterN  -- ^ The combining character @COMBINING LATIN SMALL LETTER N@ from the Unicode standard, defined by @'\\x1de0'@ (&#x2022;&#x1de0;).
  | CombiningLatinLetterSmallCapitalN  -- ^ The combining character @COMBINING LATIN LETTER SMALL CAPITAL N@ from the Unicode standard, defined by @'\\x1de1'@ (&#x2022;&#x1de1;).
  | CombiningLatinLetterSmallCapitalR  -- ^ The combining character @COMBINING LATIN LETTER SMALL CAPITAL R@ from the Unicode standard, defined by @'\\x1de2'@ (&#x2022;&#x1de2;).
  | CombiningLatinSmallLetterRRotunda  -- ^ The combining character @COMBINING LATIN SMALL LETTER R ROTUNDA@ from the Unicode standard, defined by @'\\x1de3'@ (&#x2022;&#x1de3;).
  | CombiningLatinSmallLetterS  -- ^ The combining character @COMBINING LATIN SMALL LETTER S@ from the Unicode standard, defined by @'\\x1de4'@ (&#x2022;&#x1de4;).
  | CombiningLatinSmallLetterLongS  -- ^ The combining character @COMBINING LATIN SMALL LETTER LONG S@ from the Unicode standard, defined by @'\\x1de5'@ (&#x2022;&#x1de5;).
  | CombiningLatinSmallLetterZ  -- ^ The combining character @COMBINING LATIN SMALL LETTER Z@ from the Unicode standard, defined by @'\\x1de6'@ (&#x2022;&#x1de6;).
  | CombiningLatinSmallLetterAlpha  -- ^ The combining character @COMBINING LATIN SMALL LETTER ALPHA@ from the Unicode standard, defined by @'\\x1de7'@ (&#x2022;&#x1de7;).
  | CombiningLatinSmallLetterB  -- ^ The combining character @COMBINING LATIN SMALL LETTER B@ from the Unicode standard, defined by @'\\x1de8'@ (&#x2022;&#x1de8;).
  | CombiningLatinSmallLetterBeta  -- ^ The combining character @COMBINING LATIN SMALL LETTER BETA@ from the Unicode standard, defined by @'\\x1de9'@ (&#x2022;&#x1de9;).
  | CombiningLatinSmallLetterSchwa  -- ^ The combining character @COMBINING LATIN SMALL LETTER SCHWA@ from the Unicode standard, defined by @'\\x1dea'@ (&#x2022;&#x1dea;).
  | CombiningLatinSmallLetterF  -- ^ The combining character @COMBINING LATIN SMALL LETTER F@ from the Unicode standard, defined by @'\\x1deb'@ (&#x2022;&#x1deb;).
  | CombiningLatinSmallLetterLWithDoubleMiddleTilde  -- ^ The combining character @COMBINING LATIN SMALL LETTER L WITH DOUBLE MIDDLE TILDE@ from the Unicode standard, defined by @'\\x1dec'@ (&#x2022;&#x1dec;).
  | CombiningLatinSmallLetterOWithLightCentralizationStroke  -- ^ The combining character @COMBINING LATIN SMALL LETTER O WITH LIGHT CENTRALIZATION STROKE@ from the Unicode standard, defined by @'\\x1ded'@ (&#x2022;&#x1ded;).
  | CombiningLatinSmallLetterP  -- ^ The combining character @COMBINING LATIN SMALL LETTER P@ from the Unicode standard, defined by @'\\x1dee'@ (&#x2022;&#x1dee;).
  | CombiningLatinSmallLetterEsh  -- ^ The combining character @COMBINING LATIN SMALL LETTER ESH@ from the Unicode standard, defined by @'\\x1def'@ (&#x2022;&#x1def;).
  | CombiningLatinSmallLetterUWithLightCentralizationStroke  -- ^ The combining character @COMBINING LATIN SMALL LETTER U WITH LIGHT CENTRALIZATION STROKE@ from the Unicode standard, defined by @'\\x1df0'@ (&#x2022;&#x1df0;).
  | CombiningLatinSmallLetterW  -- ^ The combining character @COMBINING LATIN SMALL LETTER W@ from the Unicode standard, defined by @'\\x1df1'@ (&#x2022;&#x1df1;).
  | CombiningLatinSmallLetterAWithDiaeresis  -- ^ The combining character @COMBINING LATIN SMALL LETTER A WITH DIAERESIS@ from the Unicode standard, defined by @'\\x1df2'@ (&#x2022;&#x1df2;).
  | CombiningLatinSmallLetterOWithDiaeresis  -- ^ The combining character @COMBINING LATIN SMALL LETTER O WITH DIAERESIS@ from the Unicode standard, defined by @'\\x1df3'@ (&#x2022;&#x1df3;).
  | CombiningLatinSmallLetterUWithDiaeresis  -- ^ The combining character @COMBINING LATIN SMALL LETTER U WITH DIAERESIS@ from the Unicode standard, defined by @'\\x1df4'@ (&#x2022;&#x1df4;).
  | CombiningUpTackAbove  -- ^ The combining character @COMBINING UP TACK ABOVE@ from the Unicode standard, defined by @'\\x1df5'@ (&#x2022;&#x1df5;).
  | CombiningDeletionMark  -- ^ The combining character @COMBINING DELETION MARK@ from the Unicode standard, defined by @'\\x1dfb'@ (&#x2022;&#x1dfb;).
  | CombiningDoubleInvertedBreveBelow  -- ^ The combining character @COMBINING DOUBLE INVERTED BREVE BELOW@ from the Unicode standard, defined by @'\\x1dfc'@ (&#x2022;&#x1dfc;).
  | CombiningAlmostEqualToBelow  -- ^ The combining character @COMBINING ALMOST EQUAL TO BELOW@ from the Unicode standard, defined by @'\\x1dfd'@ (&#x2022;&#x1dfd;).
  | CombiningLeftArrowheadAbove  -- ^ The combining character @COMBINING LEFT ARROWHEAD ABOVE@ from the Unicode standard, defined by @'\\x1dfe'@ (&#x2022;&#x1dfe;).
  | CombiningRightArrowheadAndDownArrowheadBelow  -- ^ The combining character @COMBINING RIGHT ARROWHEAD AND DOWN ARROWHEAD BELOW@ from the Unicode standard, defined by @'\\x1dff'@ (&#x2022;&#x1dff;).
  | CombiningLeftHarpoonAbove  -- ^ The combining character @COMBINING LEFT HARPOON ABOVE@ from the Unicode standard, defined by @'\\x20d0'@ (&#x2022;&#x20d0;).
  | CombiningRightHarpoonAbove  -- ^ The combining character @COMBINING RIGHT HARPOON ABOVE@ from the Unicode standard, defined by @'\\x20d1'@ (&#x2022;&#x20d1;).
  | CombiningLongVerticalLineOverlay  -- ^ The combining character @COMBINING LONG VERTICAL LINE OVERLAY@ from the Unicode standard, defined by @'\\x20d2'@ (&#x2022;&#x20d2;).
  | CombiningShortVerticalLineOverlay  -- ^ The combining character @COMBINING SHORT VERTICAL LINE OVERLAY@ from the Unicode standard, defined by @'\\x20d3'@ (&#x2022;&#x20d3;).
  | CombiningAnticlockwiseArrowAbove  -- ^ The combining character @COMBINING ANTICLOCKWISE ARROW ABOVE@ from the Unicode standard, defined by @'\\x20d4'@ (&#x2022;&#x20d4;).
  | CombiningClockwiseArrowAbove  -- ^ The combining character @COMBINING CLOCKWISE ARROW ABOVE@ from the Unicode standard, defined by @'\\x20d5'@ (&#x2022;&#x20d5;).
  | CombiningLeftArrowAbove  -- ^ The combining character @COMBINING LEFT ARROW ABOVE@ from the Unicode standard, defined by @'\\x20d6'@ (&#x2022;&#x20d6;).
  | CombiningRightArrowAbove  -- ^ The combining character @COMBINING RIGHT ARROW ABOVE@ from the Unicode standard, defined by @'\\x20d7'@ (&#x2022;&#x20d7;).
  | CombiningRingOverlay  -- ^ The combining character @COMBINING RING OVERLAY@ from the Unicode standard, defined by @'\\x20d8'@ (&#x2022;&#x20d8;).
  | CombiningClockwiseRingOverlay  -- ^ The combining character @COMBINING CLOCKWISE RING OVERLAY@ from the Unicode standard, defined by @'\\x20d9'@ (&#x2022;&#x20d9;).
  | CombiningAnticlockwiseRingOverlay  -- ^ The combining character @COMBINING ANTICLOCKWISE RING OVERLAY@ from the Unicode standard, defined by @'\\x20da'@ (&#x2022;&#x20da;).
  | CombiningThreeDotsAbove  -- ^ The combining character @COMBINING THREE DOTS ABOVE@ from the Unicode standard, defined by @'\\x20db'@ (&#x2022;&#x20db;).
  | CombiningFourDotsAbove  -- ^ The combining character @COMBINING FOUR DOTS ABOVE@ from the Unicode standard, defined by @'\\x20dc'@ (&#x2022;&#x20dc;).
  | CombiningLeftRightArrowAbove  -- ^ The combining character @COMBINING LEFT RIGHT ARROW ABOVE@ from the Unicode standard, defined by @'\\x20e1'@ (&#x2022;&#x20e1;).
  | CombiningReverseSolidusOverlay  -- ^ The combining character @COMBINING REVERSE SOLIDUS OVERLAY@ from the Unicode standard, defined by @'\\x20e5'@ (&#x2022;&#x20e5;).
  | CombiningDoubleVerticalStrokeOverlay  -- ^ The combining character @COMBINING DOUBLE VERTICAL STROKE OVERLAY@ from the Unicode standard, defined by @'\\x20e6'@ (&#x2022;&#x20e6;).
  | CombiningAnnuitySymbol  -- ^ The combining character @COMBINING ANNUITY SYMBOL@ from the Unicode standard, defined by @'\\x20e7'@ (&#x2022;&#x20e7;).
  | CombiningTripleUnderdot  -- ^ The combining character @COMBINING TRIPLE UNDERDOT@ from the Unicode standard, defined by @'\\x20e8'@ (&#x2022;&#x20e8;).
  | CombiningWideBridgeAbove  -- ^ The combining character @COMBINING WIDE BRIDGE ABOVE@ from the Unicode standard, defined by @'\\x20e9'@ (&#x2022;&#x20e9;).
  | CombiningLeftwardsArrowOverlay  -- ^ The combining character @COMBINING LEFTWARDS ARROW OVERLAY@ from the Unicode standard, defined by @'\\x20ea'@ (&#x2022;&#x20ea;).
  | CombiningLongDoubleSolidusOverlay  -- ^ The combining character @COMBINING LONG DOUBLE SOLIDUS OVERLAY@ from the Unicode standard, defined by @'\\x20eb'@ (&#x2022;&#x20eb;).
  | CombiningRightwardsHarpoonWithBarbDownwards  -- ^ The combining character @COMBINING RIGHTWARDS HARPOON WITH BARB DOWNWARDS@ from the Unicode standard, defined by @'\\x20ec'@ (&#x2022;&#x20ec;).
  | CombiningLeftwardsHarpoonWithBarbDownwards  -- ^ The combining character @COMBINING LEFTWARDS HARPOON WITH BARB DOWNWARDS@ from the Unicode standard, defined by @'\\x20ed'@ (&#x2022;&#x20ed;).
  | CombiningLeftArrowBelow  -- ^ The combining character @COMBINING LEFT ARROW BELOW@ from the Unicode standard, defined by @'\\x20ee'@ (&#x2022;&#x20ee;).
  | CombiningRightArrowBelow  -- ^ The combining character @COMBINING RIGHT ARROW BELOW@ from the Unicode standard, defined by @'\\x20ef'@ (&#x2022;&#x20ef;).
  | CombiningAsteriskAbove  -- ^ The combining character @COMBINING ASTERISK ABOVE@ from the Unicode standard, defined by @'\\x20f0'@ (&#x2022;&#x20f0;).
  | CopticCombiningNiAbove  -- ^ The combining character @COPTIC COMBINING NI ABOVE@ from the Unicode standard, defined by @'\\x2cef'@ (&#x2022;&#x2cef;).
  | CopticCombiningSpiritusAsper  -- ^ The combining character @COPTIC COMBINING SPIRITUS ASPER@ from the Unicode standard, defined by @'\\x2cf0'@ (&#x2022;&#x2cf0;).
  | CopticCombiningSpiritusLenis  -- ^ The combining character @COPTIC COMBINING SPIRITUS LENIS@ from the Unicode standard, defined by @'\\x2cf1'@ (&#x2022;&#x2cf1;).
  | TifinaghConsonantJoiner  -- ^ The combining character @TIFINAGH CONSONANT JOINER@ from the Unicode standard, defined by @'\\x2d7f'@ (&#x2022;&#x2d7f;).
  | CombiningCyrillicLetterBe  -- ^ The combining character @COMBINING CYRILLIC LETTER BE@ from the Unicode standard, defined by @'\\x2de0'@ (&#x2022;&#x2de0;).
  | CombiningCyrillicLetterVe  -- ^ The combining character @COMBINING CYRILLIC LETTER VE@ from the Unicode standard, defined by @'\\x2de1'@ (&#x2022;&#x2de1;).
  | CombiningCyrillicLetterGhe  -- ^ The combining character @COMBINING CYRILLIC LETTER GHE@ from the Unicode standard, defined by @'\\x2de2'@ (&#x2022;&#x2de2;).
  | CombiningCyrillicLetterDe  -- ^ The combining character @COMBINING CYRILLIC LETTER DE@ from the Unicode standard, defined by @'\\x2de3'@ (&#x2022;&#x2de3;).
  | CombiningCyrillicLetterZhe  -- ^ The combining character @COMBINING CYRILLIC LETTER ZHE@ from the Unicode standard, defined by @'\\x2de4'@ (&#x2022;&#x2de4;).
  | CombiningCyrillicLetterZe  -- ^ The combining character @COMBINING CYRILLIC LETTER ZE@ from the Unicode standard, defined by @'\\x2de5'@ (&#x2022;&#x2de5;).
  | CombiningCyrillicLetterKa  -- ^ The combining character @COMBINING CYRILLIC LETTER KA@ from the Unicode standard, defined by @'\\x2de6'@ (&#x2022;&#x2de6;).
  | CombiningCyrillicLetterEl  -- ^ The combining character @COMBINING CYRILLIC LETTER EL@ from the Unicode standard, defined by @'\\x2de7'@ (&#x2022;&#x2de7;).
  | CombiningCyrillicLetterEm  -- ^ The combining character @COMBINING CYRILLIC LETTER EM@ from the Unicode standard, defined by @'\\x2de8'@ (&#x2022;&#x2de8;).
  | CombiningCyrillicLetterEn  -- ^ The combining character @COMBINING CYRILLIC LETTER EN@ from the Unicode standard, defined by @'\\x2de9'@ (&#x2022;&#x2de9;).
  | CombiningCyrillicLetterO  -- ^ The combining character @COMBINING CYRILLIC LETTER O@ from the Unicode standard, defined by @'\\x2dea'@ (&#x2022;&#x2dea;).
  | CombiningCyrillicLetterPe  -- ^ The combining character @COMBINING CYRILLIC LETTER PE@ from the Unicode standard, defined by @'\\x2deb'@ (&#x2022;&#x2deb;).
  | CombiningCyrillicLetterEr  -- ^ The combining character @COMBINING CYRILLIC LETTER ER@ from the Unicode standard, defined by @'\\x2dec'@ (&#x2022;&#x2dec;).
  | CombiningCyrillicLetterEs  -- ^ The combining character @COMBINING CYRILLIC LETTER ES@ from the Unicode standard, defined by @'\\x2ded'@ (&#x2022;&#x2ded;).
  | CombiningCyrillicLetterTe  -- ^ The combining character @COMBINING CYRILLIC LETTER TE@ from the Unicode standard, defined by @'\\x2dee'@ (&#x2022;&#x2dee;).
  | CombiningCyrillicLetterHa  -- ^ The combining character @COMBINING CYRILLIC LETTER HA@ from the Unicode standard, defined by @'\\x2def'@ (&#x2022;&#x2def;).
  | CombiningCyrillicLetterTse  -- ^ The combining character @COMBINING CYRILLIC LETTER TSE@ from the Unicode standard, defined by @'\\x2df0'@ (&#x2022;&#x2df0;).
  | CombiningCyrillicLetterChe  -- ^ The combining character @COMBINING CYRILLIC LETTER CHE@ from the Unicode standard, defined by @'\\x2df1'@ (&#x2022;&#x2df1;).
  | CombiningCyrillicLetterSha  -- ^ The combining character @COMBINING CYRILLIC LETTER SHA@ from the Unicode standard, defined by @'\\x2df2'@ (&#x2022;&#x2df2;).
  | CombiningCyrillicLetterShcha  -- ^ The combining character @COMBINING CYRILLIC LETTER SHCHA@ from the Unicode standard, defined by @'\\x2df3'@ (&#x2022;&#x2df3;).
  | CombiningCyrillicLetterFita  -- ^ The combining character @COMBINING CYRILLIC LETTER FITA@ from the Unicode standard, defined by @'\\x2df4'@ (&#x2022;&#x2df4;).
  | CombiningCyrillicLetterEsTe  -- ^ The combining character @COMBINING CYRILLIC LETTER ES-TE@ from the Unicode standard, defined by @'\\x2df5'@ (&#x2022;&#x2df5;).
  | CombiningCyrillicLetterA  -- ^ The combining character @COMBINING CYRILLIC LETTER A@ from the Unicode standard, defined by @'\\x2df6'@ (&#x2022;&#x2df6;).
  | CombiningCyrillicLetterIe  -- ^ The combining character @COMBINING CYRILLIC LETTER IE@ from the Unicode standard, defined by @'\\x2df7'@ (&#x2022;&#x2df7;).
  | CombiningCyrillicLetterDjerv  -- ^ The combining character @COMBINING CYRILLIC LETTER DJERV@ from the Unicode standard, defined by @'\\x2df8'@ (&#x2022;&#x2df8;).
  | CombiningCyrillicLetterMonographUk  -- ^ The combining character @COMBINING CYRILLIC LETTER MONOGRAPH UK@ from the Unicode standard, defined by @'\\x2df9'@ (&#x2022;&#x2df9;).
  | CombiningCyrillicLetterYat  -- ^ The combining character @COMBINING CYRILLIC LETTER YAT@ from the Unicode standard, defined by @'\\x2dfa'@ (&#x2022;&#x2dfa;).
  | CombiningCyrillicLetterYu  -- ^ The combining character @COMBINING CYRILLIC LETTER YU@ from the Unicode standard, defined by @'\\x2dfb'@ (&#x2022;&#x2dfb;).
  | CombiningCyrillicLetterIotifiedA  -- ^ The combining character @COMBINING CYRILLIC LETTER IOTIFIED A@ from the Unicode standard, defined by @'\\x2dfc'@ (&#x2022;&#x2dfc;).
  | CombiningCyrillicLetterLittleYus  -- ^ The combining character @COMBINING CYRILLIC LETTER LITTLE YUS@ from the Unicode standard, defined by @'\\x2dfd'@ (&#x2022;&#x2dfd;).
  | CombiningCyrillicLetterBigYus  -- ^ The combining character @COMBINING CYRILLIC LETTER BIG YUS@ from the Unicode standard, defined by @'\\x2dfe'@ (&#x2022;&#x2dfe;).
  | CombiningCyrillicLetterIotifiedBigYus  -- ^ The combining character @COMBINING CYRILLIC LETTER IOTIFIED BIG YUS@ from the Unicode standard, defined by @'\\x2dff'@ (&#x2022;&#x2dff;).
  | IdeographicLevelToneMark  -- ^ The combining character @IDEOGRAPHIC LEVEL TONE MARK@ from the Unicode standard, defined by @'\\x302a'@ (&#x2022;&#x302a;).
  | IdeographicRisingToneMark  -- ^ The combining character @IDEOGRAPHIC RISING TONE MARK@ from the Unicode standard, defined by @'\\x302b'@ (&#x2022;&#x302b;).
  | IdeographicDepartingToneMark  -- ^ The combining character @IDEOGRAPHIC DEPARTING TONE MARK@ from the Unicode standard, defined by @'\\x302c'@ (&#x2022;&#x302c;).
  | IdeographicEnteringToneMark  -- ^ The combining character @IDEOGRAPHIC ENTERING TONE MARK@ from the Unicode standard, defined by @'\\x302d'@ (&#x2022;&#x302d;).
  | HangulSingleDotToneMark  -- ^ The combining character @HANGUL SINGLE DOT TONE MARK@ from the Unicode standard, defined by @'\\x302e'@ (&#x2022;&#x302e;).
  | HangulDoubleDotToneMark  -- ^ The combining character @HANGUL DOUBLE DOT TONE MARK@ from the Unicode standard, defined by @'\\x302f'@ (&#x2022;&#x302f;).
  | CombiningKatakanaHiraganaVoicedSoundMark  -- ^ The combining character @COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK@ from the Unicode standard, defined by @'\\x3099'@ (&#x2022;&#x3099;).
  | CombiningKatakanaHiraganaSemiVoicedSoundMark  -- ^ The combining character @COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK@ from the Unicode standard, defined by @'\\x309a'@ (&#x2022;&#x309a;).
  | CombiningCyrillicVzmet  -- ^ The combining character @COMBINING CYRILLIC VZMET@ from the Unicode standard, defined by @'\\xa66f'@ (&#x2022;&#xa66f;).
  | CombiningCyrillicLetterUkrainianIe  -- ^ The combining character @COMBINING CYRILLIC LETTER UKRAINIAN IE@ from the Unicode standard, defined by @'\\xa674'@ (&#x2022;&#xa674;).
  | CombiningCyrillicLetterI  -- ^ The combining character @COMBINING CYRILLIC LETTER I@ from the Unicode standard, defined by @'\\xa675'@ (&#x2022;&#xa675;).
  | CombiningCyrillicLetterYi  -- ^ The combining character @COMBINING CYRILLIC LETTER YI@ from the Unicode standard, defined by @'\\xa676'@ (&#x2022;&#xa676;).
  | CombiningCyrillicLetterU  -- ^ The combining character @COMBINING CYRILLIC LETTER U@ from the Unicode standard, defined by @'\\xa677'@ (&#x2022;&#xa677;).
  | CombiningCyrillicLetterHardSign  -- ^ The combining character @COMBINING CYRILLIC LETTER HARD SIGN@ from the Unicode standard, defined by @'\\xa678'@ (&#x2022;&#xa678;).
  | CombiningCyrillicLetterYeru  -- ^ The combining character @COMBINING CYRILLIC LETTER YERU@ from the Unicode standard, defined by @'\\xa679'@ (&#x2022;&#xa679;).
  | CombiningCyrillicLetterSoftSign  -- ^ The combining character @COMBINING CYRILLIC LETTER SOFT SIGN@ from the Unicode standard, defined by @'\\xa67a'@ (&#x2022;&#xa67a;).
  | CombiningCyrillicLetterOmega  -- ^ The combining character @COMBINING CYRILLIC LETTER OMEGA@ from the Unicode standard, defined by @'\\xa67b'@ (&#x2022;&#xa67b;).
  | CombiningCyrillicKavyka  -- ^ The combining character @COMBINING CYRILLIC KAVYKA@ from the Unicode standard, defined by @'\\xa67c'@ (&#x2022;&#xa67c;).
  | CombiningCyrillicPayerok  -- ^ The combining character @COMBINING CYRILLIC PAYEROK@ from the Unicode standard, defined by @'\\xa67d'@ (&#x2022;&#xa67d;).
  | CombiningCyrillicLetterEf  -- ^ The combining character @COMBINING CYRILLIC LETTER EF@ from the Unicode standard, defined by @'\\xa69e'@ (&#x2022;&#xa69e;).
  | CombiningCyrillicLetterIotifiedE  -- ^ The combining character @COMBINING CYRILLIC LETTER IOTIFIED E@ from the Unicode standard, defined by @'\\xa69f'@ (&#x2022;&#xa69f;).
  | BamumCombiningMarkKoqndon  -- ^ The combining character @BAMUM COMBINING MARK KOQNDON@ from the Unicode standard, defined by @'\\xa6f0'@ (&#x2022;&#xa6f0;).
  | BamumCombiningMarkTukwentis  -- ^ The combining character @BAMUM COMBINING MARK TUKWENTIS@ from the Unicode standard, defined by @'\\xa6f1'@ (&#x2022;&#xa6f1;).
  | SylotiNagriSignHasanta  -- ^ The combining character @SYLOTI NAGRI SIGN HASANTA@ from the Unicode standard, defined by @'\\xa806'@ (&#x2022;&#xa806;).
  | SaurashtraSignVirama  -- ^ The combining character @SAURASHTRA SIGN VIRAMA@ from the Unicode standard, defined by @'\\xa8c4'@ (&#x2022;&#xa8c4;).
  | CombiningDevanagariDigitZero  -- ^ The combining character @COMBINING DEVANAGARI DIGIT ZERO@ from the Unicode standard, defined by @'\\xa8e0'@ (&#x2022;&#xa8e0;).
  | CombiningDevanagariDigitOne  -- ^ The combining character @COMBINING DEVANAGARI DIGIT ONE@ from the Unicode standard, defined by @'\\xa8e1'@ (&#x2022;&#xa8e1;).
  | CombiningDevanagariDigitTwo  -- ^ The combining character @COMBINING DEVANAGARI DIGIT TWO@ from the Unicode standard, defined by @'\\xa8e2'@ (&#x2022;&#xa8e2;).
  | CombiningDevanagariDigitThree  -- ^ The combining character @COMBINING DEVANAGARI DIGIT THREE@ from the Unicode standard, defined by @'\\xa8e3'@ (&#x2022;&#xa8e3;).
  | CombiningDevanagariDigitFour  -- ^ The combining character @COMBINING DEVANAGARI DIGIT FOUR@ from the Unicode standard, defined by @'\\xa8e4'@ (&#x2022;&#xa8e4;).
  | CombiningDevanagariDigitFive  -- ^ The combining character @COMBINING DEVANAGARI DIGIT FIVE@ from the Unicode standard, defined by @'\\xa8e5'@ (&#x2022;&#xa8e5;).
  | CombiningDevanagariDigitSix  -- ^ The combining character @COMBINING DEVANAGARI DIGIT SIX@ from the Unicode standard, defined by @'\\xa8e6'@ (&#x2022;&#xa8e6;).
  | CombiningDevanagariDigitSeven  -- ^ The combining character @COMBINING DEVANAGARI DIGIT SEVEN@ from the Unicode standard, defined by @'\\xa8e7'@ (&#x2022;&#xa8e7;).
  | CombiningDevanagariDigitEight  -- ^ The combining character @COMBINING DEVANAGARI DIGIT EIGHT@ from the Unicode standard, defined by @'\\xa8e8'@ (&#x2022;&#xa8e8;).
  | CombiningDevanagariDigitNine  -- ^ The combining character @COMBINING DEVANAGARI DIGIT NINE@ from the Unicode standard, defined by @'\\xa8e9'@ (&#x2022;&#xa8e9;).
  | CombiningDevanagariLetterA  -- ^ The combining character @COMBINING DEVANAGARI LETTER A@ from the Unicode standard, defined by @'\\xa8ea'@ (&#x2022;&#xa8ea;).
  | CombiningDevanagariLetterU  -- ^ The combining character @COMBINING DEVANAGARI LETTER U@ from the Unicode standard, defined by @'\\xa8eb'@ (&#x2022;&#xa8eb;).
  | CombiningDevanagariLetterKa  -- ^ The combining character @COMBINING DEVANAGARI LETTER KA@ from the Unicode standard, defined by @'\\xa8ec'@ (&#x2022;&#xa8ec;).
  | CombiningDevanagariLetterNa  -- ^ The combining character @COMBINING DEVANAGARI LETTER NA@ from the Unicode standard, defined by @'\\xa8ed'@ (&#x2022;&#xa8ed;).
  | CombiningDevanagariLetterPa  -- ^ The combining character @COMBINING DEVANAGARI LETTER PA@ from the Unicode standard, defined by @'\\xa8ee'@ (&#x2022;&#xa8ee;).
  | CombiningDevanagariLetterRa  -- ^ The combining character @COMBINING DEVANAGARI LETTER RA@ from the Unicode standard, defined by @'\\xa8ef'@ (&#x2022;&#xa8ef;).
  | CombiningDevanagariLetterVi  -- ^ The combining character @COMBINING DEVANAGARI LETTER VI@ from the Unicode standard, defined by @'\\xa8f0'@ (&#x2022;&#xa8f0;).
  | CombiningDevanagariSignAvagraha  -- ^ The combining character @COMBINING DEVANAGARI SIGN AVAGRAHA@ from the Unicode standard, defined by @'\\xa8f1'@ (&#x2022;&#xa8f1;).
  | KayahLiTonePlophu  -- ^ The combining character @KAYAH LI TONE PLOPHU@ from the Unicode standard, defined by @'\\xa92b'@ (&#x2022;&#xa92b;).
  | KayahLiToneCalya  -- ^ The combining character @KAYAH LI TONE CALYA@ from the Unicode standard, defined by @'\\xa92c'@ (&#x2022;&#xa92c;).
  | KayahLiToneCalyaPlophu  -- ^ The combining character @KAYAH LI TONE CALYA PLOPHU@ from the Unicode standard, defined by @'\\xa92d'@ (&#x2022;&#xa92d;).
  | RejangVirama  -- ^ The combining character @REJANG VIRAMA@ from the Unicode standard, defined by @'\\xa953'@ (&#x2022;&#xa953;).
  | JavaneseSignCecakTelu  -- ^ The combining character @JAVANESE SIGN CECAK TELU@ from the Unicode standard, defined by @'\\xa9b3'@ (&#x2022;&#xa9b3;).
  | JavanesePangkon  -- ^ The combining character @JAVANESE PANGKON@ from the Unicode standard, defined by @'\\xa9c0'@ (&#x2022;&#xa9c0;).
  | TaiVietMaiKang  -- ^ The combining character @TAI VIET MAI KANG@ from the Unicode standard, defined by @'\\xaab0'@ (&#x2022;&#xaab0;).
  | TaiVietVowelI  -- ^ The combining character @TAI VIET VOWEL I@ from the Unicode standard, defined by @'\\xaab2'@ (&#x2022;&#xaab2;).
  | TaiVietVowelUe  -- ^ The combining character @TAI VIET VOWEL UE@ from the Unicode standard, defined by @'\\xaab3'@ (&#x2022;&#xaab3;).
  | TaiVietVowelU  -- ^ The combining character @TAI VIET VOWEL U@ from the Unicode standard, defined by @'\\xaab4'@ (&#x2022;&#xaab4;).
  | TaiVietMaiKhit  -- ^ The combining character @TAI VIET MAI KHIT@ from the Unicode standard, defined by @'\\xaab7'@ (&#x2022;&#xaab7;).
  | TaiVietVowelIa  -- ^ The combining character @TAI VIET VOWEL IA@ from the Unicode standard, defined by @'\\xaab8'@ (&#x2022;&#xaab8;).
  | TaiVietVowelAm  -- ^ The combining character @TAI VIET VOWEL AM@ from the Unicode standard, defined by @'\\xaabe'@ (&#x2022;&#xaabe;).
  | TaiVietToneMaiEk  -- ^ The combining character @TAI VIET TONE MAI EK@ from the Unicode standard, defined by @'\\xaabf'@ (&#x2022;&#xaabf;).
  | TaiVietToneMaiTho  -- ^ The combining character @TAI VIET TONE MAI THO@ from the Unicode standard, defined by @'\\xaac1'@ (&#x2022;&#xaac1;).
  | MeeteiMayekVirama  -- ^ The combining character @MEETEI MAYEK VIRAMA@ from the Unicode standard, defined by @'\\xaaf6'@ (&#x2022;&#xaaf6;).
  | MeeteiMayekApunIyek  -- ^ The combining character @MEETEI MAYEK APUN IYEK@ from the Unicode standard, defined by @'\\xabed'@ (&#x2022;&#xabed;).
  | HebrewPointJudeoSpanishVarika  -- ^ The combining character @HEBREW POINT JUDEO-SPANISH VARIKA@ from the Unicode standard, defined by @'\\xfb1e'@ (&#x2022;&#xfb1e;).
  | CombiningLigatureLeftHalf  -- ^ The combining character @COMBINING LIGATURE LEFT HALF@ from the Unicode standard, defined by @'\\xfe20'@ (&#x2022;&#xfe20;).
  | CombiningLigatureRightHalf  -- ^ The combining character @COMBINING LIGATURE RIGHT HALF@ from the Unicode standard, defined by @'\\xfe21'@ (&#x2022;&#xfe21;).
  | CombiningDoubleTildeLeftHalf  -- ^ The combining character @COMBINING DOUBLE TILDE LEFT HALF@ from the Unicode standard, defined by @'\\xfe22'@ (&#x2022;&#xfe22;).
  | CombiningDoubleTildeRightHalf  -- ^ The combining character @COMBINING DOUBLE TILDE RIGHT HALF@ from the Unicode standard, defined by @'\\xfe23'@ (&#x2022;&#xfe23;).
  | CombiningMacronLeftHalf  -- ^ The combining character @COMBINING MACRON LEFT HALF@ from the Unicode standard, defined by @'\\xfe24'@ (&#x2022;&#xfe24;).
  | CombiningMacronRightHalf  -- ^ The combining character @COMBINING MACRON RIGHT HALF@ from the Unicode standard, defined by @'\\xfe25'@ (&#x2022;&#xfe25;).
  | CombiningConjoiningMacron  -- ^ The combining character @COMBINING CONJOINING MACRON@ from the Unicode standard, defined by @'\\xfe26'@ (&#x2022;&#xfe26;).
  | CombiningLigatureLeftHalfBelow  -- ^ The combining character @COMBINING LIGATURE LEFT HALF BELOW@ from the Unicode standard, defined by @'\\xfe27'@ (&#x2022;&#xfe27;).
  | CombiningLigatureRightHalfBelow  -- ^ The combining character @COMBINING LIGATURE RIGHT HALF BELOW@ from the Unicode standard, defined by @'\\xfe28'@ (&#x2022;&#xfe28;).
  | CombiningTildeLeftHalfBelow  -- ^ The combining character @COMBINING TILDE LEFT HALF BELOW@ from the Unicode standard, defined by @'\\xfe29'@ (&#x2022;&#xfe29;).
  | CombiningTildeRightHalfBelow  -- ^ The combining character @COMBINING TILDE RIGHT HALF BELOW@ from the Unicode standard, defined by @'\\xfe2a'@ (&#x2022;&#xfe2a;).
  | CombiningMacronLeftHalfBelow  -- ^ The combining character @COMBINING MACRON LEFT HALF BELOW@ from the Unicode standard, defined by @'\\xfe2b'@ (&#x2022;&#xfe2b;).
  | CombiningMacronRightHalfBelow  -- ^ The combining character @COMBINING MACRON RIGHT HALF BELOW@ from the Unicode standard, defined by @'\\xfe2c'@ (&#x2022;&#xfe2c;).
  | CombiningConjoiningMacronBelow  -- ^ The combining character @COMBINING CONJOINING MACRON BELOW@ from the Unicode standard, defined by @'\\xfe2d'@ (&#x2022;&#xfe2d;).
  | CombiningCyrillicTitloLeftHalf  -- ^ The combining character @COMBINING CYRILLIC TITLO LEFT HALF@ from the Unicode standard, defined by @'\\xfe2e'@ (&#x2022;&#xfe2e;).
  | CombiningCyrillicTitloRightHalf  -- ^ The combining character @COMBINING CYRILLIC TITLO RIGHT HALF@ from the Unicode standard, defined by @'\\xfe2f'@ (&#x2022;&#xfe2f;).
  | PhaistosDiscSignCombiningObliqueStroke  -- ^ The combining character @PHAISTOS DISC SIGN COMBINING OBLIQUE STROKE@ from the Unicode standard, defined by @'\\x101fd'@ (&#x2022;&#x101fd;).
  | CopticEpactThousandsMark  -- ^ The combining character @COPTIC EPACT THOUSANDS MARK@ from the Unicode standard, defined by @'\\x102e0'@ (&#x2022;&#x102e0;).
  | CombiningOldPermicLetterAn  -- ^ The combining character @COMBINING OLD PERMIC LETTER AN@ from the Unicode standard, defined by @'\\x10376'@ (&#x2022;&#x10376;).
  | CombiningOldPermicLetterDoi  -- ^ The combining character @COMBINING OLD PERMIC LETTER DOI@ from the Unicode standard, defined by @'\\x10377'@ (&#x2022;&#x10377;).
  | CombiningOldPermicLetterZata  -- ^ The combining character @COMBINING OLD PERMIC LETTER ZATA@ from the Unicode standard, defined by @'\\x10378'@ (&#x2022;&#x10378;).
  | CombiningOldPermicLetterNenoe  -- ^ The combining character @COMBINING OLD PERMIC LETTER NENOE@ from the Unicode standard, defined by @'\\x10379'@ (&#x2022;&#x10379;).
  | CombiningOldPermicLetterSii  -- ^ The combining character @COMBINING OLD PERMIC LETTER SII@ from the Unicode standard, defined by @'\\x1037a'@ (&#x2022;&#x1037a;).
  | KharoshthiSignDoubleRingBelow  -- ^ The combining character @KHAROSHTHI SIGN DOUBLE RING BELOW@ from the Unicode standard, defined by @'\\x10a0d'@ (&#x2022;&#x10a0d;).
  | KharoshthiSignVisarga  -- ^ The combining character @KHAROSHTHI SIGN VISARGA@ from the Unicode standard, defined by @'\\x10a0f'@ (&#x2022;&#x10a0f;).
  | KharoshthiSignBarAbove  -- ^ The combining character @KHAROSHTHI SIGN BAR ABOVE@ from the Unicode standard, defined by @'\\x10a38'@ (&#x2022;&#x10a38;).
  | KharoshthiSignCauda  -- ^ The combining character @KHAROSHTHI SIGN CAUDA@ from the Unicode standard, defined by @'\\x10a39'@ (&#x2022;&#x10a39;).
  | KharoshthiSignDotBelow  -- ^ The combining character @KHAROSHTHI SIGN DOT BELOW@ from the Unicode standard, defined by @'\\x10a3a'@ (&#x2022;&#x10a3a;).
  | KharoshthiVirama  -- ^ The combining character @KHAROSHTHI VIRAMA@ from the Unicode standard, defined by @'\\x10a3f'@ (&#x2022;&#x10a3f;).
  | ManichaeanAbbreviationMarkAbove  -- ^ The combining character @MANICHAEAN ABBREVIATION MARK ABOVE@ from the Unicode standard, defined by @'\\x10ae5'@ (&#x2022;&#x10ae5;).
  | ManichaeanAbbreviationMarkBelow  -- ^ The combining character @MANICHAEAN ABBREVIATION MARK BELOW@ from the Unicode standard, defined by @'\\x10ae6'@ (&#x2022;&#x10ae6;).
  | BrahmiVirama  -- ^ The combining character @BRAHMI VIRAMA@ from the Unicode standard, defined by @'\\x11046'@ (&#x2022;&#x11046;).
  | BrahmiNumberJoiner  -- ^ The combining character @BRAHMI NUMBER JOINER@ from the Unicode standard, defined by @'\\x1107f'@ (&#x2022;&#x1107f;).
  | KaithiSignVirama  -- ^ The combining character @KAITHI SIGN VIRAMA@ from the Unicode standard, defined by @'\\x110b9'@ (&#x2022;&#x110b9;).
  | KaithiSignNukta  -- ^ The combining character @KAITHI SIGN NUKTA@ from the Unicode standard, defined by @'\\x110ba'@ (&#x2022;&#x110ba;).
  | ChakmaSignCandrabindu  -- ^ The combining character @CHAKMA SIGN CANDRABINDU@ from the Unicode standard, defined by @'\\x11100'@ (&#x2022;&#x11100;).
  | ChakmaSignAnusvara  -- ^ The combining character @CHAKMA SIGN ANUSVARA@ from the Unicode standard, defined by @'\\x11101'@ (&#x2022;&#x11101;).
  | ChakmaSignVisarga  -- ^ The combining character @CHAKMA SIGN VISARGA@ from the Unicode standard, defined by @'\\x11102'@ (&#x2022;&#x11102;).
  | ChakmaVowelSignA  -- ^ The combining character @CHAKMA VOWEL SIGN A@ from the Unicode standard, defined by @'\\x11127'@ (&#x2022;&#x11127;).
  | ChakmaVirama  -- ^ The combining character @CHAKMA VIRAMA@ from the Unicode standard, defined by @'\\x11133'@ (&#x2022;&#x11133;).
  | ChakmaMaayyaa  -- ^ The combining character @CHAKMA MAAYYAA@ from the Unicode standard, defined by @'\\x11134'@ (&#x2022;&#x11134;).
  | MahajaniSignNukta  -- ^ The combining character @MAHAJANI SIGN NUKTA@ from the Unicode standard, defined by @'\\x11173'@ (&#x2022;&#x11173;).
  | SharadaSignVirama  -- ^ The combining character @SHARADA SIGN VIRAMA@ from the Unicode standard, defined by @'\\x111c0'@ (&#x2022;&#x111c0;).
  | SharadaSignNukta  -- ^ The combining character @SHARADA SIGN NUKTA@ from the Unicode standard, defined by @'\\x111ca'@ (&#x2022;&#x111ca;).
  | KhojkiSignVirama  -- ^ The combining character @KHOJKI SIGN VIRAMA@ from the Unicode standard, defined by @'\\x11235'@ (&#x2022;&#x11235;).
  | KhojkiSignNukta  -- ^ The combining character @KHOJKI SIGN NUKTA@ from the Unicode standard, defined by @'\\x11236'@ (&#x2022;&#x11236;).
  | KhudawadiSignNukta  -- ^ The combining character @KHUDAWADI SIGN NUKTA@ from the Unicode standard, defined by @'\\x112e9'@ (&#x2022;&#x112e9;).
  | KhudawadiSignVirama  -- ^ The combining character @KHUDAWADI SIGN VIRAMA@ from the Unicode standard, defined by @'\\x112ea'@ (&#x2022;&#x112ea;).
  | GranthaSignNukta  -- ^ The combining character @GRANTHA SIGN NUKTA@ from the Unicode standard, defined by @'\\x1133c'@ (&#x2022;&#x1133c;).
  | GranthaVowelSignAa  -- ^ The combining character @GRANTHA VOWEL SIGN AA@ from the Unicode standard, defined by @'\\x1133e'@ (&#x2022;&#x1133e;).
  | GranthaSignVirama  -- ^ The combining character @GRANTHA SIGN VIRAMA@ from the Unicode standard, defined by @'\\x1134d'@ (&#x2022;&#x1134d;).
  | GranthaAuLengthMark  -- ^ The combining character @GRANTHA AU LENGTH MARK@ from the Unicode standard, defined by @'\\x11357'@ (&#x2022;&#x11357;).
  | CombiningGranthaDigitZero  -- ^ The combining character @COMBINING GRANTHA DIGIT ZERO@ from the Unicode standard, defined by @'\\x11366'@ (&#x2022;&#x11366;).
  | CombiningGranthaDigitOne  -- ^ The combining character @COMBINING GRANTHA DIGIT ONE@ from the Unicode standard, defined by @'\\x11367'@ (&#x2022;&#x11367;).
  | CombiningGranthaDigitTwo  -- ^ The combining character @COMBINING GRANTHA DIGIT TWO@ from the Unicode standard, defined by @'\\x11368'@ (&#x2022;&#x11368;).
  | CombiningGranthaDigitThree  -- ^ The combining character @COMBINING GRANTHA DIGIT THREE@ from the Unicode standard, defined by @'\\x11369'@ (&#x2022;&#x11369;).
  | CombiningGranthaDigitFour  -- ^ The combining character @COMBINING GRANTHA DIGIT FOUR@ from the Unicode standard, defined by @'\\x1136a'@ (&#x2022;&#x1136a;).
  | CombiningGranthaDigitFive  -- ^ The combining character @COMBINING GRANTHA DIGIT FIVE@ from the Unicode standard, defined by @'\\x1136b'@ (&#x2022;&#x1136b;).
  | CombiningGranthaDigitSix  -- ^ The combining character @COMBINING GRANTHA DIGIT SIX@ from the Unicode standard, defined by @'\\x1136c'@ (&#x2022;&#x1136c;).
  | CombiningGranthaLetterA  -- ^ The combining character @COMBINING GRANTHA LETTER A@ from the Unicode standard, defined by @'\\x11370'@ (&#x2022;&#x11370;).
  | CombiningGranthaLetterKa  -- ^ The combining character @COMBINING GRANTHA LETTER KA@ from the Unicode standard, defined by @'\\x11371'@ (&#x2022;&#x11371;).
  | CombiningGranthaLetterNa  -- ^ The combining character @COMBINING GRANTHA LETTER NA@ from the Unicode standard, defined by @'\\x11372'@ (&#x2022;&#x11372;).
  | CombiningGranthaLetterVi  -- ^ The combining character @COMBINING GRANTHA LETTER VI@ from the Unicode standard, defined by @'\\x11373'@ (&#x2022;&#x11373;).
  | CombiningGranthaLetterPa  -- ^ The combining character @COMBINING GRANTHA LETTER PA@ from the Unicode standard, defined by @'\\x11374'@ (&#x2022;&#x11374;).
  | NewaSignVirama  -- ^ The combining character @NEWA SIGN VIRAMA@ from the Unicode standard, defined by @'\\x11442'@ (&#x2022;&#x11442;).
  | NewaSignNukta  -- ^ The combining character @NEWA SIGN NUKTA@ from the Unicode standard, defined by @'\\x11446'@ (&#x2022;&#x11446;).
  | TirhutaVowelSignAa  -- ^ The combining character @TIRHUTA VOWEL SIGN AA@ from the Unicode standard, defined by @'\\x114b0'@ (&#x2022;&#x114b0;).
  | TirhutaVowelSignShortE  -- ^ The combining character @TIRHUTA VOWEL SIGN SHORT E@ from the Unicode standard, defined by @'\\x114ba'@ (&#x2022;&#x114ba;).
  | TirhutaVowelSignShortO  -- ^ The combining character @TIRHUTA VOWEL SIGN SHORT O@ from the Unicode standard, defined by @'\\x114bd'@ (&#x2022;&#x114bd;).
  | TirhutaSignVirama  -- ^ The combining character @TIRHUTA SIGN VIRAMA@ from the Unicode standard, defined by @'\\x114c2'@ (&#x2022;&#x114c2;).
  | TirhutaSignNukta  -- ^ The combining character @TIRHUTA SIGN NUKTA@ from the Unicode standard, defined by @'\\x114c3'@ (&#x2022;&#x114c3;).
  | SiddhamVowelSignAa  -- ^ The combining character @SIDDHAM VOWEL SIGN AA@ from the Unicode standard, defined by @'\\x115af'@ (&#x2022;&#x115af;).
  | SiddhamSignVirama  -- ^ The combining character @SIDDHAM SIGN VIRAMA@ from the Unicode standard, defined by @'\\x115bf'@ (&#x2022;&#x115bf;).
  | SiddhamSignNukta  -- ^ The combining character @SIDDHAM SIGN NUKTA@ from the Unicode standard, defined by @'\\x115c0'@ (&#x2022;&#x115c0;).
  | ModiSignVirama  -- ^ The combining character @MODI SIGN VIRAMA@ from the Unicode standard, defined by @'\\x1163f'@ (&#x2022;&#x1163f;).
  | TakriSignVirama  -- ^ The combining character @TAKRI SIGN VIRAMA@ from the Unicode standard, defined by @'\\x116b6'@ (&#x2022;&#x116b6;).
  | TakriSignNukta  -- ^ The combining character @TAKRI SIGN NUKTA@ from the Unicode standard, defined by @'\\x116b7'@ (&#x2022;&#x116b7;).
  | AhomSignKiller  -- ^ The combining character @AHOM SIGN KILLER@ from the Unicode standard, defined by @'\\x1172b'@ (&#x2022;&#x1172b;).
  | BhaiksukiSignVirama  -- ^ The combining character @BHAIKSUKI SIGN VIRAMA@ from the Unicode standard, defined by @'\\x11c3f'@ (&#x2022;&#x11c3f;).
  | BassaVahCombiningHighTone  -- ^ The combining character @BASSA VAH COMBINING HIGH TONE@ from the Unicode standard, defined by @'\\x16af0'@ (&#x2022;&#x16af0;).
  | BassaVahCombiningLowTone  -- ^ The combining character @BASSA VAH COMBINING LOW TONE@ from the Unicode standard, defined by @'\\x16af1'@ (&#x2022;&#x16af1;).
  | BassaVahCombiningMidTone  -- ^ The combining character @BASSA VAH COMBINING MID TONE@ from the Unicode standard, defined by @'\\x16af2'@ (&#x2022;&#x16af2;).
  | BassaVahCombiningLowMidTone  -- ^ The combining character @BASSA VAH COMBINING LOW-MID TONE@ from the Unicode standard, defined by @'\\x16af3'@ (&#x2022;&#x16af3;).
  | BassaVahCombiningHighLowTone  -- ^ The combining character @BASSA VAH COMBINING HIGH-LOW TONE@ from the Unicode standard, defined by @'\\x16af4'@ (&#x2022;&#x16af4;).
  | PahawhHmongMarkCimTub  -- ^ The combining character @PAHAWH HMONG MARK CIM TUB@ from the Unicode standard, defined by @'\\x16b30'@ (&#x2022;&#x16b30;).
  | PahawhHmongMarkCimSo  -- ^ The combining character @PAHAWH HMONG MARK CIM SO@ from the Unicode standard, defined by @'\\x16b31'@ (&#x2022;&#x16b31;).
  | PahawhHmongMarkCimKes  -- ^ The combining character @PAHAWH HMONG MARK CIM KES@ from the Unicode standard, defined by @'\\x16b32'@ (&#x2022;&#x16b32;).
  | PahawhHmongMarkCimKhav  -- ^ The combining character @PAHAWH HMONG MARK CIM KHAV@ from the Unicode standard, defined by @'\\x16b33'@ (&#x2022;&#x16b33;).
  | PahawhHmongMarkCimSuam  -- ^ The combining character @PAHAWH HMONG MARK CIM SUAM@ from the Unicode standard, defined by @'\\x16b34'@ (&#x2022;&#x16b34;).
  | PahawhHmongMarkCimHom  -- ^ The combining character @PAHAWH HMONG MARK CIM HOM@ from the Unicode standard, defined by @'\\x16b35'@ (&#x2022;&#x16b35;).
  | PahawhHmongMarkCimTaum  -- ^ The combining character @PAHAWH HMONG MARK CIM TAUM@ from the Unicode standard, defined by @'\\x16b36'@ (&#x2022;&#x16b36;).
  | DuployanDoubleMark  -- ^ The combining character @DUPLOYAN DOUBLE MARK@ from the Unicode standard, defined by @'\\x1bc9e'@ (&#x2022;&#x1bc9e;).
  | MusicalSymbolCombiningStem  -- ^ The combining character @MUSICAL SYMBOL COMBINING STEM@ from the Unicode standard, defined by @'\\x1d165'@ (&#x2022;&#x1d165;).
  | MusicalSymbolCombiningSprechgesangStem  -- ^ The combining character @MUSICAL SYMBOL COMBINING SPRECHGESANG STEM@ from the Unicode standard, defined by @'\\x1d166'@ (&#x2022;&#x1d166;).
  | MusicalSymbolCombiningTremolo1  -- ^ The combining character @MUSICAL SYMBOL COMBINING TREMOLO-1@ from the Unicode standard, defined by @'\\x1d167'@ (&#x2022;&#x1d167;).
  | MusicalSymbolCombiningTremolo2  -- ^ The combining character @MUSICAL SYMBOL COMBINING TREMOLO-2@ from the Unicode standard, defined by @'\\x1d168'@ (&#x2022;&#x1d168;).
  | MusicalSymbolCombiningTremolo3  -- ^ The combining character @MUSICAL SYMBOL COMBINING TREMOLO-3@ from the Unicode standard, defined by @'\\x1d169'@ (&#x2022;&#x1d169;).
  | MusicalSymbolCombiningAugmentationDot  -- ^ The combining character @MUSICAL SYMBOL COMBINING AUGMENTATION DOT@ from the Unicode standard, defined by @'\\x1d16d'@ (&#x2022;&#x1d16d;).
  | MusicalSymbolCombiningFlag1  -- ^ The combining character @MUSICAL SYMBOL COMBINING FLAG-1@ from the Unicode standard, defined by @'\\x1d16e'@ (&#x2022;&#x1d16e;).
  | MusicalSymbolCombiningFlag2  -- ^ The combining character @MUSICAL SYMBOL COMBINING FLAG-2@ from the Unicode standard, defined by @'\\x1d16f'@ (&#x2022;&#x1d16f;).
  | MusicalSymbolCombiningFlag3  -- ^ The combining character @MUSICAL SYMBOL COMBINING FLAG-3@ from the Unicode standard, defined by @'\\x1d170'@ (&#x2022;&#x1d170;).
  | MusicalSymbolCombiningFlag4  -- ^ The combining character @MUSICAL SYMBOL COMBINING FLAG-4@ from the Unicode standard, defined by @'\\x1d171'@ (&#x2022;&#x1d171;).
  | MusicalSymbolCombiningFlag5  -- ^ The combining character @MUSICAL SYMBOL COMBINING FLAG-5@ from the Unicode standard, defined by @'\\x1d172'@ (&#x2022;&#x1d172;).
  | MusicalSymbolCombiningAccent  -- ^ The combining character @MUSICAL SYMBOL COMBINING ACCENT@ from the Unicode standard, defined by @'\\x1d17b'@ (&#x2022;&#x1d17b;).
  | MusicalSymbolCombiningStaccato  -- ^ The combining character @MUSICAL SYMBOL COMBINING STACCATO@ from the Unicode standard, defined by @'\\x1d17c'@ (&#x2022;&#x1d17c;).
  | MusicalSymbolCombiningTenuto  -- ^ The combining character @MUSICAL SYMBOL COMBINING TENUTO@ from the Unicode standard, defined by @'\\x1d17d'@ (&#x2022;&#x1d17d;).
  | MusicalSymbolCombiningStaccatissimo  -- ^ The combining character @MUSICAL SYMBOL COMBINING STACCATISSIMO@ from the Unicode standard, defined by @'\\x1d17e'@ (&#x2022;&#x1d17e;).
  | MusicalSymbolCombiningMarcato  -- ^ The combining character @MUSICAL SYMBOL COMBINING MARCATO@ from the Unicode standard, defined by @'\\x1d17f'@ (&#x2022;&#x1d17f;).
  | MusicalSymbolCombiningMarcatoStaccato  -- ^ The combining character @MUSICAL SYMBOL COMBINING MARCATO-STACCATO@ from the Unicode standard, defined by @'\\x1d180'@ (&#x2022;&#x1d180;).
  | MusicalSymbolCombiningAccentStaccato  -- ^ The combining character @MUSICAL SYMBOL COMBINING ACCENT-STACCATO@ from the Unicode standard, defined by @'\\x1d181'@ (&#x2022;&#x1d181;).
  | MusicalSymbolCombiningLoure  -- ^ The combining character @MUSICAL SYMBOL COMBINING LOURE@ from the Unicode standard, defined by @'\\x1d182'@ (&#x2022;&#x1d182;).
  | MusicalSymbolCombiningDoit  -- ^ The combining character @MUSICAL SYMBOL COMBINING DOIT@ from the Unicode standard, defined by @'\\x1d185'@ (&#x2022;&#x1d185;).
  | MusicalSymbolCombiningRip  -- ^ The combining character @MUSICAL SYMBOL COMBINING RIP@ from the Unicode standard, defined by @'\\x1d186'@ (&#x2022;&#x1d186;).
  | MusicalSymbolCombiningFlip  -- ^ The combining character @MUSICAL SYMBOL COMBINING FLIP@ from the Unicode standard, defined by @'\\x1d187'@ (&#x2022;&#x1d187;).
  | MusicalSymbolCombiningSmear  -- ^ The combining character @MUSICAL SYMBOL COMBINING SMEAR@ from the Unicode standard, defined by @'\\x1d188'@ (&#x2022;&#x1d188;).
  | MusicalSymbolCombiningBend  -- ^ The combining character @MUSICAL SYMBOL COMBINING BEND@ from the Unicode standard, defined by @'\\x1d189'@ (&#x2022;&#x1d189;).
  | MusicalSymbolCombiningDoubleTongue  -- ^ The combining character @MUSICAL SYMBOL COMBINING DOUBLE TONGUE@ from the Unicode standard, defined by @'\\x1d18a'@ (&#x2022;&#x1d18a;).
  | MusicalSymbolCombiningTripleTongue  -- ^ The combining character @MUSICAL SYMBOL COMBINING TRIPLE TONGUE@ from the Unicode standard, defined by @'\\x1d18b'@ (&#x2022;&#x1d18b;).
  | MusicalSymbolCombiningDownBow  -- ^ The combining character @MUSICAL SYMBOL COMBINING DOWN BOW@ from the Unicode standard, defined by @'\\x1d1aa'@ (&#x2022;&#x1d1aa;).
  | MusicalSymbolCombiningUpBow  -- ^ The combining character @MUSICAL SYMBOL COMBINING UP BOW@ from the Unicode standard, defined by @'\\x1d1ab'@ (&#x2022;&#x1d1ab;).
  | MusicalSymbolCombiningHarmonic  -- ^ The combining character @MUSICAL SYMBOL COMBINING HARMONIC@ from the Unicode standard, defined by @'\\x1d1ac'@ (&#x2022;&#x1d1ac;).
  | MusicalSymbolCombiningSnapPizzicato  -- ^ The combining character @MUSICAL SYMBOL COMBINING SNAP PIZZICATO@ from the Unicode standard, defined by @'\\x1d1ad'@ (&#x2022;&#x1d1ad;).
  | CombiningGreekMusicalTriseme  -- ^ The combining character @COMBINING GREEK MUSICAL TRISEME@ from the Unicode standard, defined by @'\\x1d242'@ (&#x2022;&#x1d242;).
  | CombiningGreekMusicalTetraseme  -- ^ The combining character @COMBINING GREEK MUSICAL TETRASEME@ from the Unicode standard, defined by @'\\x1d243'@ (&#x2022;&#x1d243;).
  | CombiningGreekMusicalPentaseme  -- ^ The combining character @COMBINING GREEK MUSICAL PENTASEME@ from the Unicode standard, defined by @'\\x1d244'@ (&#x2022;&#x1d244;).
  | CombiningGlagoliticLetterAzu  -- ^ The combining character @COMBINING GLAGOLITIC LETTER AZU@ from the Unicode standard, defined by @'\\x1e000'@ (&#x2022;&#x1e000;).
  | CombiningGlagoliticLetterBuky  -- ^ The combining character @COMBINING GLAGOLITIC LETTER BUKY@ from the Unicode standard, defined by @'\\x1e001'@ (&#x2022;&#x1e001;).
  | CombiningGlagoliticLetterVede  -- ^ The combining character @COMBINING GLAGOLITIC LETTER VEDE@ from the Unicode standard, defined by @'\\x1e002'@ (&#x2022;&#x1e002;).
  | CombiningGlagoliticLetterGlagoli  -- ^ The combining character @COMBINING GLAGOLITIC LETTER GLAGOLI@ from the Unicode standard, defined by @'\\x1e003'@ (&#x2022;&#x1e003;).
  | CombiningGlagoliticLetterDobro  -- ^ The combining character @COMBINING GLAGOLITIC LETTER DOBRO@ from the Unicode standard, defined by @'\\x1e004'@ (&#x2022;&#x1e004;).
  | CombiningGlagoliticLetterYestu  -- ^ The combining character @COMBINING GLAGOLITIC LETTER YESTU@ from the Unicode standard, defined by @'\\x1e005'@ (&#x2022;&#x1e005;).
  | CombiningGlagoliticLetterZhivete  -- ^ The combining character @COMBINING GLAGOLITIC LETTER ZHIVETE@ from the Unicode standard, defined by @'\\x1e006'@ (&#x2022;&#x1e006;).
  | CombiningGlagoliticLetterZemlja  -- ^ The combining character @COMBINING GLAGOLITIC LETTER ZEMLJA@ from the Unicode standard, defined by @'\\x1e008'@ (&#x2022;&#x1e008;).
  | CombiningGlagoliticLetterIzhe  -- ^ The combining character @COMBINING GLAGOLITIC LETTER IZHE@ from the Unicode standard, defined by @'\\x1e009'@ (&#x2022;&#x1e009;).
  | CombiningGlagoliticLetterInitialIzhe  -- ^ The combining character @COMBINING GLAGOLITIC LETTER INITIAL IZHE@ from the Unicode standard, defined by @'\\x1e00a'@ (&#x2022;&#x1e00a;).
  | CombiningGlagoliticLetterI  -- ^ The combining character @COMBINING GLAGOLITIC LETTER I@ from the Unicode standard, defined by @'\\x1e00b'@ (&#x2022;&#x1e00b;).
  | CombiningGlagoliticLetterDjervi  -- ^ The combining character @COMBINING GLAGOLITIC LETTER DJERVI@ from the Unicode standard, defined by @'\\x1e00c'@ (&#x2022;&#x1e00c;).
  | CombiningGlagoliticLetterKako  -- ^ The combining character @COMBINING GLAGOLITIC LETTER KAKO@ from the Unicode standard, defined by @'\\x1e00d'@ (&#x2022;&#x1e00d;).
  | CombiningGlagoliticLetterLjudije  -- ^ The combining character @COMBINING GLAGOLITIC LETTER LJUDIJE@ from the Unicode standard, defined by @'\\x1e00e'@ (&#x2022;&#x1e00e;).
  | CombiningGlagoliticLetterMyslite  -- ^ The combining character @COMBINING GLAGOLITIC LETTER MYSLITE@ from the Unicode standard, defined by @'\\x1e00f'@ (&#x2022;&#x1e00f;).
  | CombiningGlagoliticLetterNashi  -- ^ The combining character @COMBINING GLAGOLITIC LETTER NASHI@ from the Unicode standard, defined by @'\\x1e010'@ (&#x2022;&#x1e010;).
  | CombiningGlagoliticLetterOnu  -- ^ The combining character @COMBINING GLAGOLITIC LETTER ONU@ from the Unicode standard, defined by @'\\x1e011'@ (&#x2022;&#x1e011;).
  | CombiningGlagoliticLetterPokoji  -- ^ The combining character @COMBINING GLAGOLITIC LETTER POKOJI@ from the Unicode standard, defined by @'\\x1e012'@ (&#x2022;&#x1e012;).
  | CombiningGlagoliticLetterRitsi  -- ^ The combining character @COMBINING GLAGOLITIC LETTER RITSI@ from the Unicode standard, defined by @'\\x1e013'@ (&#x2022;&#x1e013;).
  | CombiningGlagoliticLetterSlovo  -- ^ The combining character @COMBINING GLAGOLITIC LETTER SLOVO@ from the Unicode standard, defined by @'\\x1e014'@ (&#x2022;&#x1e014;).
  | CombiningGlagoliticLetterTvrido  -- ^ The combining character @COMBINING GLAGOLITIC LETTER TVRIDO@ from the Unicode standard, defined by @'\\x1e015'@ (&#x2022;&#x1e015;).
  | CombiningGlagoliticLetterUku  -- ^ The combining character @COMBINING GLAGOLITIC LETTER UKU@ from the Unicode standard, defined by @'\\x1e016'@ (&#x2022;&#x1e016;).
  | CombiningGlagoliticLetterFritu  -- ^ The combining character @COMBINING GLAGOLITIC LETTER FRITU@ from the Unicode standard, defined by @'\\x1e017'@ (&#x2022;&#x1e017;).
  | CombiningGlagoliticLetterHeru  -- ^ The combining character @COMBINING GLAGOLITIC LETTER HERU@ from the Unicode standard, defined by @'\\x1e018'@ (&#x2022;&#x1e018;).
  | CombiningGlagoliticLetterShta  -- ^ The combining character @COMBINING GLAGOLITIC LETTER SHTA@ from the Unicode standard, defined by @'\\x1e01b'@ (&#x2022;&#x1e01b;).
  | CombiningGlagoliticLetterTsi  -- ^ The combining character @COMBINING GLAGOLITIC LETTER TSI@ from the Unicode standard, defined by @'\\x1e01c'@ (&#x2022;&#x1e01c;).
  | CombiningGlagoliticLetterChrivi  -- ^ The combining character @COMBINING GLAGOLITIC LETTER CHRIVI@ from the Unicode standard, defined by @'\\x1e01d'@ (&#x2022;&#x1e01d;).
  | CombiningGlagoliticLetterSha  -- ^ The combining character @COMBINING GLAGOLITIC LETTER SHA@ from the Unicode standard, defined by @'\\x1e01e'@ (&#x2022;&#x1e01e;).
  | CombiningGlagoliticLetterYeru  -- ^ The combining character @COMBINING GLAGOLITIC LETTER YERU@ from the Unicode standard, defined by @'\\x1e01f'@ (&#x2022;&#x1e01f;).
  | CombiningGlagoliticLetterYeri  -- ^ The combining character @COMBINING GLAGOLITIC LETTER YERI@ from the Unicode standard, defined by @'\\x1e020'@ (&#x2022;&#x1e020;).
  | CombiningGlagoliticLetterYati  -- ^ The combining character @COMBINING GLAGOLITIC LETTER YATI@ from the Unicode standard, defined by @'\\x1e021'@ (&#x2022;&#x1e021;).
  | CombiningGlagoliticLetterYu  -- ^ The combining character @COMBINING GLAGOLITIC LETTER YU@ from the Unicode standard, defined by @'\\x1e023'@ (&#x2022;&#x1e023;).
  | CombiningGlagoliticLetterSmallYus  -- ^ The combining character @COMBINING GLAGOLITIC LETTER SMALL YUS@ from the Unicode standard, defined by @'\\x1e024'@ (&#x2022;&#x1e024;).
  | CombiningGlagoliticLetterYo  -- ^ The combining character @COMBINING GLAGOLITIC LETTER YO@ from the Unicode standard, defined by @'\\x1e026'@ (&#x2022;&#x1e026;).
  | CombiningGlagoliticLetterIotatedSmallYus  -- ^ The combining character @COMBINING GLAGOLITIC LETTER IOTATED SMALL YUS@ from the Unicode standard, defined by @'\\x1e027'@ (&#x2022;&#x1e027;).
  | CombiningGlagoliticLetterBigYus  -- ^ The combining character @COMBINING GLAGOLITIC LETTER BIG YUS@ from the Unicode standard, defined by @'\\x1e028'@ (&#x2022;&#x1e028;).
  | CombiningGlagoliticLetterIotatedBigYus  -- ^ The combining character @COMBINING GLAGOLITIC LETTER IOTATED BIG YUS@ from the Unicode standard, defined by @'\\x1e029'@ (&#x2022;&#x1e029;).
  | CombiningGlagoliticLetterFita  -- ^ The combining character @COMBINING GLAGOLITIC LETTER FITA@ from the Unicode standard, defined by @'\\x1e02a'@ (&#x2022;&#x1e02a;).
  | MendeKikakuiCombiningNumberTeens  -- ^ The combining character @MENDE KIKAKUI COMBINING NUMBER TEENS@ from the Unicode standard, defined by @'\\x1e8d0'@ (&#x2022;&#x1e8d0;).
  | MendeKikakuiCombiningNumberTens  -- ^ The combining character @MENDE KIKAKUI COMBINING NUMBER TENS@ from the Unicode standard, defined by @'\\x1e8d1'@ (&#x2022;&#x1e8d1;).
  | MendeKikakuiCombiningNumberHundreds  -- ^ The combining character @MENDE KIKAKUI COMBINING NUMBER HUNDREDS@ from the Unicode standard, defined by @'\\x1e8d2'@ (&#x2022;&#x1e8d2;).
  | MendeKikakuiCombiningNumberThousands  -- ^ The combining character @MENDE KIKAKUI COMBINING NUMBER THOUSANDS@ from the Unicode standard, defined by @'\\x1e8d3'@ (&#x2022;&#x1e8d3;).
  | MendeKikakuiCombiningNumberTenThousands  -- ^ The combining character @MENDE KIKAKUI COMBINING NUMBER TEN THOUSANDS@ from the Unicode standard, defined by @'\\x1e8d4'@ (&#x2022;&#x1e8d4;).
  | MendeKikakuiCombiningNumberHundredThousands  -- ^ The combining character @MENDE KIKAKUI COMBINING NUMBER HUNDRED THOUSANDS@ from the Unicode standard, defined by @'\\x1e8d5'@ (&#x2022;&#x1e8d5;).
  | MendeKikakuiCombiningNumberMillions  -- ^ The combining character @MENDE KIKAKUI COMBINING NUMBER MILLIONS@ from the Unicode standard, defined by @'\\x1e8d6'@ (&#x2022;&#x1e8d6;).
  | AdlamAlifLengthener  -- ^ The combining character @ADLAM ALIF LENGTHENER@ from the Unicode standard, defined by @'\\x1e944'@ (&#x2022;&#x1e944;).
  | AdlamVowelLengthener  -- ^ The combining character @ADLAM VOWEL LENGTHENER@ from the Unicode standard, defined by @'\\x1e945'@ (&#x2022;&#x1e945;).
  | AdlamGeminationMark  -- ^ The combining character @ADLAM GEMINATION MARK@ from the Unicode standard, defined by @'\\x1e946'@ (&#x2022;&#x1e946;).
  | AdlamHamza  -- ^ The combining character @ADLAM HAMZA@ from the Unicode standard, defined by @'\\x1e947'@ (&#x2022;&#x1e947;).
  | AdlamConsonantModifier  -- ^ The combining character @ADLAM CONSONANT MODIFIER@ from the Unicode standard, defined by @'\\x1e948'@ (&#x2022;&#x1e948;).
  | AdlamGeminateConsonantModifier  -- ^ The combining character @ADLAM GEMINATE CONSONANT MODIFIER@ from the Unicode standard, defined by @'\\x1e949'@ (&#x2022;&#x1e949;).
  | AdlamNukta  -- ^ The combining character @ADLAM NUKTA@ from the Unicode standard, defined by @'\\x1e94a'@ (&#x2022;&#x1e94a;).
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | A type synonym to make working with 'CombiningCharacter' more convenient.
type CombiningChar = CombiningCharacter

instance IsString CombiningCharacter where
    fromString [x] = combiningCharacter' x
    fromString _ = error "The given string should contain exactly one codepoint"

-- | A set of 'CombiningCharacter's that can then all be applied to the same
-- character. The '(*^)' is used both to "stack" characters in a
-- 'CombiningSequence', and to apply a 'CombiningCharacter' or a
-- 'CombiningSequence' to a 'Char'.
newtype CombiningSequence = CombiningSequence (NonEmpty CombiningCharacter) deriving (Eq, Ord, Read, Show)

instance IsString CombiningSequence where
    fromString (c:cs)
        | Just y <- traverse combiningCharacter (c :| cs) = CombiningSequence y
    fromString _ = error "The given string should contain at least one character, and all should be combining codepoints."

instance IsString [CombiningCharacter] where
    fromString = map combiningCharacter'

-- | A typeclass used to apply a 'CombiningCharacter' or a 'CombiningSequence'
-- to a 'Char', and return a 'Text' object.
class ApplyCombine a b c | a b -> c where
    -- | Applies the given 'CombiningCharacter' or 'CombiningSequence' to the
    -- given character. The operator is right-to-left, to allow "stacking" of
    -- 'CombiningCharacter's for example:
    --
    --  > 'a' *^ CombiningGraveAccent *^ CombiningPlusSignBelow
    (*^) :: a -> b -> c
    -- | Applies the given 'CombiningCharacter' or 'CombiningSequence' to the
    -- given character, and use composition characters in case that is possible.
    -- The operator is right-to-left, to allow "stacking" of
    -- 'CombiningCharacter's for example:
    --
    --  > 'a' *^! CombiningGraveAccent *^! CombiningPlusSignBelow
    (*^!) :: a -> b -> c
    (*^!) = (*^)

instance ApplyCombine CombiningCharacter CombiningCharacter CombiningSequence where
    (*^) c c2 = CombiningSequence (c :| [c2])

instance ApplyCombine CombiningCharacter CombiningSequence CombiningSequence where
    (*^) c (CombiningSequence cs) = CombiningSequence (c <| cs)

instance ApplyCombine CombiningCharacter [CombiningCharacter] [CombiningCharacter] where
    (*^) = (:)

instance ApplyCombine Char CombiningCharacter Text where
    (*^) c c2 = cons c (singleton (combiningToUnicode c2))
    (*^!) c d = let (y, ys) = composeCombiningSequence c [d] in pack (y:map combiningToUnicode ys)

instance ApplyCombine Char [CombiningCharacter] Text where
    (*^) c = pack . (c:) . map combiningToUnicode
    (*^!) c ds = let (y, ys) = composeCombiningSequence c ds in pack (y:map combiningToUnicode ys)

instance ApplyCombine Char CombiningSequence Text where
    (*^) c (CombiningSequence cs) = cons c (pack (map combiningToUnicode (toList cs)))
    (*^!) c (CombiningSequence (d :| ds)) = let (y, ys) = composeCombiningSequence c (d:ds) in pack (y:map combiningToUnicode ys)

-- | Convert the given 'CombiningCharacter' to a 'Char' in Unicode, this
-- codepoints need a preceding codepoint to be applied to.
combiningToUnicode
  :: CombiningCharacter -- ^ The given 'CombiningCharacter' to convert to a Unicode codepoint.
  -> Char -- ^ A Unicode 'Char'acter that is the codepoint of the given 'CombiningCharacter'. The
combiningToUnicode CombiningGraveAccent = '\x0300'
combiningToUnicode CombiningAcuteAccent = '\x0301'
combiningToUnicode CombiningCircumflexAccent = '\x0302'
combiningToUnicode CombiningTilde = '\x0303'
combiningToUnicode CombiningMacron = '\x0304'
combiningToUnicode CombiningOverline = '\x0305'
combiningToUnicode CombiningBreve = '\x0306'
combiningToUnicode CombiningDotAbove = '\x0307'
combiningToUnicode CombiningDiaeresis = '\x0308'
combiningToUnicode CombiningHookAbove = '\x0309'
combiningToUnicode CombiningRingAbove = '\x030a'
combiningToUnicode CombiningDoubleAcuteAccent = '\x030b'
combiningToUnicode CombiningCaron = '\x030c'
combiningToUnicode CombiningVerticalLineAbove = '\x030d'
combiningToUnicode CombiningDoubleVerticalLineAbove = '\x030e'
combiningToUnicode CombiningDoubleGraveAccent = '\x030f'
combiningToUnicode CombiningCandrabindu = '\x0310'
combiningToUnicode CombiningInvertedBreve = '\x0311'
combiningToUnicode CombiningTurnedCommaAbove = '\x0312'
combiningToUnicode CombiningCommaAbove = '\x0313'
combiningToUnicode CombiningReversedCommaAbove = '\x0314'
combiningToUnicode CombiningCommaAboveRight = '\x0315'
combiningToUnicode CombiningGraveAccentBelow = '\x0316'
combiningToUnicode CombiningAcuteAccentBelow = '\x0317'
combiningToUnicode CombiningLeftTackBelow = '\x0318'
combiningToUnicode CombiningRightTackBelow = '\x0319'
combiningToUnicode CombiningLeftAngleAbove = '\x031a'
combiningToUnicode CombiningHorn = '\x031b'
combiningToUnicode CombiningLeftHalfRingBelow = '\x031c'
combiningToUnicode CombiningUpTackBelow = '\x031d'
combiningToUnicode CombiningDownTackBelow = '\x031e'
combiningToUnicode CombiningPlusSignBelow = '\x031f'
combiningToUnicode CombiningMinusSignBelow = '\x0320'
combiningToUnicode CombiningPalatalizedHookBelow = '\x0321'
combiningToUnicode CombiningRetroflexHookBelow = '\x0322'
combiningToUnicode CombiningDotBelow = '\x0323'
combiningToUnicode CombiningDiaeresisBelow = '\x0324'
combiningToUnicode CombiningRingBelow = '\x0325'
combiningToUnicode CombiningCommaBelow = '\x0326'
combiningToUnicode CombiningCedilla = '\x0327'
combiningToUnicode CombiningOgonek = '\x0328'
combiningToUnicode CombiningVerticalLineBelow = '\x0329'
combiningToUnicode CombiningBridgeBelow = '\x032a'
combiningToUnicode CombiningInvertedDoubleArchBelow = '\x032b'
combiningToUnicode CombiningCaronBelow = '\x032c'
combiningToUnicode CombiningCircumflexAccentBelow = '\x032d'
combiningToUnicode CombiningBreveBelow = '\x032e'
combiningToUnicode CombiningInvertedBreveBelow = '\x032f'
combiningToUnicode CombiningTildeBelow = '\x0330'
combiningToUnicode CombiningMacronBelow = '\x0331'
combiningToUnicode CombiningLowLine = '\x0332'
combiningToUnicode CombiningDoubleLowLine = '\x0333'
combiningToUnicode CombiningTildeOverlay = '\x0334'
combiningToUnicode CombiningShortStrokeOverlay = '\x0335'
combiningToUnicode CombiningLongStrokeOverlay = '\x0336'
combiningToUnicode CombiningShortSolidusOverlay = '\x0337'
combiningToUnicode CombiningLongSolidusOverlay = '\x0338'
combiningToUnicode CombiningRightHalfRingBelow = '\x0339'
combiningToUnicode CombiningInvertedBridgeBelow = '\x033a'
combiningToUnicode CombiningSquareBelow = '\x033b'
combiningToUnicode CombiningSeagullBelow = '\x033c'
combiningToUnicode CombiningXAbove = '\x033d'
combiningToUnicode CombiningVerticalTilde = '\x033e'
combiningToUnicode CombiningDoubleOverline = '\x033f'
combiningToUnicode CombiningGraveToneMark = '\x0340'
combiningToUnicode CombiningAcuteToneMark = '\x0341'
combiningToUnicode CombiningGreekPerispomeni = '\x0342'
combiningToUnicode CombiningGreekKoronis = '\x0343'
combiningToUnicode CombiningGreekDialytikaTonos = '\x0344'
combiningToUnicode CombiningGreekYpogegrammeni = '\x0345'
combiningToUnicode CombiningBridgeAbove = '\x0346'
combiningToUnicode CombiningEqualsSignBelow = '\x0347'
combiningToUnicode CombiningDoubleVerticalLineBelow = '\x0348'
combiningToUnicode CombiningLeftAngleBelow = '\x0349'
combiningToUnicode CombiningNotTildeAbove = '\x034a'
combiningToUnicode CombiningHomotheticAbove = '\x034b'
combiningToUnicode CombiningAlmostEqualToAbove = '\x034c'
combiningToUnicode CombiningLeftRightArrowBelow = '\x034d'
combiningToUnicode CombiningUpwardsArrowBelow = '\x034e'
combiningToUnicode CombiningRightArrowheadAbove = '\x0350'
combiningToUnicode CombiningLeftHalfRingAbove = '\x0351'
combiningToUnicode CombiningFermata = '\x0352'
combiningToUnicode CombiningXBelow = '\x0353'
combiningToUnicode CombiningLeftArrowheadBelow = '\x0354'
combiningToUnicode CombiningRightArrowheadBelow = '\x0355'
combiningToUnicode CombiningRightArrowheadAndUpArrowheadBelow = '\x0356'
combiningToUnicode CombiningRightHalfRingAbove = '\x0357'
combiningToUnicode CombiningDotAboveRight = '\x0358'
combiningToUnicode CombiningAsteriskBelow = '\x0359'
combiningToUnicode CombiningDoubleRingBelow = '\x035a'
combiningToUnicode CombiningZigzagAbove = '\x035b'
combiningToUnicode CombiningDoubleBreveBelow = '\x035c'
combiningToUnicode CombiningDoubleBreve = '\x035d'
combiningToUnicode CombiningDoubleMacron = '\x035e'
combiningToUnicode CombiningDoubleMacronBelow = '\x035f'
combiningToUnicode CombiningDoubleTilde = '\x0360'
combiningToUnicode CombiningDoubleInvertedBreve = '\x0361'
combiningToUnicode CombiningDoubleRightwardsArrowBelow = '\x0362'
combiningToUnicode CombiningLatinSmallLetterA = '\x0363'
combiningToUnicode CombiningLatinSmallLetterE = '\x0364'
combiningToUnicode CombiningLatinSmallLetterI = '\x0365'
combiningToUnicode CombiningLatinSmallLetterO = '\x0366'
combiningToUnicode CombiningLatinSmallLetterU = '\x0367'
combiningToUnicode CombiningLatinSmallLetterC = '\x0368'
combiningToUnicode CombiningLatinSmallLetterD = '\x0369'
combiningToUnicode CombiningLatinSmallLetterH = '\x036a'
combiningToUnicode CombiningLatinSmallLetterM = '\x036b'
combiningToUnicode CombiningLatinSmallLetterR = '\x036c'
combiningToUnicode CombiningLatinSmallLetterT = '\x036d'
combiningToUnicode CombiningLatinSmallLetterV = '\x036e'
combiningToUnicode CombiningLatinSmallLetterX = '\x036f'
combiningToUnicode CombiningCyrillicTitlo = '\x0483'
combiningToUnicode CombiningCyrillicPalatalization = '\x0484'
combiningToUnicode CombiningCyrillicDasiaPneumata = '\x0485'
combiningToUnicode CombiningCyrillicPsiliPneumata = '\x0486'
combiningToUnicode CombiningCyrillicPokrytie = '\x0487'
combiningToUnicode HebrewAccentEtnahta = '\x0591'
combiningToUnicode HebrewAccentSegol = '\x0592'
combiningToUnicode HebrewAccentShalshelet = '\x0593'
combiningToUnicode HebrewAccentZaqefQatan = '\x0594'
combiningToUnicode HebrewAccentZaqefGadol = '\x0595'
combiningToUnicode HebrewAccentTipeha = '\x0596'
combiningToUnicode HebrewAccentRevia = '\x0597'
combiningToUnicode HebrewAccentZarqa = '\x0598'
combiningToUnicode HebrewAccentPashta = '\x0599'
combiningToUnicode HebrewAccentYetiv = '\x059a'
combiningToUnicode HebrewAccentTevir = '\x059b'
combiningToUnicode HebrewAccentGeresh = '\x059c'
combiningToUnicode HebrewAccentGereshMuqdam = '\x059d'
combiningToUnicode HebrewAccentGershayim = '\x059e'
combiningToUnicode HebrewAccentQarneyPara = '\x059f'
combiningToUnicode HebrewAccentTelishaGedola = '\x05a0'
combiningToUnicode HebrewAccentPazer = '\x05a1'
combiningToUnicode HebrewAccentAtnahHafukh = '\x05a2'
combiningToUnicode HebrewAccentMunah = '\x05a3'
combiningToUnicode HebrewAccentMahapakh = '\x05a4'
combiningToUnicode HebrewAccentMerkha = '\x05a5'
combiningToUnicode HebrewAccentMerkhaKefula = '\x05a6'
combiningToUnicode HebrewAccentDarga = '\x05a7'
combiningToUnicode HebrewAccentQadma = '\x05a8'
combiningToUnicode HebrewAccentTelishaQetana = '\x05a9'
combiningToUnicode HebrewAccentYerahBenYomo = '\x05aa'
combiningToUnicode HebrewAccentOle = '\x05ab'
combiningToUnicode HebrewAccentIluy = '\x05ac'
combiningToUnicode HebrewAccentDehi = '\x05ad'
combiningToUnicode HebrewAccentZinor = '\x05ae'
combiningToUnicode HebrewMarkMasoraCircle = '\x05af'
combiningToUnicode HebrewPointSheva = '\x05b0'
combiningToUnicode HebrewPointHatafSegol = '\x05b1'
combiningToUnicode HebrewPointHatafPatah = '\x05b2'
combiningToUnicode HebrewPointHatafQamats = '\x05b3'
combiningToUnicode HebrewPointHiriq = '\x05b4'
combiningToUnicode HebrewPointTsere = '\x05b5'
combiningToUnicode HebrewPointSegol = '\x05b6'
combiningToUnicode HebrewPointPatah = '\x05b7'
combiningToUnicode HebrewPointQamats = '\x05b8'
combiningToUnicode HebrewPointHolam = '\x05b9'
combiningToUnicode HebrewPointHolamHaserForVav = '\x05ba'
combiningToUnicode HebrewPointQubuts = '\x05bb'
combiningToUnicode HebrewPointDageshOrMapiq = '\x05bc'
combiningToUnicode HebrewPointMeteg = '\x05bd'
combiningToUnicode HebrewPointRafe = '\x05bf'
combiningToUnicode HebrewPointShinDot = '\x05c1'
combiningToUnicode HebrewPointSinDot = '\x05c2'
combiningToUnicode HebrewMarkUpperDot = '\x05c4'
combiningToUnicode HebrewMarkLowerDot = '\x05c5'
combiningToUnicode HebrewPointQamatsQatan = '\x05c7'
combiningToUnicode ArabicSignSallallahouAlayheWassallam = '\x0610'
combiningToUnicode ArabicSignAlayheAssallam = '\x0611'
combiningToUnicode ArabicSignRahmatullahAlayhe = '\x0612'
combiningToUnicode ArabicSignRadiAllahouAnhu = '\x0613'
combiningToUnicode ArabicSignTakhallus = '\x0614'
combiningToUnicode ArabicSmallHighTah = '\x0615'
combiningToUnicode ArabicSmallHighLigatureAlefWithLamWithYeh = '\x0616'
combiningToUnicode ArabicSmallHighZain = '\x0617'
combiningToUnicode ArabicSmallFatha = '\x0618'
combiningToUnicode ArabicSmallDamma = '\x0619'
combiningToUnicode ArabicSmallKasra = '\x061a'
combiningToUnicode ArabicFathatan = '\x064b'
combiningToUnicode ArabicDammatan = '\x064c'
combiningToUnicode ArabicKasratan = '\x064d'
combiningToUnicode ArabicFatha = '\x064e'
combiningToUnicode ArabicDamma = '\x064f'
combiningToUnicode ArabicKasra = '\x0650'
combiningToUnicode ArabicShadda = '\x0651'
combiningToUnicode ArabicSukun = '\x0652'
combiningToUnicode ArabicMaddahAbove = '\x0653'
combiningToUnicode ArabicHamzaAbove = '\x0654'
combiningToUnicode ArabicHamzaBelow = '\x0655'
combiningToUnicode ArabicSubscriptAlef = '\x0656'
combiningToUnicode ArabicInvertedDamma = '\x0657'
combiningToUnicode ArabicMarkNoonGhunna = '\x0658'
combiningToUnicode ArabicZwarakay = '\x0659'
combiningToUnicode ArabicVowelSignSmallVAbove = '\x065a'
combiningToUnicode ArabicVowelSignInvertedSmallVAbove = '\x065b'
combiningToUnicode ArabicVowelSignDotBelow = '\x065c'
combiningToUnicode ArabicReversedDamma = '\x065d'
combiningToUnicode ArabicFathaWithTwoDots = '\x065e'
combiningToUnicode ArabicWavyHamzaBelow = '\x065f'
combiningToUnicode ArabicLetterSuperscriptAlef = '\x0670'
combiningToUnicode ArabicSmallHighLigatureSadWithLamWithAlefMaksura = '\x06d6'
combiningToUnicode ArabicSmallHighLigatureQafWithLamWithAlefMaksura = '\x06d7'
combiningToUnicode ArabicSmallHighMeemInitialForm = '\x06d8'
combiningToUnicode ArabicSmallHighLamAlef = '\x06d9'
combiningToUnicode ArabicSmallHighJeem = '\x06da'
combiningToUnicode ArabicSmallHighThreeDots = '\x06db'
combiningToUnicode ArabicSmallHighSeen = '\x06dc'
combiningToUnicode ArabicSmallHighRoundedZero = '\x06df'
combiningToUnicode ArabicSmallHighUprightRectangularZero = '\x06e0'
combiningToUnicode ArabicSmallHighDotlessHeadOfKhah = '\x06e1'
combiningToUnicode ArabicSmallHighMeemIsolatedForm = '\x06e2'
combiningToUnicode ArabicSmallLowSeen = '\x06e3'
combiningToUnicode ArabicSmallHighMadda = '\x06e4'
combiningToUnicode ArabicSmallHighYeh = '\x06e7'
combiningToUnicode ArabicSmallHighNoon = '\x06e8'
combiningToUnicode ArabicEmptyCentreLowStop = '\x06ea'
combiningToUnicode ArabicEmptyCentreHighStop = '\x06eb'
combiningToUnicode ArabicRoundedHighStopWithFilledCentre = '\x06ec'
combiningToUnicode ArabicSmallLowMeem = '\x06ed'
combiningToUnicode SyriacLetterSuperscriptAlaph = '\x0711'
combiningToUnicode SyriacPthahaAbove = '\x0730'
combiningToUnicode SyriacPthahaBelow = '\x0731'
combiningToUnicode SyriacPthahaDotted = '\x0732'
combiningToUnicode SyriacZqaphaAbove = '\x0733'
combiningToUnicode SyriacZqaphaBelow = '\x0734'
combiningToUnicode SyriacZqaphaDotted = '\x0735'
combiningToUnicode SyriacRbasaAbove = '\x0736'
combiningToUnicode SyriacRbasaBelow = '\x0737'
combiningToUnicode SyriacDottedZlamaHorizontal = '\x0738'
combiningToUnicode SyriacDottedZlamaAngular = '\x0739'
combiningToUnicode SyriacHbasaAbove = '\x073a'
combiningToUnicode SyriacHbasaBelow = '\x073b'
combiningToUnicode SyriacHbasaEsasaDotted = '\x073c'
combiningToUnicode SyriacEsasaAbove = '\x073d'
combiningToUnicode SyriacEsasaBelow = '\x073e'
combiningToUnicode SyriacRwaha = '\x073f'
combiningToUnicode SyriacFeminineDot = '\x0740'
combiningToUnicode SyriacQushshaya = '\x0741'
combiningToUnicode SyriacRukkakha = '\x0742'
combiningToUnicode SyriacTwoVerticalDotsAbove = '\x0743'
combiningToUnicode SyriacTwoVerticalDotsBelow = '\x0744'
combiningToUnicode SyriacThreeDotsAbove = '\x0745'
combiningToUnicode SyriacThreeDotsBelow = '\x0746'
combiningToUnicode SyriacObliqueLineAbove = '\x0747'
combiningToUnicode SyriacObliqueLineBelow = '\x0748'
combiningToUnicode SyriacMusic = '\x0749'
combiningToUnicode SyriacBarrekh = '\x074a'
combiningToUnicode NkoCombiningShortHighTone = '\x07eb'
combiningToUnicode NkoCombiningShortLowTone = '\x07ec'
combiningToUnicode NkoCombiningShortRisingTone = '\x07ed'
combiningToUnicode NkoCombiningLongDescendingTone = '\x07ee'
combiningToUnicode NkoCombiningLongHighTone = '\x07ef'
combiningToUnicode NkoCombiningLongLowTone = '\x07f0'
combiningToUnicode NkoCombiningLongRisingTone = '\x07f1'
combiningToUnicode NkoCombiningNasalizationMark = '\x07f2'
combiningToUnicode NkoCombiningDoubleDotAbove = '\x07f3'
combiningToUnicode SamaritanMarkIn = '\x0816'
combiningToUnicode SamaritanMarkInAlaf = '\x0817'
combiningToUnicode SamaritanMarkOcclusion = '\x0818'
combiningToUnicode SamaritanMarkDagesh = '\x0819'
combiningToUnicode SamaritanMarkEpentheticYut = '\x081b'
combiningToUnicode SamaritanVowelSignLongE = '\x081c'
combiningToUnicode SamaritanVowelSignE = '\x081d'
combiningToUnicode SamaritanVowelSignOverlongAa = '\x081e'
combiningToUnicode SamaritanVowelSignLongAa = '\x081f'
combiningToUnicode SamaritanVowelSignAa = '\x0820'
combiningToUnicode SamaritanVowelSignOverlongA = '\x0821'
combiningToUnicode SamaritanVowelSignLongA = '\x0822'
combiningToUnicode SamaritanVowelSignA = '\x0823'
combiningToUnicode SamaritanVowelSignShortA = '\x0825'
combiningToUnicode SamaritanVowelSignLongU = '\x0826'
combiningToUnicode SamaritanVowelSignU = '\x0827'
combiningToUnicode SamaritanVowelSignLongI = '\x0829'
combiningToUnicode SamaritanVowelSignI = '\x082a'
combiningToUnicode SamaritanVowelSignO = '\x082b'
combiningToUnicode SamaritanVowelSignSukun = '\x082c'
combiningToUnicode SamaritanMarkNequdaa = '\x082d'
combiningToUnicode MandaicAffricationMark = '\x0859'
combiningToUnicode MandaicVocalizationMark = '\x085a'
combiningToUnicode MandaicGeminationMark = '\x085b'
combiningToUnicode ArabicSmallHighWordArRub = '\x08d4'
combiningToUnicode ArabicSmallHighSad = '\x08d5'
combiningToUnicode ArabicSmallHighAin = '\x08d6'
combiningToUnicode ArabicSmallHighQaf = '\x08d7'
combiningToUnicode ArabicSmallHighNoonWithKasra = '\x08d8'
combiningToUnicode ArabicSmallLowNoonWithKasra = '\x08d9'
combiningToUnicode ArabicSmallHighWordAthThalatha = '\x08da'
combiningToUnicode ArabicSmallHighWordAsSajda = '\x08db'
combiningToUnicode ArabicSmallHighWordAnNisf = '\x08dc'
combiningToUnicode ArabicSmallHighWordSakta = '\x08dd'
combiningToUnicode ArabicSmallHighWordQif = '\x08de'
combiningToUnicode ArabicSmallHighWordWaqfa = '\x08df'
combiningToUnicode ArabicSmallHighFootnoteMarker = '\x08e0'
combiningToUnicode ArabicSmallHighSignSafha = '\x08e1'
combiningToUnicode ArabicTurnedDammaBelow = '\x08e3'
combiningToUnicode ArabicCurlyFatha = '\x08e4'
combiningToUnicode ArabicCurlyDamma = '\x08e5'
combiningToUnicode ArabicCurlyKasra = '\x08e6'
combiningToUnicode ArabicCurlyFathatan = '\x08e7'
combiningToUnicode ArabicCurlyDammatan = '\x08e8'
combiningToUnicode ArabicCurlyKasratan = '\x08e9'
combiningToUnicode ArabicToneOneDotAbove = '\x08ea'
combiningToUnicode ArabicToneTwoDotsAbove = '\x08eb'
combiningToUnicode ArabicToneLoopAbove = '\x08ec'
combiningToUnicode ArabicToneOneDotBelow = '\x08ed'
combiningToUnicode ArabicToneTwoDotsBelow = '\x08ee'
combiningToUnicode ArabicToneLoopBelow = '\x08ef'
combiningToUnicode ArabicOpenFathatan = '\x08f0'
combiningToUnicode ArabicOpenDammatan = '\x08f1'
combiningToUnicode ArabicOpenKasratan = '\x08f2'
combiningToUnicode ArabicSmallHighWaw = '\x08f3'
combiningToUnicode ArabicFathaWithRing = '\x08f4'
combiningToUnicode ArabicFathaWithDotAbove = '\x08f5'
combiningToUnicode ArabicKasraWithDotBelow = '\x08f6'
combiningToUnicode ArabicLeftArrowheadAbove = '\x08f7'
combiningToUnicode ArabicRightArrowheadAbove = '\x08f8'
combiningToUnicode ArabicLeftArrowheadBelow = '\x08f9'
combiningToUnicode ArabicRightArrowheadBelow = '\x08fa'
combiningToUnicode ArabicDoubleRightArrowheadAbove = '\x08fb'
combiningToUnicode ArabicDoubleRightArrowheadAboveWithDot = '\x08fc'
combiningToUnicode ArabicRightArrowheadAboveWithDot = '\x08fd'
combiningToUnicode ArabicDammaWithDot = '\x08fe'
combiningToUnicode ArabicMarkSidewaysNoonGhunna = '\x08ff'
combiningToUnicode DevanagariSignNukta = '\x093c'
combiningToUnicode DevanagariSignVirama = '\x094d'
combiningToUnicode DevanagariStressSignUdatta = '\x0951'
combiningToUnicode DevanagariStressSignAnudatta = '\x0952'
combiningToUnicode DevanagariGraveAccent = '\x0953'
combiningToUnicode DevanagariAcuteAccent = '\x0954'
combiningToUnicode BengaliSignNukta = '\x09bc'
combiningToUnicode BengaliVowelSignAa = '\x09be'
combiningToUnicode BengaliSignVirama = '\x09cd'
combiningToUnicode BengaliAuLengthMark = '\x09d7'
combiningToUnicode GurmukhiSignNukta = '\x0a3c'
combiningToUnicode GurmukhiSignVirama = '\x0a4d'
combiningToUnicode GujaratiSignNukta = '\x0abc'
combiningToUnicode GujaratiSignVirama = '\x0acd'
combiningToUnicode OriyaSignNukta = '\x0b3c'
combiningToUnicode OriyaVowelSignAa = '\x0b3e'
combiningToUnicode OriyaSignVirama = '\x0b4d'
combiningToUnicode OriyaAiLengthMark = '\x0b56'
combiningToUnicode OriyaAuLengthMark = '\x0b57'
combiningToUnicode TamilVowelSignAa = '\x0bbe'
combiningToUnicode TamilSignVirama = '\x0bcd'
combiningToUnicode TamilAuLengthMark = '\x0bd7'
combiningToUnicode TeluguSignVirama = '\x0c4d'
combiningToUnicode TeluguLengthMark = '\x0c55'
combiningToUnicode TeluguAiLengthMark = '\x0c56'
combiningToUnicode KannadaSignNukta = '\x0cbc'
combiningToUnicode KannadaVowelSignUu = '\x0cc2'
combiningToUnicode KannadaSignVirama = '\x0ccd'
combiningToUnicode KannadaLengthMark = '\x0cd5'
combiningToUnicode KannadaAiLengthMark = '\x0cd6'
combiningToUnicode MalayalamVowelSignAa = '\x0d3e'
combiningToUnicode MalayalamSignVirama = '\x0d4d'
combiningToUnicode MalayalamAuLengthMark = '\x0d57'
combiningToUnicode SinhalaSignAlLakuna = '\x0dca'
combiningToUnicode SinhalaVowelSignAelaPilla = '\x0dcf'
combiningToUnicode SinhalaVowelSignGayanukitta = '\x0ddf'
combiningToUnicode ThaiCharacterSaraU = '\x0e38'
combiningToUnicode ThaiCharacterSaraUu = '\x0e39'
combiningToUnicode ThaiCharacterPhinthu = '\x0e3a'
combiningToUnicode ThaiCharacterMaiEk = '\x0e48'
combiningToUnicode ThaiCharacterMaiTho = '\x0e49'
combiningToUnicode ThaiCharacterMaiTri = '\x0e4a'
combiningToUnicode ThaiCharacterMaiChattawa = '\x0e4b'
combiningToUnicode LaoVowelSignU = '\x0eb8'
combiningToUnicode LaoVowelSignUu = '\x0eb9'
combiningToUnicode LaoToneMaiEk = '\x0ec8'
combiningToUnicode LaoToneMaiTho = '\x0ec9'
combiningToUnicode LaoToneMaiTi = '\x0eca'
combiningToUnicode LaoToneMaiCatawa = '\x0ecb'
combiningToUnicode TibetanAstrologicalSignKhyudPa = '\x0f18'
combiningToUnicode TibetanAstrologicalSignSdongTshugs = '\x0f19'
combiningToUnicode TibetanMarkNgasBzungNyiZla = '\x0f35'
combiningToUnicode TibetanMarkNgasBzungSgorRtags = '\x0f37'
combiningToUnicode TibetanMarkTsaPhru = '\x0f39'
combiningToUnicode TibetanVowelSignAa = '\x0f71'
combiningToUnicode TibetanVowelSignI = '\x0f72'
combiningToUnicode TibetanVowelSignU = '\x0f74'
combiningToUnicode TibetanVowelSignE = '\x0f7a'
combiningToUnicode TibetanVowelSignEe = '\x0f7b'
combiningToUnicode TibetanVowelSignO = '\x0f7c'
combiningToUnicode TibetanVowelSignOo = '\x0f7d'
combiningToUnicode TibetanVowelSignReversedI = '\x0f80'
combiningToUnicode TibetanSignNyiZlaNaaDa = '\x0f82'
combiningToUnicode TibetanSignSnaLdan = '\x0f83'
combiningToUnicode TibetanMarkHalanta = '\x0f84'
combiningToUnicode TibetanSignLciRtags = '\x0f86'
combiningToUnicode TibetanSignYangRtags = '\x0f87'
combiningToUnicode TibetanSubjoinedLetterSsa = '\x0fb5'
combiningToUnicode TibetanSubjoinedLetterHa = '\x0fb7'
combiningToUnicode TibetanSymbolPadmaGdan = '\x0fc6'
combiningToUnicode MyanmarVowelSignIi = '\x102e'
combiningToUnicode MyanmarSignDotBelow = '\x1037'
combiningToUnicode MyanmarSignVirama = '\x1039'
combiningToUnicode MyanmarSignAsat = '\x103a'
combiningToUnicode MyanmarSignShanCouncilEmphaticTone = '\x108d'
combiningToUnicode EthiopicCombiningGeminationAndVowelLengthMark = '\x135d'
combiningToUnicode EthiopicCombiningVowelLengthMark = '\x135e'
combiningToUnicode EthiopicCombiningGeminationMark = '\x135f'
combiningToUnicode TagalogSignVirama = '\x1714'
combiningToUnicode HanunooSignPamudpod = '\x1734'
combiningToUnicode KhmerSignCoeng = '\x17d2'
combiningToUnicode KhmerSignAtthacan = '\x17dd'
combiningToUnicode MongolianLetterAliGaliDagalga = '\x18a9'
combiningToUnicode LimbuSignMukphreng = '\x1939'
combiningToUnicode LimbuSignKemphreng = '\x193a'
combiningToUnicode LimbuSignSaI = '\x193b'
combiningToUnicode BugineseVowelSignI = '\x1a17'
combiningToUnicode BugineseVowelSignU = '\x1a18'
combiningToUnicode TaiThamSignSakot = '\x1a60'
combiningToUnicode TaiThamSignTone1 = '\x1a75'
combiningToUnicode TaiThamSignTone2 = '\x1a76'
combiningToUnicode TaiThamSignKhuenTone3 = '\x1a77'
combiningToUnicode TaiThamSignKhuenTone4 = '\x1a78'
combiningToUnicode TaiThamSignKhuenTone5 = '\x1a79'
combiningToUnicode TaiThamSignRaHaam = '\x1a7a'
combiningToUnicode TaiThamSignMaiSam = '\x1a7b'
combiningToUnicode TaiThamSignKhuenLueKaran = '\x1a7c'
combiningToUnicode TaiThamCombiningCryptogrammicDot = '\x1a7f'
combiningToUnicode CombiningDoubledCircumflexAccent = '\x1ab0'
combiningToUnicode CombiningDiaeresisRing = '\x1ab1'
combiningToUnicode CombiningInfinity = '\x1ab2'
combiningToUnicode CombiningDownwardsArrow = '\x1ab3'
combiningToUnicode CombiningTripleDot = '\x1ab4'
combiningToUnicode CombiningXXBelow = '\x1ab5'
combiningToUnicode CombiningWigglyLineBelow = '\x1ab6'
combiningToUnicode CombiningOpenMarkBelow = '\x1ab7'
combiningToUnicode CombiningDoubleOpenMarkBelow = '\x1ab8'
combiningToUnicode CombiningLightCentralizationStrokeBelow = '\x1ab9'
combiningToUnicode CombiningStrongCentralizationStrokeBelow = '\x1aba'
combiningToUnicode CombiningParenthesesAbove = '\x1abb'
combiningToUnicode CombiningDoubleParenthesesAbove = '\x1abc'
combiningToUnicode CombiningParenthesesBelow = '\x1abd'
combiningToUnicode BalineseSignRerekan = '\x1b34'
combiningToUnicode BalineseVowelSignTedung = '\x1b35'
combiningToUnicode BalineseAdegAdeg = '\x1b44'
combiningToUnicode BalineseMusicalSymbolCombiningTegeh = '\x1b6b'
combiningToUnicode BalineseMusicalSymbolCombiningEndep = '\x1b6c'
combiningToUnicode BalineseMusicalSymbolCombiningKempul = '\x1b6d'
combiningToUnicode BalineseMusicalSymbolCombiningKempli = '\x1b6e'
combiningToUnicode BalineseMusicalSymbolCombiningJegogan = '\x1b6f'
combiningToUnicode BalineseMusicalSymbolCombiningKempulWithJegogan = '\x1b70'
combiningToUnicode BalineseMusicalSymbolCombiningKempliWithJegogan = '\x1b71'
combiningToUnicode BalineseMusicalSymbolCombiningBende = '\x1b72'
combiningToUnicode BalineseMusicalSymbolCombiningGong = '\x1b73'
combiningToUnicode SundaneseSignPamaaeh = '\x1baa'
combiningToUnicode SundaneseSignVirama = '\x1bab'
combiningToUnicode BatakSignTompi = '\x1be6'
combiningToUnicode BatakPangolat = '\x1bf2'
combiningToUnicode BatakPanongonan = '\x1bf3'
combiningToUnicode LepchaSignNukta = '\x1c37'
combiningToUnicode VedicToneKarshana = '\x1cd0'
combiningToUnicode VedicToneShara = '\x1cd1'
combiningToUnicode VedicTonePrenkha = '\x1cd2'
combiningToUnicode VedicSignYajurvedicMidlineSvarita = '\x1cd4'
combiningToUnicode VedicToneYajurvedicAggravatedIndependentSvarita = '\x1cd5'
combiningToUnicode VedicToneYajurvedicIndependentSvarita = '\x1cd6'
combiningToUnicode VedicToneYajurvedicKathakaIndependentSvarita = '\x1cd7'
combiningToUnicode VedicToneCandraBelow = '\x1cd8'
combiningToUnicode VedicToneYajurvedicKathakaIndependentSvaritaSchroeder = '\x1cd9'
combiningToUnicode VedicToneDoubleSvarita = '\x1cda'
combiningToUnicode VedicToneTripleSvarita = '\x1cdb'
combiningToUnicode VedicToneKathakaAnudatta = '\x1cdc'
combiningToUnicode VedicToneDotBelow = '\x1cdd'
combiningToUnicode VedicToneTwoDotsBelow = '\x1cde'
combiningToUnicode VedicToneThreeDotsBelow = '\x1cdf'
combiningToUnicode VedicToneRigvedicKashmiriIndependentSvarita = '\x1ce0'
combiningToUnicode VedicSignVisargaSvarita = '\x1ce2'
combiningToUnicode VedicSignVisargaUdatta = '\x1ce3'
combiningToUnicode VedicSignReversedVisargaUdatta = '\x1ce4'
combiningToUnicode VedicSignVisargaAnudatta = '\x1ce5'
combiningToUnicode VedicSignReversedVisargaAnudatta = '\x1ce6'
combiningToUnicode VedicSignVisargaUdattaWithTail = '\x1ce7'
combiningToUnicode VedicSignVisargaAnudattaWithTail = '\x1ce8'
combiningToUnicode VedicSignTiryak = '\x1ced'
combiningToUnicode VedicToneCandraAbove = '\x1cf4'
combiningToUnicode VedicToneRingAbove = '\x1cf8'
combiningToUnicode VedicToneDoubleRingAbove = '\x1cf9'
combiningToUnicode CombiningDottedGraveAccent = '\x1dc0'
combiningToUnicode CombiningDottedAcuteAccent = '\x1dc1'
combiningToUnicode CombiningSnakeBelow = '\x1dc2'
combiningToUnicode CombiningSuspensionMark = '\x1dc3'
combiningToUnicode CombiningMacronAcute = '\x1dc4'
combiningToUnicode CombiningGraveMacron = '\x1dc5'
combiningToUnicode CombiningMacronGrave = '\x1dc6'
combiningToUnicode CombiningAcuteMacron = '\x1dc7'
combiningToUnicode CombiningGraveAcuteGrave = '\x1dc8'
combiningToUnicode CombiningAcuteGraveAcute = '\x1dc9'
combiningToUnicode CombiningLatinSmallLetterRBelow = '\x1dca'
combiningToUnicode CombiningBreveMacron = '\x1dcb'
combiningToUnicode CombiningMacronBreve = '\x1dcc'
combiningToUnicode CombiningDoubleCircumflexAbove = '\x1dcd'
combiningToUnicode CombiningOgonekAbove = '\x1dce'
combiningToUnicode CombiningZigzagBelow = '\x1dcf'
combiningToUnicode CombiningIsBelow = '\x1dd0'
combiningToUnicode CombiningUrAbove = '\x1dd1'
combiningToUnicode CombiningUsAbove = '\x1dd2'
combiningToUnicode CombiningLatinSmallLetterFlattenedOpenAAbove = '\x1dd3'
combiningToUnicode CombiningLatinSmallLetterAe = '\x1dd4'
combiningToUnicode CombiningLatinSmallLetterAo = '\x1dd5'
combiningToUnicode CombiningLatinSmallLetterAv = '\x1dd6'
combiningToUnicode CombiningLatinSmallLetterCCedilla = '\x1dd7'
combiningToUnicode CombiningLatinSmallLetterInsularD = '\x1dd8'
combiningToUnicode CombiningLatinSmallLetterEth = '\x1dd9'
combiningToUnicode CombiningLatinSmallLetterG = '\x1dda'
combiningToUnicode CombiningLatinLetterSmallCapitalG = '\x1ddb'
combiningToUnicode CombiningLatinSmallLetterK = '\x1ddc'
combiningToUnicode CombiningLatinSmallLetterL = '\x1ddd'
combiningToUnicode CombiningLatinLetterSmallCapitalL = '\x1dde'
combiningToUnicode CombiningLatinLetterSmallCapitalM = '\x1ddf'
combiningToUnicode CombiningLatinSmallLetterN = '\x1de0'
combiningToUnicode CombiningLatinLetterSmallCapitalN = '\x1de1'
combiningToUnicode CombiningLatinLetterSmallCapitalR = '\x1de2'
combiningToUnicode CombiningLatinSmallLetterRRotunda = '\x1de3'
combiningToUnicode CombiningLatinSmallLetterS = '\x1de4'
combiningToUnicode CombiningLatinSmallLetterLongS = '\x1de5'
combiningToUnicode CombiningLatinSmallLetterZ = '\x1de6'
combiningToUnicode CombiningLatinSmallLetterAlpha = '\x1de7'
combiningToUnicode CombiningLatinSmallLetterB = '\x1de8'
combiningToUnicode CombiningLatinSmallLetterBeta = '\x1de9'
combiningToUnicode CombiningLatinSmallLetterSchwa = '\x1dea'
combiningToUnicode CombiningLatinSmallLetterF = '\x1deb'
combiningToUnicode CombiningLatinSmallLetterLWithDoubleMiddleTilde = '\x1dec'
combiningToUnicode CombiningLatinSmallLetterOWithLightCentralizationStroke = '\x1ded'
combiningToUnicode CombiningLatinSmallLetterP = '\x1dee'
combiningToUnicode CombiningLatinSmallLetterEsh = '\x1def'
combiningToUnicode CombiningLatinSmallLetterUWithLightCentralizationStroke = '\x1df0'
combiningToUnicode CombiningLatinSmallLetterW = '\x1df1'
combiningToUnicode CombiningLatinSmallLetterAWithDiaeresis = '\x1df2'
combiningToUnicode CombiningLatinSmallLetterOWithDiaeresis = '\x1df3'
combiningToUnicode CombiningLatinSmallLetterUWithDiaeresis = '\x1df4'
combiningToUnicode CombiningUpTackAbove = '\x1df5'
combiningToUnicode CombiningDeletionMark = '\x1dfb'
combiningToUnicode CombiningDoubleInvertedBreveBelow = '\x1dfc'
combiningToUnicode CombiningAlmostEqualToBelow = '\x1dfd'
combiningToUnicode CombiningLeftArrowheadAbove = '\x1dfe'
combiningToUnicode CombiningRightArrowheadAndDownArrowheadBelow = '\x1dff'
combiningToUnicode CombiningLeftHarpoonAbove = '\x20d0'
combiningToUnicode CombiningRightHarpoonAbove = '\x20d1'
combiningToUnicode CombiningLongVerticalLineOverlay = '\x20d2'
combiningToUnicode CombiningShortVerticalLineOverlay = '\x20d3'
combiningToUnicode CombiningAnticlockwiseArrowAbove = '\x20d4'
combiningToUnicode CombiningClockwiseArrowAbove = '\x20d5'
combiningToUnicode CombiningLeftArrowAbove = '\x20d6'
combiningToUnicode CombiningRightArrowAbove = '\x20d7'
combiningToUnicode CombiningRingOverlay = '\x20d8'
combiningToUnicode CombiningClockwiseRingOverlay = '\x20d9'
combiningToUnicode CombiningAnticlockwiseRingOverlay = '\x20da'
combiningToUnicode CombiningThreeDotsAbove = '\x20db'
combiningToUnicode CombiningFourDotsAbove = '\x20dc'
combiningToUnicode CombiningLeftRightArrowAbove = '\x20e1'
combiningToUnicode CombiningReverseSolidusOverlay = '\x20e5'
combiningToUnicode CombiningDoubleVerticalStrokeOverlay = '\x20e6'
combiningToUnicode CombiningAnnuitySymbol = '\x20e7'
combiningToUnicode CombiningTripleUnderdot = '\x20e8'
combiningToUnicode CombiningWideBridgeAbove = '\x20e9'
combiningToUnicode CombiningLeftwardsArrowOverlay = '\x20ea'
combiningToUnicode CombiningLongDoubleSolidusOverlay = '\x20eb'
combiningToUnicode CombiningRightwardsHarpoonWithBarbDownwards = '\x20ec'
combiningToUnicode CombiningLeftwardsHarpoonWithBarbDownwards = '\x20ed'
combiningToUnicode CombiningLeftArrowBelow = '\x20ee'
combiningToUnicode CombiningRightArrowBelow = '\x20ef'
combiningToUnicode CombiningAsteriskAbove = '\x20f0'
combiningToUnicode CopticCombiningNiAbove = '\x2cef'
combiningToUnicode CopticCombiningSpiritusAsper = '\x2cf0'
combiningToUnicode CopticCombiningSpiritusLenis = '\x2cf1'
combiningToUnicode TifinaghConsonantJoiner = '\x2d7f'
combiningToUnicode CombiningCyrillicLetterBe = '\x2de0'
combiningToUnicode CombiningCyrillicLetterVe = '\x2de1'
combiningToUnicode CombiningCyrillicLetterGhe = '\x2de2'
combiningToUnicode CombiningCyrillicLetterDe = '\x2de3'
combiningToUnicode CombiningCyrillicLetterZhe = '\x2de4'
combiningToUnicode CombiningCyrillicLetterZe = '\x2de5'
combiningToUnicode CombiningCyrillicLetterKa = '\x2de6'
combiningToUnicode CombiningCyrillicLetterEl = '\x2de7'
combiningToUnicode CombiningCyrillicLetterEm = '\x2de8'
combiningToUnicode CombiningCyrillicLetterEn = '\x2de9'
combiningToUnicode CombiningCyrillicLetterO = '\x2dea'
combiningToUnicode CombiningCyrillicLetterPe = '\x2deb'
combiningToUnicode CombiningCyrillicLetterEr = '\x2dec'
combiningToUnicode CombiningCyrillicLetterEs = '\x2ded'
combiningToUnicode CombiningCyrillicLetterTe = '\x2dee'
combiningToUnicode CombiningCyrillicLetterHa = '\x2def'
combiningToUnicode CombiningCyrillicLetterTse = '\x2df0'
combiningToUnicode CombiningCyrillicLetterChe = '\x2df1'
combiningToUnicode CombiningCyrillicLetterSha = '\x2df2'
combiningToUnicode CombiningCyrillicLetterShcha = '\x2df3'
combiningToUnicode CombiningCyrillicLetterFita = '\x2df4'
combiningToUnicode CombiningCyrillicLetterEsTe = '\x2df5'
combiningToUnicode CombiningCyrillicLetterA = '\x2df6'
combiningToUnicode CombiningCyrillicLetterIe = '\x2df7'
combiningToUnicode CombiningCyrillicLetterDjerv = '\x2df8'
combiningToUnicode CombiningCyrillicLetterMonographUk = '\x2df9'
combiningToUnicode CombiningCyrillicLetterYat = '\x2dfa'
combiningToUnicode CombiningCyrillicLetterYu = '\x2dfb'
combiningToUnicode CombiningCyrillicLetterIotifiedA = '\x2dfc'
combiningToUnicode CombiningCyrillicLetterLittleYus = '\x2dfd'
combiningToUnicode CombiningCyrillicLetterBigYus = '\x2dfe'
combiningToUnicode CombiningCyrillicLetterIotifiedBigYus = '\x2dff'
combiningToUnicode IdeographicLevelToneMark = '\x302a'
combiningToUnicode IdeographicRisingToneMark = '\x302b'
combiningToUnicode IdeographicDepartingToneMark = '\x302c'
combiningToUnicode IdeographicEnteringToneMark = '\x302d'
combiningToUnicode HangulSingleDotToneMark = '\x302e'
combiningToUnicode HangulDoubleDotToneMark = '\x302f'
combiningToUnicode CombiningKatakanaHiraganaVoicedSoundMark = '\x3099'
combiningToUnicode CombiningKatakanaHiraganaSemiVoicedSoundMark = '\x309a'
combiningToUnicode CombiningCyrillicVzmet = '\xa66f'
combiningToUnicode CombiningCyrillicLetterUkrainianIe = '\xa674'
combiningToUnicode CombiningCyrillicLetterI = '\xa675'
combiningToUnicode CombiningCyrillicLetterYi = '\xa676'
combiningToUnicode CombiningCyrillicLetterU = '\xa677'
combiningToUnicode CombiningCyrillicLetterHardSign = '\xa678'
combiningToUnicode CombiningCyrillicLetterYeru = '\xa679'
combiningToUnicode CombiningCyrillicLetterSoftSign = '\xa67a'
combiningToUnicode CombiningCyrillicLetterOmega = '\xa67b'
combiningToUnicode CombiningCyrillicKavyka = '\xa67c'
combiningToUnicode CombiningCyrillicPayerok = '\xa67d'
combiningToUnicode CombiningCyrillicLetterEf = '\xa69e'
combiningToUnicode CombiningCyrillicLetterIotifiedE = '\xa69f'
combiningToUnicode BamumCombiningMarkKoqndon = '\xa6f0'
combiningToUnicode BamumCombiningMarkTukwentis = '\xa6f1'
combiningToUnicode SylotiNagriSignHasanta = '\xa806'
combiningToUnicode SaurashtraSignVirama = '\xa8c4'
combiningToUnicode CombiningDevanagariDigitZero = '\xa8e0'
combiningToUnicode CombiningDevanagariDigitOne = '\xa8e1'
combiningToUnicode CombiningDevanagariDigitTwo = '\xa8e2'
combiningToUnicode CombiningDevanagariDigitThree = '\xa8e3'
combiningToUnicode CombiningDevanagariDigitFour = '\xa8e4'
combiningToUnicode CombiningDevanagariDigitFive = '\xa8e5'
combiningToUnicode CombiningDevanagariDigitSix = '\xa8e6'
combiningToUnicode CombiningDevanagariDigitSeven = '\xa8e7'
combiningToUnicode CombiningDevanagariDigitEight = '\xa8e8'
combiningToUnicode CombiningDevanagariDigitNine = '\xa8e9'
combiningToUnicode CombiningDevanagariLetterA = '\xa8ea'
combiningToUnicode CombiningDevanagariLetterU = '\xa8eb'
combiningToUnicode CombiningDevanagariLetterKa = '\xa8ec'
combiningToUnicode CombiningDevanagariLetterNa = '\xa8ed'
combiningToUnicode CombiningDevanagariLetterPa = '\xa8ee'
combiningToUnicode CombiningDevanagariLetterRa = '\xa8ef'
combiningToUnicode CombiningDevanagariLetterVi = '\xa8f0'
combiningToUnicode CombiningDevanagariSignAvagraha = '\xa8f1'
combiningToUnicode KayahLiTonePlophu = '\xa92b'
combiningToUnicode KayahLiToneCalya = '\xa92c'
combiningToUnicode KayahLiToneCalyaPlophu = '\xa92d'
combiningToUnicode RejangVirama = '\xa953'
combiningToUnicode JavaneseSignCecakTelu = '\xa9b3'
combiningToUnicode JavanesePangkon = '\xa9c0'
combiningToUnicode TaiVietMaiKang = '\xaab0'
combiningToUnicode TaiVietVowelI = '\xaab2'
combiningToUnicode TaiVietVowelUe = '\xaab3'
combiningToUnicode TaiVietVowelU = '\xaab4'
combiningToUnicode TaiVietMaiKhit = '\xaab7'
combiningToUnicode TaiVietVowelIa = '\xaab8'
combiningToUnicode TaiVietVowelAm = '\xaabe'
combiningToUnicode TaiVietToneMaiEk = '\xaabf'
combiningToUnicode TaiVietToneMaiTho = '\xaac1'
combiningToUnicode MeeteiMayekVirama = '\xaaf6'
combiningToUnicode MeeteiMayekApunIyek = '\xabed'
combiningToUnicode HebrewPointJudeoSpanishVarika = '\xfb1e'
combiningToUnicode CombiningLigatureLeftHalf = '\xfe20'
combiningToUnicode CombiningLigatureRightHalf = '\xfe21'
combiningToUnicode CombiningDoubleTildeLeftHalf = '\xfe22'
combiningToUnicode CombiningDoubleTildeRightHalf = '\xfe23'
combiningToUnicode CombiningMacronLeftHalf = '\xfe24'
combiningToUnicode CombiningMacronRightHalf = '\xfe25'
combiningToUnicode CombiningConjoiningMacron = '\xfe26'
combiningToUnicode CombiningLigatureLeftHalfBelow = '\xfe27'
combiningToUnicode CombiningLigatureRightHalfBelow = '\xfe28'
combiningToUnicode CombiningTildeLeftHalfBelow = '\xfe29'
combiningToUnicode CombiningTildeRightHalfBelow = '\xfe2a'
combiningToUnicode CombiningMacronLeftHalfBelow = '\xfe2b'
combiningToUnicode CombiningMacronRightHalfBelow = '\xfe2c'
combiningToUnicode CombiningConjoiningMacronBelow = '\xfe2d'
combiningToUnicode CombiningCyrillicTitloLeftHalf = '\xfe2e'
combiningToUnicode CombiningCyrillicTitloRightHalf = '\xfe2f'
combiningToUnicode PhaistosDiscSignCombiningObliqueStroke = '\x101fd'
combiningToUnicode CopticEpactThousandsMark = '\x102e0'
combiningToUnicode CombiningOldPermicLetterAn = '\x10376'
combiningToUnicode CombiningOldPermicLetterDoi = '\x10377'
combiningToUnicode CombiningOldPermicLetterZata = '\x10378'
combiningToUnicode CombiningOldPermicLetterNenoe = '\x10379'
combiningToUnicode CombiningOldPermicLetterSii = '\x1037a'
combiningToUnicode KharoshthiSignDoubleRingBelow = '\x10a0d'
combiningToUnicode KharoshthiSignVisarga = '\x10a0f'
combiningToUnicode KharoshthiSignBarAbove = '\x10a38'
combiningToUnicode KharoshthiSignCauda = '\x10a39'
combiningToUnicode KharoshthiSignDotBelow = '\x10a3a'
combiningToUnicode KharoshthiVirama = '\x10a3f'
combiningToUnicode ManichaeanAbbreviationMarkAbove = '\x10ae5'
combiningToUnicode ManichaeanAbbreviationMarkBelow = '\x10ae6'
combiningToUnicode BrahmiVirama = '\x11046'
combiningToUnicode BrahmiNumberJoiner = '\x1107f'
combiningToUnicode KaithiSignVirama = '\x110b9'
combiningToUnicode KaithiSignNukta = '\x110ba'
combiningToUnicode ChakmaSignCandrabindu = '\x11100'
combiningToUnicode ChakmaSignAnusvara = '\x11101'
combiningToUnicode ChakmaSignVisarga = '\x11102'
combiningToUnicode ChakmaVowelSignA = '\x11127'
combiningToUnicode ChakmaVirama = '\x11133'
combiningToUnicode ChakmaMaayyaa = '\x11134'
combiningToUnicode MahajaniSignNukta = '\x11173'
combiningToUnicode SharadaSignVirama = '\x111c0'
combiningToUnicode SharadaSignNukta = '\x111ca'
combiningToUnicode KhojkiSignVirama = '\x11235'
combiningToUnicode KhojkiSignNukta = '\x11236'
combiningToUnicode KhudawadiSignNukta = '\x112e9'
combiningToUnicode KhudawadiSignVirama = '\x112ea'
combiningToUnicode GranthaSignNukta = '\x1133c'
combiningToUnicode GranthaVowelSignAa = '\x1133e'
combiningToUnicode GranthaSignVirama = '\x1134d'
combiningToUnicode GranthaAuLengthMark = '\x11357'
combiningToUnicode CombiningGranthaDigitZero = '\x11366'
combiningToUnicode CombiningGranthaDigitOne = '\x11367'
combiningToUnicode CombiningGranthaDigitTwo = '\x11368'
combiningToUnicode CombiningGranthaDigitThree = '\x11369'
combiningToUnicode CombiningGranthaDigitFour = '\x1136a'
combiningToUnicode CombiningGranthaDigitFive = '\x1136b'
combiningToUnicode CombiningGranthaDigitSix = '\x1136c'
combiningToUnicode CombiningGranthaLetterA = '\x11370'
combiningToUnicode CombiningGranthaLetterKa = '\x11371'
combiningToUnicode CombiningGranthaLetterNa = '\x11372'
combiningToUnicode CombiningGranthaLetterVi = '\x11373'
combiningToUnicode CombiningGranthaLetterPa = '\x11374'
combiningToUnicode NewaSignVirama = '\x11442'
combiningToUnicode NewaSignNukta = '\x11446'
combiningToUnicode TirhutaVowelSignAa = '\x114b0'
combiningToUnicode TirhutaVowelSignShortE = '\x114ba'
combiningToUnicode TirhutaVowelSignShortO = '\x114bd'
combiningToUnicode TirhutaSignVirama = '\x114c2'
combiningToUnicode TirhutaSignNukta = '\x114c3'
combiningToUnicode SiddhamVowelSignAa = '\x115af'
combiningToUnicode SiddhamSignVirama = '\x115bf'
combiningToUnicode SiddhamSignNukta = '\x115c0'
combiningToUnicode ModiSignVirama = '\x1163f'
combiningToUnicode TakriSignVirama = '\x116b6'
combiningToUnicode TakriSignNukta = '\x116b7'
combiningToUnicode AhomSignKiller = '\x1172b'
combiningToUnicode BhaiksukiSignVirama = '\x11c3f'
combiningToUnicode BassaVahCombiningHighTone = '\x16af0'
combiningToUnicode BassaVahCombiningLowTone = '\x16af1'
combiningToUnicode BassaVahCombiningMidTone = '\x16af2'
combiningToUnicode BassaVahCombiningLowMidTone = '\x16af3'
combiningToUnicode BassaVahCombiningHighLowTone = '\x16af4'
combiningToUnicode PahawhHmongMarkCimTub = '\x16b30'
combiningToUnicode PahawhHmongMarkCimSo = '\x16b31'
combiningToUnicode PahawhHmongMarkCimKes = '\x16b32'
combiningToUnicode PahawhHmongMarkCimKhav = '\x16b33'
combiningToUnicode PahawhHmongMarkCimSuam = '\x16b34'
combiningToUnicode PahawhHmongMarkCimHom = '\x16b35'
combiningToUnicode PahawhHmongMarkCimTaum = '\x16b36'
combiningToUnicode DuployanDoubleMark = '\x1bc9e'
combiningToUnicode MusicalSymbolCombiningStem = '\x1d165'
combiningToUnicode MusicalSymbolCombiningSprechgesangStem = '\x1d166'
combiningToUnicode MusicalSymbolCombiningTremolo1 = '\x1d167'
combiningToUnicode MusicalSymbolCombiningTremolo2 = '\x1d168'
combiningToUnicode MusicalSymbolCombiningTremolo3 = '\x1d169'
combiningToUnicode MusicalSymbolCombiningAugmentationDot = '\x1d16d'
combiningToUnicode MusicalSymbolCombiningFlag1 = '\x1d16e'
combiningToUnicode MusicalSymbolCombiningFlag2 = '\x1d16f'
combiningToUnicode MusicalSymbolCombiningFlag3 = '\x1d170'
combiningToUnicode MusicalSymbolCombiningFlag4 = '\x1d171'
combiningToUnicode MusicalSymbolCombiningFlag5 = '\x1d172'
combiningToUnicode MusicalSymbolCombiningAccent = '\x1d17b'
combiningToUnicode MusicalSymbolCombiningStaccato = '\x1d17c'
combiningToUnicode MusicalSymbolCombiningTenuto = '\x1d17d'
combiningToUnicode MusicalSymbolCombiningStaccatissimo = '\x1d17e'
combiningToUnicode MusicalSymbolCombiningMarcato = '\x1d17f'
combiningToUnicode MusicalSymbolCombiningMarcatoStaccato = '\x1d180'
combiningToUnicode MusicalSymbolCombiningAccentStaccato = '\x1d181'
combiningToUnicode MusicalSymbolCombiningLoure = '\x1d182'
combiningToUnicode MusicalSymbolCombiningDoit = '\x1d185'
combiningToUnicode MusicalSymbolCombiningRip = '\x1d186'
combiningToUnicode MusicalSymbolCombiningFlip = '\x1d187'
combiningToUnicode MusicalSymbolCombiningSmear = '\x1d188'
combiningToUnicode MusicalSymbolCombiningBend = '\x1d189'
combiningToUnicode MusicalSymbolCombiningDoubleTongue = '\x1d18a'
combiningToUnicode MusicalSymbolCombiningTripleTongue = '\x1d18b'
combiningToUnicode MusicalSymbolCombiningDownBow = '\x1d1aa'
combiningToUnicode MusicalSymbolCombiningUpBow = '\x1d1ab'
combiningToUnicode MusicalSymbolCombiningHarmonic = '\x1d1ac'
combiningToUnicode MusicalSymbolCombiningSnapPizzicato = '\x1d1ad'
combiningToUnicode CombiningGreekMusicalTriseme = '\x1d242'
combiningToUnicode CombiningGreekMusicalTetraseme = '\x1d243'
combiningToUnicode CombiningGreekMusicalPentaseme = '\x1d244'
combiningToUnicode CombiningGlagoliticLetterAzu = '\x1e000'
combiningToUnicode CombiningGlagoliticLetterBuky = '\x1e001'
combiningToUnicode CombiningGlagoliticLetterVede = '\x1e002'
combiningToUnicode CombiningGlagoliticLetterGlagoli = '\x1e003'
combiningToUnicode CombiningGlagoliticLetterDobro = '\x1e004'
combiningToUnicode CombiningGlagoliticLetterYestu = '\x1e005'
combiningToUnicode CombiningGlagoliticLetterZhivete = '\x1e006'
combiningToUnicode CombiningGlagoliticLetterZemlja = '\x1e008'
combiningToUnicode CombiningGlagoliticLetterIzhe = '\x1e009'
combiningToUnicode CombiningGlagoliticLetterInitialIzhe = '\x1e00a'
combiningToUnicode CombiningGlagoliticLetterI = '\x1e00b'
combiningToUnicode CombiningGlagoliticLetterDjervi = '\x1e00c'
combiningToUnicode CombiningGlagoliticLetterKako = '\x1e00d'
combiningToUnicode CombiningGlagoliticLetterLjudije = '\x1e00e'
combiningToUnicode CombiningGlagoliticLetterMyslite = '\x1e00f'
combiningToUnicode CombiningGlagoliticLetterNashi = '\x1e010'
combiningToUnicode CombiningGlagoliticLetterOnu = '\x1e011'
combiningToUnicode CombiningGlagoliticLetterPokoji = '\x1e012'
combiningToUnicode CombiningGlagoliticLetterRitsi = '\x1e013'
combiningToUnicode CombiningGlagoliticLetterSlovo = '\x1e014'
combiningToUnicode CombiningGlagoliticLetterTvrido = '\x1e015'
combiningToUnicode CombiningGlagoliticLetterUku = '\x1e016'
combiningToUnicode CombiningGlagoliticLetterFritu = '\x1e017'
combiningToUnicode CombiningGlagoliticLetterHeru = '\x1e018'
combiningToUnicode CombiningGlagoliticLetterShta = '\x1e01b'
combiningToUnicode CombiningGlagoliticLetterTsi = '\x1e01c'
combiningToUnicode CombiningGlagoliticLetterChrivi = '\x1e01d'
combiningToUnicode CombiningGlagoliticLetterSha = '\x1e01e'
combiningToUnicode CombiningGlagoliticLetterYeru = '\x1e01f'
combiningToUnicode CombiningGlagoliticLetterYeri = '\x1e020'
combiningToUnicode CombiningGlagoliticLetterYati = '\x1e021'
combiningToUnicode CombiningGlagoliticLetterYu = '\x1e023'
combiningToUnicode CombiningGlagoliticLetterSmallYus = '\x1e024'
combiningToUnicode CombiningGlagoliticLetterYo = '\x1e026'
combiningToUnicode CombiningGlagoliticLetterIotatedSmallYus = '\x1e027'
combiningToUnicode CombiningGlagoliticLetterBigYus = '\x1e028'
combiningToUnicode CombiningGlagoliticLetterIotatedBigYus = '\x1e029'
combiningToUnicode CombiningGlagoliticLetterFita = '\x1e02a'
combiningToUnicode MendeKikakuiCombiningNumberTeens = '\x1e8d0'
combiningToUnicode MendeKikakuiCombiningNumberTens = '\x1e8d1'
combiningToUnicode MendeKikakuiCombiningNumberHundreds = '\x1e8d2'
combiningToUnicode MendeKikakuiCombiningNumberThousands = '\x1e8d3'
combiningToUnicode MendeKikakuiCombiningNumberTenThousands = '\x1e8d4'
combiningToUnicode MendeKikakuiCombiningNumberHundredThousands = '\x1e8d5'
combiningToUnicode MendeKikakuiCombiningNumberMillions = '\x1e8d6'
combiningToUnicode AdlamAlifLengthener = '\x1e944'
combiningToUnicode AdlamVowelLengthener = '\x1e945'
combiningToUnicode AdlamGeminationMark = '\x1e946'
combiningToUnicode AdlamHamza = '\x1e947'
combiningToUnicode AdlamConsonantModifier = '\x1e948'
combiningToUnicode AdlamGeminateConsonantModifier = '\x1e949'
combiningToUnicode AdlamNukta = '\x1e94a'

-- | Checks if the given 'Char' is a combining character.
isCombiningCharacter
  :: Char -- ^ The given 'Char'acter to check.
  -> Bool -- ^ 'True' if the given 'Char'acter is a combining character; 'False' otherwise.
isCombiningCharacter '\x5bf' = True
isCombiningCharacter '\x5c7' = True
isCombiningCharacter '\x670' = True
isCombiningCharacter '\x711' = True
isCombiningCharacter '\x93c' = True
isCombiningCharacter '\x94d' = True
isCombiningCharacter '\x9bc' = True
isCombiningCharacter '\x9be' = True
isCombiningCharacter '\x9cd' = True
isCombiningCharacter '\x9d7' = True
isCombiningCharacter '\xa3c' = True
isCombiningCharacter '\xa4d' = True
isCombiningCharacter '\xabc' = True
isCombiningCharacter '\xacd' = True
isCombiningCharacter '\xb3c' = True
isCombiningCharacter '\xb3e' = True
isCombiningCharacter '\xb4d' = True
isCombiningCharacter '\xbbe' = True
isCombiningCharacter '\xbcd' = True
isCombiningCharacter '\xbd7' = True
isCombiningCharacter '\xc4d' = True
isCombiningCharacter '\xcbc' = True
isCombiningCharacter '\xcc2' = True
isCombiningCharacter '\xccd' = True
isCombiningCharacter '\xd3e' = True
isCombiningCharacter '\xd4d' = True
isCombiningCharacter '\xd57' = True
isCombiningCharacter '\xdca' = True
isCombiningCharacter '\xdcf' = True
isCombiningCharacter '\xddf' = True
isCombiningCharacter '\xf35' = True
isCombiningCharacter '\xf37' = True
isCombiningCharacter '\xf39' = True
isCombiningCharacter '\xf74' = True
isCombiningCharacter '\xf80' = True
isCombiningCharacter '\xfb5' = True
isCombiningCharacter '\xfb7' = True
isCombiningCharacter '\xfc6' = True
isCombiningCharacter '\x102e' = True
isCombiningCharacter '\x1037' = True
isCombiningCharacter '\x108d' = True
isCombiningCharacter '\x1714' = True
isCombiningCharacter '\x1734' = True
isCombiningCharacter '\x17d2' = True
isCombiningCharacter '\x17dd' = True
isCombiningCharacter '\x18a9' = True
isCombiningCharacter '\x1a60' = True
isCombiningCharacter '\x1a7f' = True
isCombiningCharacter '\x1b44' = True
isCombiningCharacter '\x1be6' = True
isCombiningCharacter '\x1c37' = True
isCombiningCharacter '\x1ced' = True
isCombiningCharacter '\x1cf4' = True
isCombiningCharacter '\x20e1' = True
isCombiningCharacter '\x2d7f' = True
isCombiningCharacter '\xa66f' = True
isCombiningCharacter '\xa806' = True
isCombiningCharacter '\xa8c4' = True
isCombiningCharacter '\xa953' = True
isCombiningCharacter '\xa9b3' = True
isCombiningCharacter '\xa9c0' = True
isCombiningCharacter '\xaab0' = True
isCombiningCharacter '\xaac1' = True
isCombiningCharacter '\xaaf6' = True
isCombiningCharacter '\xabed' = True
isCombiningCharacter '\xfb1e' = True
isCombiningCharacter '\x101fd' = True
isCombiningCharacter '\x102e0' = True
isCombiningCharacter '\x10a0d' = True
isCombiningCharacter '\x10a0f' = True
isCombiningCharacter '\x10a3f' = True
isCombiningCharacter '\x11046' = True
isCombiningCharacter '\x1107f' = True
isCombiningCharacter '\x11127' = True
isCombiningCharacter '\x11173' = True
isCombiningCharacter '\x111c0' = True
isCombiningCharacter '\x111ca' = True
isCombiningCharacter '\x1133c' = True
isCombiningCharacter '\x1133e' = True
isCombiningCharacter '\x1134d' = True
isCombiningCharacter '\x11357' = True
isCombiningCharacter '\x11442' = True
isCombiningCharacter '\x11446' = True
isCombiningCharacter '\x114b0' = True
isCombiningCharacter '\x114ba' = True
isCombiningCharacter '\x114bd' = True
isCombiningCharacter '\x115af' = True
isCombiningCharacter '\x1163f' = True
isCombiningCharacter '\x1172b' = True
isCombiningCharacter '\x11c3f' = True
isCombiningCharacter '\x1bc9e' = True
isCombiningCharacter c
  =  ('\x300' <= c && c <= '\x34e')
  || ('\x1dc0' <= c && c <= '\x1df5')
  || ('\x591' <= c && c <= '\x5bd')
  || ('\x350' <= c && c <= '\x36f')
  || ('\x2de0' <= c && c <= '\x2dff')
  || ('\x8e3' <= c && c <= '\x8ff')
  || ('\x730' <= c && c <= '\x74a')
  || ('\x64b' <= c && c <= '\x65f')
  || ('\xa8e0' <= c && c <= '\xa8f1')
  || ('\x1e008' <= c && c <= '\x1e018')
  || ('\xfe20' <= c && c <= '\xfe2f')
  || ('\x8d4' <= c && c <= '\x8e1')
  || ('\x1ab0' <= c && c <= '\x1abd')
  || ('\x1cd4' <= c && c <= '\x1ce0')
  || ('\x20d0' <= c && c <= '\x20dc')
  || ('\x20e5' <= c && c <= '\x20f0')
  || ('\x610' <= c && c <= '\x61a')
  || ('\xa674' <= c && c <= '\xa67d')
  || ('\x7eb' <= c && c <= '\x7f3')
  || ('\x81b' <= c && c <= '\x823')
  || ('\x1b6b' <= c && c <= '\x1b73')
  || ('\x1a75' <= c && c <= '\x1a7c')
  || ('\x1d17b' <= c && c <= '\x1d182')
  || ('\x6d6' <= c && c <= '\x6dc')
  || ('\x1ce2' <= c && c <= '\x1ce8')
  || ('\x11366' <= c && c <= '\x1136c')
  || ('\x16b30' <= c && c <= '\x16b36')
  || ('\x1d185' <= c && c <= '\x1d18b')
  || ('\x1e000' <= c && c <= '\x1e006')
  || ('\x1e01b' <= c && c <= '\x1e021')
  || ('\x1e8d0' <= c && c <= '\x1e8d6')
  || ('\x1e944' <= c && c <= '\x1e94a')
  || ('\x6df' <= c && c <= '\x6e4')
  || ('\x302a' <= c && c <= '\x302f')
  || ('\x1d16d' <= c && c <= '\x1d172')
  || ('\x483' <= c && c <= '\x487')
  || ('\x829' <= c && c <= '\x82d')
  || ('\x1dfb' <= c && c <= '\x1dff')
  || ('\x10376' <= c && c <= '\x1037a')
  || ('\x11370' <= c && c <= '\x11374')
  || ('\x16af0' <= c && c <= '\x16af4')
  || ('\x1d165' <= c && c <= '\x1d169')
  || ('\x1e026' <= c && c <= '\x1e02a')
  || ('\x6ea' <= c && c <= '\x6ed')
  || ('\x816' <= c && c <= '\x819')
  || ('\x951' <= c && c <= '\x954')
  || ('\xe48' <= c && c <= '\xe4b')
  || ('\xec8' <= c && c <= '\xecb')
  || ('\xf7a' <= c && c <= '\xf7d')
  || ('\x1d1aa' <= c && c <= '\x1d1ad')
  || ('\x825' <= c && c <= '\x827')
  || ('\x859' <= c && c <= '\x85b')
  || ('\xe38' <= c && c <= '\xe3a')
  || ('\xf82' <= c && c <= '\xf84')
  || ('\x135d' <= c && c <= '\x135f')
  || ('\x1939' <= c && c <= '\x193b')
  || ('\x1cd0' <= c && c <= '\x1cd2')
  || ('\x2cef' <= c && c <= '\x2cf1')
  || ('\xa92b' <= c && c <= '\xa92d')
  || ('\xaab2' <= c && c <= '\xaab4')
  || ('\x10a38' <= c && c <= '\x10a3a')
  || ('\x11100' <= c && c <= '\x11102')
  || ('\x1d242' <= c && c <= '\x1d244')
  || ('\x5c1' <= c && c <= '\x5c2')
  || ('\x5c4' <= c && c <= '\x5c5')
  || ('\x6e7' <= c && c <= '\x6e8')
  || ('\xb56' <= c && c <= '\xb57')
  || ('\xc55' <= c && c <= '\xc56')
  || ('\xcd5' <= c && c <= '\xcd6')
  || ('\xeb8' <= c && c <= '\xeb9')
  || ('\xf18' <= c && c <= '\xf19')
  || ('\xf71' <= c && c <= '\xf72')
  || ('\xf86' <= c && c <= '\xf87')
  || ('\x1039' <= c && c <= '\x103a')
  || ('\x1a17' <= c && c <= '\x1a18')
  || ('\x1b34' <= c && c <= '\x1b35')
  || ('\x1baa' <= c && c <= '\x1bab')
  || ('\x1bf2' <= c && c <= '\x1bf3')
  || ('\x1cf8' <= c && c <= '\x1cf9')
  || ('\x3099' <= c && c <= '\x309a')
  || ('\xa69e' <= c && c <= '\xa69f')
  || ('\xa6f0' <= c && c <= '\xa6f1')
  || ('\xaab7' <= c && c <= '\xaab8')
  || ('\xaabe' <= c && c <= '\xaabf')
  || ('\x10ae5' <= c && c <= '\x10ae6')
  || ('\x110b9' <= c && c <= '\x110ba')
  || ('\x11133' <= c && c <= '\x11134')
  || ('\x11235' <= c && c <= '\x11236')
  || ('\x112e9' <= c && c <= '\x112ea')
  || ('\x114c2' <= c && c <= '\x114c3')
  || ('\x115bf' <= c && c <= '\x115c0')
  || ('\x116b6' <= c && c <= '\x116b7')
  || ('\x1e023' <= c && c <= '\x1e024')

-- | Convert the given 'Char' to its corresponding 'CombiningCharacter'. If the
-- given 'Char' is not a /combining/ character, an error is produced.
combiningCharacter'
  :: Char  -- ^ The given 'Char' to convert.
  -> CombiningCharacter -- ^ The corresponding 'CombiningCharacter' if such character exists.
combiningCharacter' c
    | Just y <- combiningCharacter c = y
    | otherwise = error ("The given character " ++ show c ++ "is a not a CombiningCharacter.")

-- | Convert the given 'Char' to its corresponding 'CombiningCharacter' wrapped
-- in a 'Just' data constructor. If the given 'Char' is not a /combining/ character,
-- 'Nothing' is returned.
combiningCharacter
  :: Char  -- ^ The given 'Char' to convert to a 'CombiningCharacter'.
  -> Maybe CombiningCharacter  -- ^ The equivalent 'CombiningCharacter' for the given 'Char' wrapped in a 'Just'; if the 'Char' is not a combining character, 'Nothing'.
combiningCharacter '\x0300' = Just CombiningGraveAccent
combiningCharacter '\x0301' = Just CombiningAcuteAccent
combiningCharacter '\x0302' = Just CombiningCircumflexAccent
combiningCharacter '\x0303' = Just CombiningTilde
combiningCharacter '\x0304' = Just CombiningMacron
combiningCharacter '\x0305' = Just CombiningOverline
combiningCharacter '\x0306' = Just CombiningBreve
combiningCharacter '\x0307' = Just CombiningDotAbove
combiningCharacter '\x0308' = Just CombiningDiaeresis
combiningCharacter '\x0309' = Just CombiningHookAbove
combiningCharacter '\x030a' = Just CombiningRingAbove
combiningCharacter '\x030b' = Just CombiningDoubleAcuteAccent
combiningCharacter '\x030c' = Just CombiningCaron
combiningCharacter '\x030d' = Just CombiningVerticalLineAbove
combiningCharacter '\x030e' = Just CombiningDoubleVerticalLineAbove
combiningCharacter '\x030f' = Just CombiningDoubleGraveAccent
combiningCharacter '\x0310' = Just CombiningCandrabindu
combiningCharacter '\x0311' = Just CombiningInvertedBreve
combiningCharacter '\x0312' = Just CombiningTurnedCommaAbove
combiningCharacter '\x0313' = Just CombiningCommaAbove
combiningCharacter '\x0314' = Just CombiningReversedCommaAbove
combiningCharacter '\x0315' = Just CombiningCommaAboveRight
combiningCharacter '\x0316' = Just CombiningGraveAccentBelow
combiningCharacter '\x0317' = Just CombiningAcuteAccentBelow
combiningCharacter '\x0318' = Just CombiningLeftTackBelow
combiningCharacter '\x0319' = Just CombiningRightTackBelow
combiningCharacter '\x031a' = Just CombiningLeftAngleAbove
combiningCharacter '\x031b' = Just CombiningHorn
combiningCharacter '\x031c' = Just CombiningLeftHalfRingBelow
combiningCharacter '\x031d' = Just CombiningUpTackBelow
combiningCharacter '\x031e' = Just CombiningDownTackBelow
combiningCharacter '\x031f' = Just CombiningPlusSignBelow
combiningCharacter '\x0320' = Just CombiningMinusSignBelow
combiningCharacter '\x0321' = Just CombiningPalatalizedHookBelow
combiningCharacter '\x0322' = Just CombiningRetroflexHookBelow
combiningCharacter '\x0323' = Just CombiningDotBelow
combiningCharacter '\x0324' = Just CombiningDiaeresisBelow
combiningCharacter '\x0325' = Just CombiningRingBelow
combiningCharacter '\x0326' = Just CombiningCommaBelow
combiningCharacter '\x0327' = Just CombiningCedilla
combiningCharacter '\x0328' = Just CombiningOgonek
combiningCharacter '\x0329' = Just CombiningVerticalLineBelow
combiningCharacter '\x032a' = Just CombiningBridgeBelow
combiningCharacter '\x032b' = Just CombiningInvertedDoubleArchBelow
combiningCharacter '\x032c' = Just CombiningCaronBelow
combiningCharacter '\x032d' = Just CombiningCircumflexAccentBelow
combiningCharacter '\x032e' = Just CombiningBreveBelow
combiningCharacter '\x032f' = Just CombiningInvertedBreveBelow
combiningCharacter '\x0330' = Just CombiningTildeBelow
combiningCharacter '\x0331' = Just CombiningMacronBelow
combiningCharacter '\x0332' = Just CombiningLowLine
combiningCharacter '\x0333' = Just CombiningDoubleLowLine
combiningCharacter '\x0334' = Just CombiningTildeOverlay
combiningCharacter '\x0335' = Just CombiningShortStrokeOverlay
combiningCharacter '\x0336' = Just CombiningLongStrokeOverlay
combiningCharacter '\x0337' = Just CombiningShortSolidusOverlay
combiningCharacter '\x0338' = Just CombiningLongSolidusOverlay
combiningCharacter '\x0339' = Just CombiningRightHalfRingBelow
combiningCharacter '\x033a' = Just CombiningInvertedBridgeBelow
combiningCharacter '\x033b' = Just CombiningSquareBelow
combiningCharacter '\x033c' = Just CombiningSeagullBelow
combiningCharacter '\x033d' = Just CombiningXAbove
combiningCharacter '\x033e' = Just CombiningVerticalTilde
combiningCharacter '\x033f' = Just CombiningDoubleOverline
combiningCharacter '\x0340' = Just CombiningGraveToneMark
combiningCharacter '\x0341' = Just CombiningAcuteToneMark
combiningCharacter '\x0342' = Just CombiningGreekPerispomeni
combiningCharacter '\x0343' = Just CombiningGreekKoronis
combiningCharacter '\x0344' = Just CombiningGreekDialytikaTonos
combiningCharacter '\x0345' = Just CombiningGreekYpogegrammeni
combiningCharacter '\x0346' = Just CombiningBridgeAbove
combiningCharacter '\x0347' = Just CombiningEqualsSignBelow
combiningCharacter '\x0348' = Just CombiningDoubleVerticalLineBelow
combiningCharacter '\x0349' = Just CombiningLeftAngleBelow
combiningCharacter '\x034a' = Just CombiningNotTildeAbove
combiningCharacter '\x034b' = Just CombiningHomotheticAbove
combiningCharacter '\x034c' = Just CombiningAlmostEqualToAbove
combiningCharacter '\x034d' = Just CombiningLeftRightArrowBelow
combiningCharacter '\x034e' = Just CombiningUpwardsArrowBelow
combiningCharacter '\x0350' = Just CombiningRightArrowheadAbove
combiningCharacter '\x0351' = Just CombiningLeftHalfRingAbove
combiningCharacter '\x0352' = Just CombiningFermata
combiningCharacter '\x0353' = Just CombiningXBelow
combiningCharacter '\x0354' = Just CombiningLeftArrowheadBelow
combiningCharacter '\x0355' = Just CombiningRightArrowheadBelow
combiningCharacter '\x0356' = Just CombiningRightArrowheadAndUpArrowheadBelow
combiningCharacter '\x0357' = Just CombiningRightHalfRingAbove
combiningCharacter '\x0358' = Just CombiningDotAboveRight
combiningCharacter '\x0359' = Just CombiningAsteriskBelow
combiningCharacter '\x035a' = Just CombiningDoubleRingBelow
combiningCharacter '\x035b' = Just CombiningZigzagAbove
combiningCharacter '\x035c' = Just CombiningDoubleBreveBelow
combiningCharacter '\x035d' = Just CombiningDoubleBreve
combiningCharacter '\x035e' = Just CombiningDoubleMacron
combiningCharacter '\x035f' = Just CombiningDoubleMacronBelow
combiningCharacter '\x0360' = Just CombiningDoubleTilde
combiningCharacter '\x0361' = Just CombiningDoubleInvertedBreve
combiningCharacter '\x0362' = Just CombiningDoubleRightwardsArrowBelow
combiningCharacter '\x0363' = Just CombiningLatinSmallLetterA
combiningCharacter '\x0364' = Just CombiningLatinSmallLetterE
combiningCharacter '\x0365' = Just CombiningLatinSmallLetterI
combiningCharacter '\x0366' = Just CombiningLatinSmallLetterO
combiningCharacter '\x0367' = Just CombiningLatinSmallLetterU
combiningCharacter '\x0368' = Just CombiningLatinSmallLetterC
combiningCharacter '\x0369' = Just CombiningLatinSmallLetterD
combiningCharacter '\x036a' = Just CombiningLatinSmallLetterH
combiningCharacter '\x036b' = Just CombiningLatinSmallLetterM
combiningCharacter '\x036c' = Just CombiningLatinSmallLetterR
combiningCharacter '\x036d' = Just CombiningLatinSmallLetterT
combiningCharacter '\x036e' = Just CombiningLatinSmallLetterV
combiningCharacter '\x036f' = Just CombiningLatinSmallLetterX
combiningCharacter '\x0483' = Just CombiningCyrillicTitlo
combiningCharacter '\x0484' = Just CombiningCyrillicPalatalization
combiningCharacter '\x0485' = Just CombiningCyrillicDasiaPneumata
combiningCharacter '\x0486' = Just CombiningCyrillicPsiliPneumata
combiningCharacter '\x0487' = Just CombiningCyrillicPokrytie
combiningCharacter '\x0591' = Just HebrewAccentEtnahta
combiningCharacter '\x0592' = Just HebrewAccentSegol
combiningCharacter '\x0593' = Just HebrewAccentShalshelet
combiningCharacter '\x0594' = Just HebrewAccentZaqefQatan
combiningCharacter '\x0595' = Just HebrewAccentZaqefGadol
combiningCharacter '\x0596' = Just HebrewAccentTipeha
combiningCharacter '\x0597' = Just HebrewAccentRevia
combiningCharacter '\x0598' = Just HebrewAccentZarqa
combiningCharacter '\x0599' = Just HebrewAccentPashta
combiningCharacter '\x059a' = Just HebrewAccentYetiv
combiningCharacter '\x059b' = Just HebrewAccentTevir
combiningCharacter '\x059c' = Just HebrewAccentGeresh
combiningCharacter '\x059d' = Just HebrewAccentGereshMuqdam
combiningCharacter '\x059e' = Just HebrewAccentGershayim
combiningCharacter '\x059f' = Just HebrewAccentQarneyPara
combiningCharacter '\x05a0' = Just HebrewAccentTelishaGedola
combiningCharacter '\x05a1' = Just HebrewAccentPazer
combiningCharacter '\x05a2' = Just HebrewAccentAtnahHafukh
combiningCharacter '\x05a3' = Just HebrewAccentMunah
combiningCharacter '\x05a4' = Just HebrewAccentMahapakh
combiningCharacter '\x05a5' = Just HebrewAccentMerkha
combiningCharacter '\x05a6' = Just HebrewAccentMerkhaKefula
combiningCharacter '\x05a7' = Just HebrewAccentDarga
combiningCharacter '\x05a8' = Just HebrewAccentQadma
combiningCharacter '\x05a9' = Just HebrewAccentTelishaQetana
combiningCharacter '\x05aa' = Just HebrewAccentYerahBenYomo
combiningCharacter '\x05ab' = Just HebrewAccentOle
combiningCharacter '\x05ac' = Just HebrewAccentIluy
combiningCharacter '\x05ad' = Just HebrewAccentDehi
combiningCharacter '\x05ae' = Just HebrewAccentZinor
combiningCharacter '\x05af' = Just HebrewMarkMasoraCircle
combiningCharacter '\x05b0' = Just HebrewPointSheva
combiningCharacter '\x05b1' = Just HebrewPointHatafSegol
combiningCharacter '\x05b2' = Just HebrewPointHatafPatah
combiningCharacter '\x05b3' = Just HebrewPointHatafQamats
combiningCharacter '\x05b4' = Just HebrewPointHiriq
combiningCharacter '\x05b5' = Just HebrewPointTsere
combiningCharacter '\x05b6' = Just HebrewPointSegol
combiningCharacter '\x05b7' = Just HebrewPointPatah
combiningCharacter '\x05b8' = Just HebrewPointQamats
combiningCharacter '\x05b9' = Just HebrewPointHolam
combiningCharacter '\x05ba' = Just HebrewPointHolamHaserForVav
combiningCharacter '\x05bb' = Just HebrewPointQubuts
combiningCharacter '\x05bc' = Just HebrewPointDageshOrMapiq
combiningCharacter '\x05bd' = Just HebrewPointMeteg
combiningCharacter '\x05bf' = Just HebrewPointRafe
combiningCharacter '\x05c1' = Just HebrewPointShinDot
combiningCharacter '\x05c2' = Just HebrewPointSinDot
combiningCharacter '\x05c4' = Just HebrewMarkUpperDot
combiningCharacter '\x05c5' = Just HebrewMarkLowerDot
combiningCharacter '\x05c7' = Just HebrewPointQamatsQatan
combiningCharacter '\x0610' = Just ArabicSignSallallahouAlayheWassallam
combiningCharacter '\x0611' = Just ArabicSignAlayheAssallam
combiningCharacter '\x0612' = Just ArabicSignRahmatullahAlayhe
combiningCharacter '\x0613' = Just ArabicSignRadiAllahouAnhu
combiningCharacter '\x0614' = Just ArabicSignTakhallus
combiningCharacter '\x0615' = Just ArabicSmallHighTah
combiningCharacter '\x0616' = Just ArabicSmallHighLigatureAlefWithLamWithYeh
combiningCharacter '\x0617' = Just ArabicSmallHighZain
combiningCharacter '\x0618' = Just ArabicSmallFatha
combiningCharacter '\x0619' = Just ArabicSmallDamma
combiningCharacter '\x061a' = Just ArabicSmallKasra
combiningCharacter '\x064b' = Just ArabicFathatan
combiningCharacter '\x064c' = Just ArabicDammatan
combiningCharacter '\x064d' = Just ArabicKasratan
combiningCharacter '\x064e' = Just ArabicFatha
combiningCharacter '\x064f' = Just ArabicDamma
combiningCharacter '\x0650' = Just ArabicKasra
combiningCharacter '\x0651' = Just ArabicShadda
combiningCharacter '\x0652' = Just ArabicSukun
combiningCharacter '\x0653' = Just ArabicMaddahAbove
combiningCharacter '\x0654' = Just ArabicHamzaAbove
combiningCharacter '\x0655' = Just ArabicHamzaBelow
combiningCharacter '\x0656' = Just ArabicSubscriptAlef
combiningCharacter '\x0657' = Just ArabicInvertedDamma
combiningCharacter '\x0658' = Just ArabicMarkNoonGhunna
combiningCharacter '\x0659' = Just ArabicZwarakay
combiningCharacter '\x065a' = Just ArabicVowelSignSmallVAbove
combiningCharacter '\x065b' = Just ArabicVowelSignInvertedSmallVAbove
combiningCharacter '\x065c' = Just ArabicVowelSignDotBelow
combiningCharacter '\x065d' = Just ArabicReversedDamma
combiningCharacter '\x065e' = Just ArabicFathaWithTwoDots
combiningCharacter '\x065f' = Just ArabicWavyHamzaBelow
combiningCharacter '\x0670' = Just ArabicLetterSuperscriptAlef
combiningCharacter '\x06d6' = Just ArabicSmallHighLigatureSadWithLamWithAlefMaksura
combiningCharacter '\x06d7' = Just ArabicSmallHighLigatureQafWithLamWithAlefMaksura
combiningCharacter '\x06d8' = Just ArabicSmallHighMeemInitialForm
combiningCharacter '\x06d9' = Just ArabicSmallHighLamAlef
combiningCharacter '\x06da' = Just ArabicSmallHighJeem
combiningCharacter '\x06db' = Just ArabicSmallHighThreeDots
combiningCharacter '\x06dc' = Just ArabicSmallHighSeen
combiningCharacter '\x06df' = Just ArabicSmallHighRoundedZero
combiningCharacter '\x06e0' = Just ArabicSmallHighUprightRectangularZero
combiningCharacter '\x06e1' = Just ArabicSmallHighDotlessHeadOfKhah
combiningCharacter '\x06e2' = Just ArabicSmallHighMeemIsolatedForm
combiningCharacter '\x06e3' = Just ArabicSmallLowSeen
combiningCharacter '\x06e4' = Just ArabicSmallHighMadda
combiningCharacter '\x06e7' = Just ArabicSmallHighYeh
combiningCharacter '\x06e8' = Just ArabicSmallHighNoon
combiningCharacter '\x06ea' = Just ArabicEmptyCentreLowStop
combiningCharacter '\x06eb' = Just ArabicEmptyCentreHighStop
combiningCharacter '\x06ec' = Just ArabicRoundedHighStopWithFilledCentre
combiningCharacter '\x06ed' = Just ArabicSmallLowMeem
combiningCharacter '\x0711' = Just SyriacLetterSuperscriptAlaph
combiningCharacter '\x0730' = Just SyriacPthahaAbove
combiningCharacter '\x0731' = Just SyriacPthahaBelow
combiningCharacter '\x0732' = Just SyriacPthahaDotted
combiningCharacter '\x0733' = Just SyriacZqaphaAbove
combiningCharacter '\x0734' = Just SyriacZqaphaBelow
combiningCharacter '\x0735' = Just SyriacZqaphaDotted
combiningCharacter '\x0736' = Just SyriacRbasaAbove
combiningCharacter '\x0737' = Just SyriacRbasaBelow
combiningCharacter '\x0738' = Just SyriacDottedZlamaHorizontal
combiningCharacter '\x0739' = Just SyriacDottedZlamaAngular
combiningCharacter '\x073a' = Just SyriacHbasaAbove
combiningCharacter '\x073b' = Just SyriacHbasaBelow
combiningCharacter '\x073c' = Just SyriacHbasaEsasaDotted
combiningCharacter '\x073d' = Just SyriacEsasaAbove
combiningCharacter '\x073e' = Just SyriacEsasaBelow
combiningCharacter '\x073f' = Just SyriacRwaha
combiningCharacter '\x0740' = Just SyriacFeminineDot
combiningCharacter '\x0741' = Just SyriacQushshaya
combiningCharacter '\x0742' = Just SyriacRukkakha
combiningCharacter '\x0743' = Just SyriacTwoVerticalDotsAbove
combiningCharacter '\x0744' = Just SyriacTwoVerticalDotsBelow
combiningCharacter '\x0745' = Just SyriacThreeDotsAbove
combiningCharacter '\x0746' = Just SyriacThreeDotsBelow
combiningCharacter '\x0747' = Just SyriacObliqueLineAbove
combiningCharacter '\x0748' = Just SyriacObliqueLineBelow
combiningCharacter '\x0749' = Just SyriacMusic
combiningCharacter '\x074a' = Just SyriacBarrekh
combiningCharacter '\x07eb' = Just NkoCombiningShortHighTone
combiningCharacter '\x07ec' = Just NkoCombiningShortLowTone
combiningCharacter '\x07ed' = Just NkoCombiningShortRisingTone
combiningCharacter '\x07ee' = Just NkoCombiningLongDescendingTone
combiningCharacter '\x07ef' = Just NkoCombiningLongHighTone
combiningCharacter '\x07f0' = Just NkoCombiningLongLowTone
combiningCharacter '\x07f1' = Just NkoCombiningLongRisingTone
combiningCharacter '\x07f2' = Just NkoCombiningNasalizationMark
combiningCharacter '\x07f3' = Just NkoCombiningDoubleDotAbove
combiningCharacter '\x0816' = Just SamaritanMarkIn
combiningCharacter '\x0817' = Just SamaritanMarkInAlaf
combiningCharacter '\x0818' = Just SamaritanMarkOcclusion
combiningCharacter '\x0819' = Just SamaritanMarkDagesh
combiningCharacter '\x081b' = Just SamaritanMarkEpentheticYut
combiningCharacter '\x081c' = Just SamaritanVowelSignLongE
combiningCharacter '\x081d' = Just SamaritanVowelSignE
combiningCharacter '\x081e' = Just SamaritanVowelSignOverlongAa
combiningCharacter '\x081f' = Just SamaritanVowelSignLongAa
combiningCharacter '\x0820' = Just SamaritanVowelSignAa
combiningCharacter '\x0821' = Just SamaritanVowelSignOverlongA
combiningCharacter '\x0822' = Just SamaritanVowelSignLongA
combiningCharacter '\x0823' = Just SamaritanVowelSignA
combiningCharacter '\x0825' = Just SamaritanVowelSignShortA
combiningCharacter '\x0826' = Just SamaritanVowelSignLongU
combiningCharacter '\x0827' = Just SamaritanVowelSignU
combiningCharacter '\x0829' = Just SamaritanVowelSignLongI
combiningCharacter '\x082a' = Just SamaritanVowelSignI
combiningCharacter '\x082b' = Just SamaritanVowelSignO
combiningCharacter '\x082c' = Just SamaritanVowelSignSukun
combiningCharacter '\x082d' = Just SamaritanMarkNequdaa
combiningCharacter '\x0859' = Just MandaicAffricationMark
combiningCharacter '\x085a' = Just MandaicVocalizationMark
combiningCharacter '\x085b' = Just MandaicGeminationMark
combiningCharacter '\x08d4' = Just ArabicSmallHighWordArRub
combiningCharacter '\x08d5' = Just ArabicSmallHighSad
combiningCharacter '\x08d6' = Just ArabicSmallHighAin
combiningCharacter '\x08d7' = Just ArabicSmallHighQaf
combiningCharacter '\x08d8' = Just ArabicSmallHighNoonWithKasra
combiningCharacter '\x08d9' = Just ArabicSmallLowNoonWithKasra
combiningCharacter '\x08da' = Just ArabicSmallHighWordAthThalatha
combiningCharacter '\x08db' = Just ArabicSmallHighWordAsSajda
combiningCharacter '\x08dc' = Just ArabicSmallHighWordAnNisf
combiningCharacter '\x08dd' = Just ArabicSmallHighWordSakta
combiningCharacter '\x08de' = Just ArabicSmallHighWordQif
combiningCharacter '\x08df' = Just ArabicSmallHighWordWaqfa
combiningCharacter '\x08e0' = Just ArabicSmallHighFootnoteMarker
combiningCharacter '\x08e1' = Just ArabicSmallHighSignSafha
combiningCharacter '\x08e3' = Just ArabicTurnedDammaBelow
combiningCharacter '\x08e4' = Just ArabicCurlyFatha
combiningCharacter '\x08e5' = Just ArabicCurlyDamma
combiningCharacter '\x08e6' = Just ArabicCurlyKasra
combiningCharacter '\x08e7' = Just ArabicCurlyFathatan
combiningCharacter '\x08e8' = Just ArabicCurlyDammatan
combiningCharacter '\x08e9' = Just ArabicCurlyKasratan
combiningCharacter '\x08ea' = Just ArabicToneOneDotAbove
combiningCharacter '\x08eb' = Just ArabicToneTwoDotsAbove
combiningCharacter '\x08ec' = Just ArabicToneLoopAbove
combiningCharacter '\x08ed' = Just ArabicToneOneDotBelow
combiningCharacter '\x08ee' = Just ArabicToneTwoDotsBelow
combiningCharacter '\x08ef' = Just ArabicToneLoopBelow
combiningCharacter '\x08f0' = Just ArabicOpenFathatan
combiningCharacter '\x08f1' = Just ArabicOpenDammatan
combiningCharacter '\x08f2' = Just ArabicOpenKasratan
combiningCharacter '\x08f3' = Just ArabicSmallHighWaw
combiningCharacter '\x08f4' = Just ArabicFathaWithRing
combiningCharacter '\x08f5' = Just ArabicFathaWithDotAbove
combiningCharacter '\x08f6' = Just ArabicKasraWithDotBelow
combiningCharacter '\x08f7' = Just ArabicLeftArrowheadAbove
combiningCharacter '\x08f8' = Just ArabicRightArrowheadAbove
combiningCharacter '\x08f9' = Just ArabicLeftArrowheadBelow
combiningCharacter '\x08fa' = Just ArabicRightArrowheadBelow
combiningCharacter '\x08fb' = Just ArabicDoubleRightArrowheadAbove
combiningCharacter '\x08fc' = Just ArabicDoubleRightArrowheadAboveWithDot
combiningCharacter '\x08fd' = Just ArabicRightArrowheadAboveWithDot
combiningCharacter '\x08fe' = Just ArabicDammaWithDot
combiningCharacter '\x08ff' = Just ArabicMarkSidewaysNoonGhunna
combiningCharacter '\x093c' = Just DevanagariSignNukta
combiningCharacter '\x094d' = Just DevanagariSignVirama
combiningCharacter '\x0951' = Just DevanagariStressSignUdatta
combiningCharacter '\x0952' = Just DevanagariStressSignAnudatta
combiningCharacter '\x0953' = Just DevanagariGraveAccent
combiningCharacter '\x0954' = Just DevanagariAcuteAccent
combiningCharacter '\x09bc' = Just BengaliSignNukta
combiningCharacter '\x09be' = Just BengaliVowelSignAa
combiningCharacter '\x09cd' = Just BengaliSignVirama
combiningCharacter '\x09d7' = Just BengaliAuLengthMark
combiningCharacter '\x0a3c' = Just GurmukhiSignNukta
combiningCharacter '\x0a4d' = Just GurmukhiSignVirama
combiningCharacter '\x0abc' = Just GujaratiSignNukta
combiningCharacter '\x0acd' = Just GujaratiSignVirama
combiningCharacter '\x0b3c' = Just OriyaSignNukta
combiningCharacter '\x0b3e' = Just OriyaVowelSignAa
combiningCharacter '\x0b4d' = Just OriyaSignVirama
combiningCharacter '\x0b56' = Just OriyaAiLengthMark
combiningCharacter '\x0b57' = Just OriyaAuLengthMark
combiningCharacter '\x0bbe' = Just TamilVowelSignAa
combiningCharacter '\x0bcd' = Just TamilSignVirama
combiningCharacter '\x0bd7' = Just TamilAuLengthMark
combiningCharacter '\x0c4d' = Just TeluguSignVirama
combiningCharacter '\x0c55' = Just TeluguLengthMark
combiningCharacter '\x0c56' = Just TeluguAiLengthMark
combiningCharacter '\x0cbc' = Just KannadaSignNukta
combiningCharacter '\x0cc2' = Just KannadaVowelSignUu
combiningCharacter '\x0ccd' = Just KannadaSignVirama
combiningCharacter '\x0cd5' = Just KannadaLengthMark
combiningCharacter '\x0cd6' = Just KannadaAiLengthMark
combiningCharacter '\x0d3e' = Just MalayalamVowelSignAa
combiningCharacter '\x0d4d' = Just MalayalamSignVirama
combiningCharacter '\x0d57' = Just MalayalamAuLengthMark
combiningCharacter '\x0dca' = Just SinhalaSignAlLakuna
combiningCharacter '\x0dcf' = Just SinhalaVowelSignAelaPilla
combiningCharacter '\x0ddf' = Just SinhalaVowelSignGayanukitta
combiningCharacter '\x0e38' = Just ThaiCharacterSaraU
combiningCharacter '\x0e39' = Just ThaiCharacterSaraUu
combiningCharacter '\x0e3a' = Just ThaiCharacterPhinthu
combiningCharacter '\x0e48' = Just ThaiCharacterMaiEk
combiningCharacter '\x0e49' = Just ThaiCharacterMaiTho
combiningCharacter '\x0e4a' = Just ThaiCharacterMaiTri
combiningCharacter '\x0e4b' = Just ThaiCharacterMaiChattawa
combiningCharacter '\x0eb8' = Just LaoVowelSignU
combiningCharacter '\x0eb9' = Just LaoVowelSignUu
combiningCharacter '\x0ec8' = Just LaoToneMaiEk
combiningCharacter '\x0ec9' = Just LaoToneMaiTho
combiningCharacter '\x0eca' = Just LaoToneMaiTi
combiningCharacter '\x0ecb' = Just LaoToneMaiCatawa
combiningCharacter '\x0f18' = Just TibetanAstrologicalSignKhyudPa
combiningCharacter '\x0f19' = Just TibetanAstrologicalSignSdongTshugs
combiningCharacter '\x0f35' = Just TibetanMarkNgasBzungNyiZla
combiningCharacter '\x0f37' = Just TibetanMarkNgasBzungSgorRtags
combiningCharacter '\x0f39' = Just TibetanMarkTsaPhru
combiningCharacter '\x0f71' = Just TibetanVowelSignAa
combiningCharacter '\x0f72' = Just TibetanVowelSignI
combiningCharacter '\x0f74' = Just TibetanVowelSignU
combiningCharacter '\x0f7a' = Just TibetanVowelSignE
combiningCharacter '\x0f7b' = Just TibetanVowelSignEe
combiningCharacter '\x0f7c' = Just TibetanVowelSignO
combiningCharacter '\x0f7d' = Just TibetanVowelSignOo
combiningCharacter '\x0f80' = Just TibetanVowelSignReversedI
combiningCharacter '\x0f82' = Just TibetanSignNyiZlaNaaDa
combiningCharacter '\x0f83' = Just TibetanSignSnaLdan
combiningCharacter '\x0f84' = Just TibetanMarkHalanta
combiningCharacter '\x0f86' = Just TibetanSignLciRtags
combiningCharacter '\x0f87' = Just TibetanSignYangRtags
combiningCharacter '\x0fb5' = Just TibetanSubjoinedLetterSsa
combiningCharacter '\x0fb7' = Just TibetanSubjoinedLetterHa
combiningCharacter '\x0fc6' = Just TibetanSymbolPadmaGdan
combiningCharacter '\x102e' = Just MyanmarVowelSignIi
combiningCharacter '\x1037' = Just MyanmarSignDotBelow
combiningCharacter '\x1039' = Just MyanmarSignVirama
combiningCharacter '\x103a' = Just MyanmarSignAsat
combiningCharacter '\x108d' = Just MyanmarSignShanCouncilEmphaticTone
combiningCharacter '\x135d' = Just EthiopicCombiningGeminationAndVowelLengthMark
combiningCharacter '\x135e' = Just EthiopicCombiningVowelLengthMark
combiningCharacter '\x135f' = Just EthiopicCombiningGeminationMark
combiningCharacter '\x1714' = Just TagalogSignVirama
combiningCharacter '\x1734' = Just HanunooSignPamudpod
combiningCharacter '\x17d2' = Just KhmerSignCoeng
combiningCharacter '\x17dd' = Just KhmerSignAtthacan
combiningCharacter '\x18a9' = Just MongolianLetterAliGaliDagalga
combiningCharacter '\x1939' = Just LimbuSignMukphreng
combiningCharacter '\x193a' = Just LimbuSignKemphreng
combiningCharacter '\x193b' = Just LimbuSignSaI
combiningCharacter '\x1a17' = Just BugineseVowelSignI
combiningCharacter '\x1a18' = Just BugineseVowelSignU
combiningCharacter '\x1a60' = Just TaiThamSignSakot
combiningCharacter '\x1a75' = Just TaiThamSignTone1
combiningCharacter '\x1a76' = Just TaiThamSignTone2
combiningCharacter '\x1a77' = Just TaiThamSignKhuenTone3
combiningCharacter '\x1a78' = Just TaiThamSignKhuenTone4
combiningCharacter '\x1a79' = Just TaiThamSignKhuenTone5
combiningCharacter '\x1a7a' = Just TaiThamSignRaHaam
combiningCharacter '\x1a7b' = Just TaiThamSignMaiSam
combiningCharacter '\x1a7c' = Just TaiThamSignKhuenLueKaran
combiningCharacter '\x1a7f' = Just TaiThamCombiningCryptogrammicDot
combiningCharacter '\x1ab0' = Just CombiningDoubledCircumflexAccent
combiningCharacter '\x1ab1' = Just CombiningDiaeresisRing
combiningCharacter '\x1ab2' = Just CombiningInfinity
combiningCharacter '\x1ab3' = Just CombiningDownwardsArrow
combiningCharacter '\x1ab4' = Just CombiningTripleDot
combiningCharacter '\x1ab5' = Just CombiningXXBelow
combiningCharacter '\x1ab6' = Just CombiningWigglyLineBelow
combiningCharacter '\x1ab7' = Just CombiningOpenMarkBelow
combiningCharacter '\x1ab8' = Just CombiningDoubleOpenMarkBelow
combiningCharacter '\x1ab9' = Just CombiningLightCentralizationStrokeBelow
combiningCharacter '\x1aba' = Just CombiningStrongCentralizationStrokeBelow
combiningCharacter '\x1abb' = Just CombiningParenthesesAbove
combiningCharacter '\x1abc' = Just CombiningDoubleParenthesesAbove
combiningCharacter '\x1abd' = Just CombiningParenthesesBelow
combiningCharacter '\x1b34' = Just BalineseSignRerekan
combiningCharacter '\x1b35' = Just BalineseVowelSignTedung
combiningCharacter '\x1b44' = Just BalineseAdegAdeg
combiningCharacter '\x1b6b' = Just BalineseMusicalSymbolCombiningTegeh
combiningCharacter '\x1b6c' = Just BalineseMusicalSymbolCombiningEndep
combiningCharacter '\x1b6d' = Just BalineseMusicalSymbolCombiningKempul
combiningCharacter '\x1b6e' = Just BalineseMusicalSymbolCombiningKempli
combiningCharacter '\x1b6f' = Just BalineseMusicalSymbolCombiningJegogan
combiningCharacter '\x1b70' = Just BalineseMusicalSymbolCombiningKempulWithJegogan
combiningCharacter '\x1b71' = Just BalineseMusicalSymbolCombiningKempliWithJegogan
combiningCharacter '\x1b72' = Just BalineseMusicalSymbolCombiningBende
combiningCharacter '\x1b73' = Just BalineseMusicalSymbolCombiningGong
combiningCharacter '\x1baa' = Just SundaneseSignPamaaeh
combiningCharacter '\x1bab' = Just SundaneseSignVirama
combiningCharacter '\x1be6' = Just BatakSignTompi
combiningCharacter '\x1bf2' = Just BatakPangolat
combiningCharacter '\x1bf3' = Just BatakPanongonan
combiningCharacter '\x1c37' = Just LepchaSignNukta
combiningCharacter '\x1cd0' = Just VedicToneKarshana
combiningCharacter '\x1cd1' = Just VedicToneShara
combiningCharacter '\x1cd2' = Just VedicTonePrenkha
combiningCharacter '\x1cd4' = Just VedicSignYajurvedicMidlineSvarita
combiningCharacter '\x1cd5' = Just VedicToneYajurvedicAggravatedIndependentSvarita
combiningCharacter '\x1cd6' = Just VedicToneYajurvedicIndependentSvarita
combiningCharacter '\x1cd7' = Just VedicToneYajurvedicKathakaIndependentSvarita
combiningCharacter '\x1cd8' = Just VedicToneCandraBelow
combiningCharacter '\x1cd9' = Just VedicToneYajurvedicKathakaIndependentSvaritaSchroeder
combiningCharacter '\x1cda' = Just VedicToneDoubleSvarita
combiningCharacter '\x1cdb' = Just VedicToneTripleSvarita
combiningCharacter '\x1cdc' = Just VedicToneKathakaAnudatta
combiningCharacter '\x1cdd' = Just VedicToneDotBelow
combiningCharacter '\x1cde' = Just VedicToneTwoDotsBelow
combiningCharacter '\x1cdf' = Just VedicToneThreeDotsBelow
combiningCharacter '\x1ce0' = Just VedicToneRigvedicKashmiriIndependentSvarita
combiningCharacter '\x1ce2' = Just VedicSignVisargaSvarita
combiningCharacter '\x1ce3' = Just VedicSignVisargaUdatta
combiningCharacter '\x1ce4' = Just VedicSignReversedVisargaUdatta
combiningCharacter '\x1ce5' = Just VedicSignVisargaAnudatta
combiningCharacter '\x1ce6' = Just VedicSignReversedVisargaAnudatta
combiningCharacter '\x1ce7' = Just VedicSignVisargaUdattaWithTail
combiningCharacter '\x1ce8' = Just VedicSignVisargaAnudattaWithTail
combiningCharacter '\x1ced' = Just VedicSignTiryak
combiningCharacter '\x1cf4' = Just VedicToneCandraAbove
combiningCharacter '\x1cf8' = Just VedicToneRingAbove
combiningCharacter '\x1cf9' = Just VedicToneDoubleRingAbove
combiningCharacter '\x1dc0' = Just CombiningDottedGraveAccent
combiningCharacter '\x1dc1' = Just CombiningDottedAcuteAccent
combiningCharacter '\x1dc2' = Just CombiningSnakeBelow
combiningCharacter '\x1dc3' = Just CombiningSuspensionMark
combiningCharacter '\x1dc4' = Just CombiningMacronAcute
combiningCharacter '\x1dc5' = Just CombiningGraveMacron
combiningCharacter '\x1dc6' = Just CombiningMacronGrave
combiningCharacter '\x1dc7' = Just CombiningAcuteMacron
combiningCharacter '\x1dc8' = Just CombiningGraveAcuteGrave
combiningCharacter '\x1dc9' = Just CombiningAcuteGraveAcute
combiningCharacter '\x1dca' = Just CombiningLatinSmallLetterRBelow
combiningCharacter '\x1dcb' = Just CombiningBreveMacron
combiningCharacter '\x1dcc' = Just CombiningMacronBreve
combiningCharacter '\x1dcd' = Just CombiningDoubleCircumflexAbove
combiningCharacter '\x1dce' = Just CombiningOgonekAbove
combiningCharacter '\x1dcf' = Just CombiningZigzagBelow
combiningCharacter '\x1dd0' = Just CombiningIsBelow
combiningCharacter '\x1dd1' = Just CombiningUrAbove
combiningCharacter '\x1dd2' = Just CombiningUsAbove
combiningCharacter '\x1dd3' = Just CombiningLatinSmallLetterFlattenedOpenAAbove
combiningCharacter '\x1dd4' = Just CombiningLatinSmallLetterAe
combiningCharacter '\x1dd5' = Just CombiningLatinSmallLetterAo
combiningCharacter '\x1dd6' = Just CombiningLatinSmallLetterAv
combiningCharacter '\x1dd7' = Just CombiningLatinSmallLetterCCedilla
combiningCharacter '\x1dd8' = Just CombiningLatinSmallLetterInsularD
combiningCharacter '\x1dd9' = Just CombiningLatinSmallLetterEth
combiningCharacter '\x1dda' = Just CombiningLatinSmallLetterG
combiningCharacter '\x1ddb' = Just CombiningLatinLetterSmallCapitalG
combiningCharacter '\x1ddc' = Just CombiningLatinSmallLetterK
combiningCharacter '\x1ddd' = Just CombiningLatinSmallLetterL
combiningCharacter '\x1dde' = Just CombiningLatinLetterSmallCapitalL
combiningCharacter '\x1ddf' = Just CombiningLatinLetterSmallCapitalM
combiningCharacter '\x1de0' = Just CombiningLatinSmallLetterN
combiningCharacter '\x1de1' = Just CombiningLatinLetterSmallCapitalN
combiningCharacter '\x1de2' = Just CombiningLatinLetterSmallCapitalR
combiningCharacter '\x1de3' = Just CombiningLatinSmallLetterRRotunda
combiningCharacter '\x1de4' = Just CombiningLatinSmallLetterS
combiningCharacter '\x1de5' = Just CombiningLatinSmallLetterLongS
combiningCharacter '\x1de6' = Just CombiningLatinSmallLetterZ
combiningCharacter '\x1de7' = Just CombiningLatinSmallLetterAlpha
combiningCharacter '\x1de8' = Just CombiningLatinSmallLetterB
combiningCharacter '\x1de9' = Just CombiningLatinSmallLetterBeta
combiningCharacter '\x1dea' = Just CombiningLatinSmallLetterSchwa
combiningCharacter '\x1deb' = Just CombiningLatinSmallLetterF
combiningCharacter '\x1dec' = Just CombiningLatinSmallLetterLWithDoubleMiddleTilde
combiningCharacter '\x1ded' = Just CombiningLatinSmallLetterOWithLightCentralizationStroke
combiningCharacter '\x1dee' = Just CombiningLatinSmallLetterP
combiningCharacter '\x1def' = Just CombiningLatinSmallLetterEsh
combiningCharacter '\x1df0' = Just CombiningLatinSmallLetterUWithLightCentralizationStroke
combiningCharacter '\x1df1' = Just CombiningLatinSmallLetterW
combiningCharacter '\x1df2' = Just CombiningLatinSmallLetterAWithDiaeresis
combiningCharacter '\x1df3' = Just CombiningLatinSmallLetterOWithDiaeresis
combiningCharacter '\x1df4' = Just CombiningLatinSmallLetterUWithDiaeresis
combiningCharacter '\x1df5' = Just CombiningUpTackAbove
combiningCharacter '\x1dfb' = Just CombiningDeletionMark
combiningCharacter '\x1dfc' = Just CombiningDoubleInvertedBreveBelow
combiningCharacter '\x1dfd' = Just CombiningAlmostEqualToBelow
combiningCharacter '\x1dfe' = Just CombiningLeftArrowheadAbove
combiningCharacter '\x1dff' = Just CombiningRightArrowheadAndDownArrowheadBelow
combiningCharacter '\x20d0' = Just CombiningLeftHarpoonAbove
combiningCharacter '\x20d1' = Just CombiningRightHarpoonAbove
combiningCharacter '\x20d2' = Just CombiningLongVerticalLineOverlay
combiningCharacter '\x20d3' = Just CombiningShortVerticalLineOverlay
combiningCharacter '\x20d4' = Just CombiningAnticlockwiseArrowAbove
combiningCharacter '\x20d5' = Just CombiningClockwiseArrowAbove
combiningCharacter '\x20d6' = Just CombiningLeftArrowAbove
combiningCharacter '\x20d7' = Just CombiningRightArrowAbove
combiningCharacter '\x20d8' = Just CombiningRingOverlay
combiningCharacter '\x20d9' = Just CombiningClockwiseRingOverlay
combiningCharacter '\x20da' = Just CombiningAnticlockwiseRingOverlay
combiningCharacter '\x20db' = Just CombiningThreeDotsAbove
combiningCharacter '\x20dc' = Just CombiningFourDotsAbove
combiningCharacter '\x20e1' = Just CombiningLeftRightArrowAbove
combiningCharacter '\x20e5' = Just CombiningReverseSolidusOverlay
combiningCharacter '\x20e6' = Just CombiningDoubleVerticalStrokeOverlay
combiningCharacter '\x20e7' = Just CombiningAnnuitySymbol
combiningCharacter '\x20e8' = Just CombiningTripleUnderdot
combiningCharacter '\x20e9' = Just CombiningWideBridgeAbove
combiningCharacter '\x20ea' = Just CombiningLeftwardsArrowOverlay
combiningCharacter '\x20eb' = Just CombiningLongDoubleSolidusOverlay
combiningCharacter '\x20ec' = Just CombiningRightwardsHarpoonWithBarbDownwards
combiningCharacter '\x20ed' = Just CombiningLeftwardsHarpoonWithBarbDownwards
combiningCharacter '\x20ee' = Just CombiningLeftArrowBelow
combiningCharacter '\x20ef' = Just CombiningRightArrowBelow
combiningCharacter '\x20f0' = Just CombiningAsteriskAbove
combiningCharacter '\x2cef' = Just CopticCombiningNiAbove
combiningCharacter '\x2cf0' = Just CopticCombiningSpiritusAsper
combiningCharacter '\x2cf1' = Just CopticCombiningSpiritusLenis
combiningCharacter '\x2d7f' = Just TifinaghConsonantJoiner
combiningCharacter '\x2de0' = Just CombiningCyrillicLetterBe
combiningCharacter '\x2de1' = Just CombiningCyrillicLetterVe
combiningCharacter '\x2de2' = Just CombiningCyrillicLetterGhe
combiningCharacter '\x2de3' = Just CombiningCyrillicLetterDe
combiningCharacter '\x2de4' = Just CombiningCyrillicLetterZhe
combiningCharacter '\x2de5' = Just CombiningCyrillicLetterZe
combiningCharacter '\x2de6' = Just CombiningCyrillicLetterKa
combiningCharacter '\x2de7' = Just CombiningCyrillicLetterEl
combiningCharacter '\x2de8' = Just CombiningCyrillicLetterEm
combiningCharacter '\x2de9' = Just CombiningCyrillicLetterEn
combiningCharacter '\x2dea' = Just CombiningCyrillicLetterO
combiningCharacter '\x2deb' = Just CombiningCyrillicLetterPe
combiningCharacter '\x2dec' = Just CombiningCyrillicLetterEr
combiningCharacter '\x2ded' = Just CombiningCyrillicLetterEs
combiningCharacter '\x2dee' = Just CombiningCyrillicLetterTe
combiningCharacter '\x2def' = Just CombiningCyrillicLetterHa
combiningCharacter '\x2df0' = Just CombiningCyrillicLetterTse
combiningCharacter '\x2df1' = Just CombiningCyrillicLetterChe
combiningCharacter '\x2df2' = Just CombiningCyrillicLetterSha
combiningCharacter '\x2df3' = Just CombiningCyrillicLetterShcha
combiningCharacter '\x2df4' = Just CombiningCyrillicLetterFita
combiningCharacter '\x2df5' = Just CombiningCyrillicLetterEsTe
combiningCharacter '\x2df6' = Just CombiningCyrillicLetterA
combiningCharacter '\x2df7' = Just CombiningCyrillicLetterIe
combiningCharacter '\x2df8' = Just CombiningCyrillicLetterDjerv
combiningCharacter '\x2df9' = Just CombiningCyrillicLetterMonographUk
combiningCharacter '\x2dfa' = Just CombiningCyrillicLetterYat
combiningCharacter '\x2dfb' = Just CombiningCyrillicLetterYu
combiningCharacter '\x2dfc' = Just CombiningCyrillicLetterIotifiedA
combiningCharacter '\x2dfd' = Just CombiningCyrillicLetterLittleYus
combiningCharacter '\x2dfe' = Just CombiningCyrillicLetterBigYus
combiningCharacter '\x2dff' = Just CombiningCyrillicLetterIotifiedBigYus
combiningCharacter '\x302a' = Just IdeographicLevelToneMark
combiningCharacter '\x302b' = Just IdeographicRisingToneMark
combiningCharacter '\x302c' = Just IdeographicDepartingToneMark
combiningCharacter '\x302d' = Just IdeographicEnteringToneMark
combiningCharacter '\x302e' = Just HangulSingleDotToneMark
combiningCharacter '\x302f' = Just HangulDoubleDotToneMark
combiningCharacter '\x3099' = Just CombiningKatakanaHiraganaVoicedSoundMark
combiningCharacter '\x309a' = Just CombiningKatakanaHiraganaSemiVoicedSoundMark
combiningCharacter '\xa66f' = Just CombiningCyrillicVzmet
combiningCharacter '\xa674' = Just CombiningCyrillicLetterUkrainianIe
combiningCharacter '\xa675' = Just CombiningCyrillicLetterI
combiningCharacter '\xa676' = Just CombiningCyrillicLetterYi
combiningCharacter '\xa677' = Just CombiningCyrillicLetterU
combiningCharacter '\xa678' = Just CombiningCyrillicLetterHardSign
combiningCharacter '\xa679' = Just CombiningCyrillicLetterYeru
combiningCharacter '\xa67a' = Just CombiningCyrillicLetterSoftSign
combiningCharacter '\xa67b' = Just CombiningCyrillicLetterOmega
combiningCharacter '\xa67c' = Just CombiningCyrillicKavyka
combiningCharacter '\xa67d' = Just CombiningCyrillicPayerok
combiningCharacter '\xa69e' = Just CombiningCyrillicLetterEf
combiningCharacter '\xa69f' = Just CombiningCyrillicLetterIotifiedE
combiningCharacter '\xa6f0' = Just BamumCombiningMarkKoqndon
combiningCharacter '\xa6f1' = Just BamumCombiningMarkTukwentis
combiningCharacter '\xa806' = Just SylotiNagriSignHasanta
combiningCharacter '\xa8c4' = Just SaurashtraSignVirama
combiningCharacter '\xa8e0' = Just CombiningDevanagariDigitZero
combiningCharacter '\xa8e1' = Just CombiningDevanagariDigitOne
combiningCharacter '\xa8e2' = Just CombiningDevanagariDigitTwo
combiningCharacter '\xa8e3' = Just CombiningDevanagariDigitThree
combiningCharacter '\xa8e4' = Just CombiningDevanagariDigitFour
combiningCharacter '\xa8e5' = Just CombiningDevanagariDigitFive
combiningCharacter '\xa8e6' = Just CombiningDevanagariDigitSix
combiningCharacter '\xa8e7' = Just CombiningDevanagariDigitSeven
combiningCharacter '\xa8e8' = Just CombiningDevanagariDigitEight
combiningCharacter '\xa8e9' = Just CombiningDevanagariDigitNine
combiningCharacter '\xa8ea' = Just CombiningDevanagariLetterA
combiningCharacter '\xa8eb' = Just CombiningDevanagariLetterU
combiningCharacter '\xa8ec' = Just CombiningDevanagariLetterKa
combiningCharacter '\xa8ed' = Just CombiningDevanagariLetterNa
combiningCharacter '\xa8ee' = Just CombiningDevanagariLetterPa
combiningCharacter '\xa8ef' = Just CombiningDevanagariLetterRa
combiningCharacter '\xa8f0' = Just CombiningDevanagariLetterVi
combiningCharacter '\xa8f1' = Just CombiningDevanagariSignAvagraha
combiningCharacter '\xa92b' = Just KayahLiTonePlophu
combiningCharacter '\xa92c' = Just KayahLiToneCalya
combiningCharacter '\xa92d' = Just KayahLiToneCalyaPlophu
combiningCharacter '\xa953' = Just RejangVirama
combiningCharacter '\xa9b3' = Just JavaneseSignCecakTelu
combiningCharacter '\xa9c0' = Just JavanesePangkon
combiningCharacter '\xaab0' = Just TaiVietMaiKang
combiningCharacter '\xaab2' = Just TaiVietVowelI
combiningCharacter '\xaab3' = Just TaiVietVowelUe
combiningCharacter '\xaab4' = Just TaiVietVowelU
combiningCharacter '\xaab7' = Just TaiVietMaiKhit
combiningCharacter '\xaab8' = Just TaiVietVowelIa
combiningCharacter '\xaabe' = Just TaiVietVowelAm
combiningCharacter '\xaabf' = Just TaiVietToneMaiEk
combiningCharacter '\xaac1' = Just TaiVietToneMaiTho
combiningCharacter '\xaaf6' = Just MeeteiMayekVirama
combiningCharacter '\xabed' = Just MeeteiMayekApunIyek
combiningCharacter '\xfb1e' = Just HebrewPointJudeoSpanishVarika
combiningCharacter '\xfe20' = Just CombiningLigatureLeftHalf
combiningCharacter '\xfe21' = Just CombiningLigatureRightHalf
combiningCharacter '\xfe22' = Just CombiningDoubleTildeLeftHalf
combiningCharacter '\xfe23' = Just CombiningDoubleTildeRightHalf
combiningCharacter '\xfe24' = Just CombiningMacronLeftHalf
combiningCharacter '\xfe25' = Just CombiningMacronRightHalf
combiningCharacter '\xfe26' = Just CombiningConjoiningMacron
combiningCharacter '\xfe27' = Just CombiningLigatureLeftHalfBelow
combiningCharacter '\xfe28' = Just CombiningLigatureRightHalfBelow
combiningCharacter '\xfe29' = Just CombiningTildeLeftHalfBelow
combiningCharacter '\xfe2a' = Just CombiningTildeRightHalfBelow
combiningCharacter '\xfe2b' = Just CombiningMacronLeftHalfBelow
combiningCharacter '\xfe2c' = Just CombiningMacronRightHalfBelow
combiningCharacter '\xfe2d' = Just CombiningConjoiningMacronBelow
combiningCharacter '\xfe2e' = Just CombiningCyrillicTitloLeftHalf
combiningCharacter '\xfe2f' = Just CombiningCyrillicTitloRightHalf
combiningCharacter '\x101fd' = Just PhaistosDiscSignCombiningObliqueStroke
combiningCharacter '\x102e0' = Just CopticEpactThousandsMark
combiningCharacter '\x10376' = Just CombiningOldPermicLetterAn
combiningCharacter '\x10377' = Just CombiningOldPermicLetterDoi
combiningCharacter '\x10378' = Just CombiningOldPermicLetterZata
combiningCharacter '\x10379' = Just CombiningOldPermicLetterNenoe
combiningCharacter '\x1037a' = Just CombiningOldPermicLetterSii
combiningCharacter '\x10a0d' = Just KharoshthiSignDoubleRingBelow
combiningCharacter '\x10a0f' = Just KharoshthiSignVisarga
combiningCharacter '\x10a38' = Just KharoshthiSignBarAbove
combiningCharacter '\x10a39' = Just KharoshthiSignCauda
combiningCharacter '\x10a3a' = Just KharoshthiSignDotBelow
combiningCharacter '\x10a3f' = Just KharoshthiVirama
combiningCharacter '\x10ae5' = Just ManichaeanAbbreviationMarkAbove
combiningCharacter '\x10ae6' = Just ManichaeanAbbreviationMarkBelow
combiningCharacter '\x11046' = Just BrahmiVirama
combiningCharacter '\x1107f' = Just BrahmiNumberJoiner
combiningCharacter '\x110b9' = Just KaithiSignVirama
combiningCharacter '\x110ba' = Just KaithiSignNukta
combiningCharacter '\x11100' = Just ChakmaSignCandrabindu
combiningCharacter '\x11101' = Just ChakmaSignAnusvara
combiningCharacter '\x11102' = Just ChakmaSignVisarga
combiningCharacter '\x11127' = Just ChakmaVowelSignA
combiningCharacter '\x11133' = Just ChakmaVirama
combiningCharacter '\x11134' = Just ChakmaMaayyaa
combiningCharacter '\x11173' = Just MahajaniSignNukta
combiningCharacter '\x111c0' = Just SharadaSignVirama
combiningCharacter '\x111ca' = Just SharadaSignNukta
combiningCharacter '\x11235' = Just KhojkiSignVirama
combiningCharacter '\x11236' = Just KhojkiSignNukta
combiningCharacter '\x112e9' = Just KhudawadiSignNukta
combiningCharacter '\x112ea' = Just KhudawadiSignVirama
combiningCharacter '\x1133c' = Just GranthaSignNukta
combiningCharacter '\x1133e' = Just GranthaVowelSignAa
combiningCharacter '\x1134d' = Just GranthaSignVirama
combiningCharacter '\x11357' = Just GranthaAuLengthMark
combiningCharacter '\x11366' = Just CombiningGranthaDigitZero
combiningCharacter '\x11367' = Just CombiningGranthaDigitOne
combiningCharacter '\x11368' = Just CombiningGranthaDigitTwo
combiningCharacter '\x11369' = Just CombiningGranthaDigitThree
combiningCharacter '\x1136a' = Just CombiningGranthaDigitFour
combiningCharacter '\x1136b' = Just CombiningGranthaDigitFive
combiningCharacter '\x1136c' = Just CombiningGranthaDigitSix
combiningCharacter '\x11370' = Just CombiningGranthaLetterA
combiningCharacter '\x11371' = Just CombiningGranthaLetterKa
combiningCharacter '\x11372' = Just CombiningGranthaLetterNa
combiningCharacter '\x11373' = Just CombiningGranthaLetterVi
combiningCharacter '\x11374' = Just CombiningGranthaLetterPa
combiningCharacter '\x11442' = Just NewaSignVirama
combiningCharacter '\x11446' = Just NewaSignNukta
combiningCharacter '\x114b0' = Just TirhutaVowelSignAa
combiningCharacter '\x114ba' = Just TirhutaVowelSignShortE
combiningCharacter '\x114bd' = Just TirhutaVowelSignShortO
combiningCharacter '\x114c2' = Just TirhutaSignVirama
combiningCharacter '\x114c3' = Just TirhutaSignNukta
combiningCharacter '\x115af' = Just SiddhamVowelSignAa
combiningCharacter '\x115bf' = Just SiddhamSignVirama
combiningCharacter '\x115c0' = Just SiddhamSignNukta
combiningCharacter '\x1163f' = Just ModiSignVirama
combiningCharacter '\x116b6' = Just TakriSignVirama
combiningCharacter '\x116b7' = Just TakriSignNukta
combiningCharacter '\x1172b' = Just AhomSignKiller
combiningCharacter '\x11c3f' = Just BhaiksukiSignVirama
combiningCharacter '\x16af0' = Just BassaVahCombiningHighTone
combiningCharacter '\x16af1' = Just BassaVahCombiningLowTone
combiningCharacter '\x16af2' = Just BassaVahCombiningMidTone
combiningCharacter '\x16af3' = Just BassaVahCombiningLowMidTone
combiningCharacter '\x16af4' = Just BassaVahCombiningHighLowTone
combiningCharacter '\x16b30' = Just PahawhHmongMarkCimTub
combiningCharacter '\x16b31' = Just PahawhHmongMarkCimSo
combiningCharacter '\x16b32' = Just PahawhHmongMarkCimKes
combiningCharacter '\x16b33' = Just PahawhHmongMarkCimKhav
combiningCharacter '\x16b34' = Just PahawhHmongMarkCimSuam
combiningCharacter '\x16b35' = Just PahawhHmongMarkCimHom
combiningCharacter '\x16b36' = Just PahawhHmongMarkCimTaum
combiningCharacter '\x1bc9e' = Just DuployanDoubleMark
combiningCharacter '\x1d165' = Just MusicalSymbolCombiningStem
combiningCharacter '\x1d166' = Just MusicalSymbolCombiningSprechgesangStem
combiningCharacter '\x1d167' = Just MusicalSymbolCombiningTremolo1
combiningCharacter '\x1d168' = Just MusicalSymbolCombiningTremolo2
combiningCharacter '\x1d169' = Just MusicalSymbolCombiningTremolo3
combiningCharacter '\x1d16d' = Just MusicalSymbolCombiningAugmentationDot
combiningCharacter '\x1d16e' = Just MusicalSymbolCombiningFlag1
combiningCharacter '\x1d16f' = Just MusicalSymbolCombiningFlag2
combiningCharacter '\x1d170' = Just MusicalSymbolCombiningFlag3
combiningCharacter '\x1d171' = Just MusicalSymbolCombiningFlag4
combiningCharacter '\x1d172' = Just MusicalSymbolCombiningFlag5
combiningCharacter '\x1d17b' = Just MusicalSymbolCombiningAccent
combiningCharacter '\x1d17c' = Just MusicalSymbolCombiningStaccato
combiningCharacter '\x1d17d' = Just MusicalSymbolCombiningTenuto
combiningCharacter '\x1d17e' = Just MusicalSymbolCombiningStaccatissimo
combiningCharacter '\x1d17f' = Just MusicalSymbolCombiningMarcato
combiningCharacter '\x1d180' = Just MusicalSymbolCombiningMarcatoStaccato
combiningCharacter '\x1d181' = Just MusicalSymbolCombiningAccentStaccato
combiningCharacter '\x1d182' = Just MusicalSymbolCombiningLoure
combiningCharacter '\x1d185' = Just MusicalSymbolCombiningDoit
combiningCharacter '\x1d186' = Just MusicalSymbolCombiningRip
combiningCharacter '\x1d187' = Just MusicalSymbolCombiningFlip
combiningCharacter '\x1d188' = Just MusicalSymbolCombiningSmear
combiningCharacter '\x1d189' = Just MusicalSymbolCombiningBend
combiningCharacter '\x1d18a' = Just MusicalSymbolCombiningDoubleTongue
combiningCharacter '\x1d18b' = Just MusicalSymbolCombiningTripleTongue
combiningCharacter '\x1d1aa' = Just MusicalSymbolCombiningDownBow
combiningCharacter '\x1d1ab' = Just MusicalSymbolCombiningUpBow
combiningCharacter '\x1d1ac' = Just MusicalSymbolCombiningHarmonic
combiningCharacter '\x1d1ad' = Just MusicalSymbolCombiningSnapPizzicato
combiningCharacter '\x1d242' = Just CombiningGreekMusicalTriseme
combiningCharacter '\x1d243' = Just CombiningGreekMusicalTetraseme
combiningCharacter '\x1d244' = Just CombiningGreekMusicalPentaseme
combiningCharacter '\x1e000' = Just CombiningGlagoliticLetterAzu
combiningCharacter '\x1e001' = Just CombiningGlagoliticLetterBuky
combiningCharacter '\x1e002' = Just CombiningGlagoliticLetterVede
combiningCharacter '\x1e003' = Just CombiningGlagoliticLetterGlagoli
combiningCharacter '\x1e004' = Just CombiningGlagoliticLetterDobro
combiningCharacter '\x1e005' = Just CombiningGlagoliticLetterYestu
combiningCharacter '\x1e006' = Just CombiningGlagoliticLetterZhivete
combiningCharacter '\x1e008' = Just CombiningGlagoliticLetterZemlja
combiningCharacter '\x1e009' = Just CombiningGlagoliticLetterIzhe
combiningCharacter '\x1e00a' = Just CombiningGlagoliticLetterInitialIzhe
combiningCharacter '\x1e00b' = Just CombiningGlagoliticLetterI
combiningCharacter '\x1e00c' = Just CombiningGlagoliticLetterDjervi
combiningCharacter '\x1e00d' = Just CombiningGlagoliticLetterKako
combiningCharacter '\x1e00e' = Just CombiningGlagoliticLetterLjudije
combiningCharacter '\x1e00f' = Just CombiningGlagoliticLetterMyslite
combiningCharacter '\x1e010' = Just CombiningGlagoliticLetterNashi
combiningCharacter '\x1e011' = Just CombiningGlagoliticLetterOnu
combiningCharacter '\x1e012' = Just CombiningGlagoliticLetterPokoji
combiningCharacter '\x1e013' = Just CombiningGlagoliticLetterRitsi
combiningCharacter '\x1e014' = Just CombiningGlagoliticLetterSlovo
combiningCharacter '\x1e015' = Just CombiningGlagoliticLetterTvrido
combiningCharacter '\x1e016' = Just CombiningGlagoliticLetterUku
combiningCharacter '\x1e017' = Just CombiningGlagoliticLetterFritu
combiningCharacter '\x1e018' = Just CombiningGlagoliticLetterHeru
combiningCharacter '\x1e01b' = Just CombiningGlagoliticLetterShta
combiningCharacter '\x1e01c' = Just CombiningGlagoliticLetterTsi
combiningCharacter '\x1e01d' = Just CombiningGlagoliticLetterChrivi
combiningCharacter '\x1e01e' = Just CombiningGlagoliticLetterSha
combiningCharacter '\x1e01f' = Just CombiningGlagoliticLetterYeru
combiningCharacter '\x1e020' = Just CombiningGlagoliticLetterYeri
combiningCharacter '\x1e021' = Just CombiningGlagoliticLetterYati
combiningCharacter '\x1e023' = Just CombiningGlagoliticLetterYu
combiningCharacter '\x1e024' = Just CombiningGlagoliticLetterSmallYus
combiningCharacter '\x1e026' = Just CombiningGlagoliticLetterYo
combiningCharacter '\x1e027' = Just CombiningGlagoliticLetterIotatedSmallYus
combiningCharacter '\x1e028' = Just CombiningGlagoliticLetterBigYus
combiningCharacter '\x1e029' = Just CombiningGlagoliticLetterIotatedBigYus
combiningCharacter '\x1e02a' = Just CombiningGlagoliticLetterFita
combiningCharacter '\x1e8d0' = Just MendeKikakuiCombiningNumberTeens
combiningCharacter '\x1e8d1' = Just MendeKikakuiCombiningNumberTens
combiningCharacter '\x1e8d2' = Just MendeKikakuiCombiningNumberHundreds
combiningCharacter '\x1e8d3' = Just MendeKikakuiCombiningNumberThousands
combiningCharacter '\x1e8d4' = Just MendeKikakuiCombiningNumberTenThousands
combiningCharacter '\x1e8d5' = Just MendeKikakuiCombiningNumberHundredThousands
combiningCharacter '\x1e8d6' = Just MendeKikakuiCombiningNumberMillions
combiningCharacter '\x1e944' = Just AdlamAlifLengthener
combiningCharacter '\x1e945' = Just AdlamVowelLengthener
combiningCharacter '\x1e946' = Just AdlamGeminationMark
combiningCharacter '\x1e947' = Just AdlamHamza
combiningCharacter '\x1e948' = Just AdlamConsonantModifier
combiningCharacter '\x1e949' = Just AdlamGeminateConsonantModifier
combiningCharacter '\x1e94a' = Just AdlamNukta
combiningCharacter _ = Nothing

-- | Convert the given 'Char'acter to a 2-tuple that contains the "root"
-- character, and a set of 'CombiningCharacter's that can be applied to
-- construct that character. Characters that do not contain a combining
-- character return an empty list for the list of 'CombiningCharacter's.
--
-- For a 'Char' that is a 'CombiningCharacter' itself, it will return a
-- 2-tuple with that character as first item, and an empty list of
-- 'CombiningCharacter's.
decomposeCombiningSequence
  :: Char -- ^ The 'Char'acter to decompose.
  -> (Char, [CombiningCharacter]) -- ^ A 2-tuple with the "root" 'Char'acter and the list of 'CombiningCharacter's that are applied to it.
decomposeCombiningSequence c
    | Just (c', cc) <- decomposeCombining c = (cc:) <$> decomposeCombiningSequence c'
    | otherwise = (c, [])

-- | Convert the given 'Char'acter to its "root" character that omits all the
-- applied 'CombiningCharacter's. If the given 'Char'acter is a
-- 'CombiningCharacter' itself, then this is returned.
stripCombiningSequence
  :: Char  -- ^ The 'Char'acter that should be stripped from its applied 'CombiningCharacter's.
  -> Char -- ^ The "root" 'Char'acter that is the given 'Char'acter stripped from the applied 'CombiningCharacter's.
stripCombiningSequence c
    | Just (c', _) <- decomposeCombining c = stripCombiningSequence c'
    | otherwise = c

-- | Remove the 'CombiningCharacter's in the 'Text' and the ones that are
-- applied to a character through "composition". This function is useful for
-- example to remove diacritics from a 'Text' object.
stripCombinings
  :: Text  -- ^ The given 'Text' object to strip 'CombiningCharacter's from, both through filtering and decomposing.
  -> Text  -- ^ A 'Text' object where the 'CombiningCharacter's are filtered out.
stripCombinings = T.filter (not . isCombiningCharacter) . T.map stripCombiningSequence

-- | Convert a given character that can be represented a 'Char' and a
-- 'CombiningCharacter' to a 2-tuple that contains this combination.
-- The returning 'Char' (the first item in the 2-tuple) can still be a composed
-- form, and thus can sometimes be passed again through this function.
decomposeCombining
  :: Char  -- ^ The given 'Char'acter to decompose.
  -> Maybe (Char, CombiningCharacter)  -- ^ A 2-tuple of a 'Char'acter and a 'CombiningCharacter' wrapped in a 'Just' if the 'Char' can be decomposed; 'Nothing' otherwise.
decomposeCombining '\x00c0' = Just ('A', CombiningGraveAccent)
decomposeCombining '\x00c1' = Just ('A', CombiningAcuteAccent)
decomposeCombining '\x00c2' = Just ('A', CombiningCircumflexAccent)
decomposeCombining '\x00c3' = Just ('A', CombiningTilde)
decomposeCombining '\x00c4' = Just ('A', CombiningDiaeresis)
decomposeCombining '\x00c5' = Just ('A', CombiningRingAbove)
decomposeCombining '\x00c7' = Just ('C', CombiningCedilla)
decomposeCombining '\x00c8' = Just ('E', CombiningGraveAccent)
decomposeCombining '\x00c9' = Just ('E', CombiningAcuteAccent)
decomposeCombining '\x00ca' = Just ('E', CombiningCircumflexAccent)
decomposeCombining '\x00cb' = Just ('E', CombiningDiaeresis)
decomposeCombining '\x00cc' = Just ('I', CombiningGraveAccent)
decomposeCombining '\x00cd' = Just ('I', CombiningAcuteAccent)
decomposeCombining '\x00ce' = Just ('I', CombiningCircumflexAccent)
decomposeCombining '\x00cf' = Just ('I', CombiningDiaeresis)
decomposeCombining '\x00d1' = Just ('N', CombiningTilde)
decomposeCombining '\x00d2' = Just ('O', CombiningGraveAccent)
decomposeCombining '\x00d3' = Just ('O', CombiningAcuteAccent)
decomposeCombining '\x00d4' = Just ('O', CombiningCircumflexAccent)
decomposeCombining '\x00d5' = Just ('O', CombiningTilde)
decomposeCombining '\x00d6' = Just ('O', CombiningDiaeresis)
decomposeCombining '\x00d9' = Just ('U', CombiningGraveAccent)
decomposeCombining '\x00da' = Just ('U', CombiningAcuteAccent)
decomposeCombining '\x00db' = Just ('U', CombiningCircumflexAccent)
decomposeCombining '\x00dc' = Just ('U', CombiningDiaeresis)
decomposeCombining '\x00dd' = Just ('Y', CombiningAcuteAccent)
decomposeCombining '\x00e0' = Just ('a', CombiningGraveAccent)
decomposeCombining '\x00e1' = Just ('a', CombiningAcuteAccent)
decomposeCombining '\x00e2' = Just ('a', CombiningCircumflexAccent)
decomposeCombining '\x00e3' = Just ('a', CombiningTilde)
decomposeCombining '\x00e4' = Just ('a', CombiningDiaeresis)
decomposeCombining '\x00e5' = Just ('a', CombiningRingAbove)
decomposeCombining '\x00e7' = Just ('c', CombiningCedilla)
decomposeCombining '\x00e8' = Just ('e', CombiningGraveAccent)
decomposeCombining '\x00e9' = Just ('e', CombiningAcuteAccent)
decomposeCombining '\x00ea' = Just ('e', CombiningCircumflexAccent)
decomposeCombining '\x00eb' = Just ('e', CombiningDiaeresis)
decomposeCombining '\x00ec' = Just ('i', CombiningGraveAccent)
decomposeCombining '\x00ed' = Just ('i', CombiningAcuteAccent)
decomposeCombining '\x00ee' = Just ('i', CombiningCircumflexAccent)
decomposeCombining '\x00ef' = Just ('i', CombiningDiaeresis)
decomposeCombining '\x00f1' = Just ('n', CombiningTilde)
decomposeCombining '\x00f2' = Just ('o', CombiningGraveAccent)
decomposeCombining '\x00f3' = Just ('o', CombiningAcuteAccent)
decomposeCombining '\x00f4' = Just ('o', CombiningCircumflexAccent)
decomposeCombining '\x00f5' = Just ('o', CombiningTilde)
decomposeCombining '\x00f6' = Just ('o', CombiningDiaeresis)
decomposeCombining '\x00f9' = Just ('u', CombiningGraveAccent)
decomposeCombining '\x00fa' = Just ('u', CombiningAcuteAccent)
decomposeCombining '\x00fb' = Just ('u', CombiningCircumflexAccent)
decomposeCombining '\x00fc' = Just ('u', CombiningDiaeresis)
decomposeCombining '\x00fd' = Just ('y', CombiningAcuteAccent)
decomposeCombining '\x00ff' = Just ('y', CombiningDiaeresis)
decomposeCombining '\x0100' = Just ('A', CombiningMacron)
decomposeCombining '\x0101' = Just ('a', CombiningMacron)
decomposeCombining '\x0102' = Just ('A', CombiningBreve)
decomposeCombining '\x0103' = Just ('a', CombiningBreve)
decomposeCombining '\x0104' = Just ('A', CombiningOgonek)
decomposeCombining '\x0105' = Just ('a', CombiningOgonek)
decomposeCombining '\x0106' = Just ('C', CombiningAcuteAccent)
decomposeCombining '\x0107' = Just ('c', CombiningAcuteAccent)
decomposeCombining '\x0108' = Just ('C', CombiningCircumflexAccent)
decomposeCombining '\x0109' = Just ('c', CombiningCircumflexAccent)
decomposeCombining '\x010a' = Just ('C', CombiningDotAbove)
decomposeCombining '\x010b' = Just ('c', CombiningDotAbove)
decomposeCombining '\x010c' = Just ('C', CombiningCaron)
decomposeCombining '\x010d' = Just ('c', CombiningCaron)
decomposeCombining '\x010e' = Just ('D', CombiningCaron)
decomposeCombining '\x010f' = Just ('d', CombiningCaron)
decomposeCombining '\x0112' = Just ('E', CombiningMacron)
decomposeCombining '\x0113' = Just ('e', CombiningMacron)
decomposeCombining '\x0114' = Just ('E', CombiningBreve)
decomposeCombining '\x0115' = Just ('e', CombiningBreve)
decomposeCombining '\x0116' = Just ('E', CombiningDotAbove)
decomposeCombining '\x0117' = Just ('e', CombiningDotAbove)
decomposeCombining '\x0118' = Just ('E', CombiningOgonek)
decomposeCombining '\x0119' = Just ('e', CombiningOgonek)
decomposeCombining '\x011a' = Just ('E', CombiningCaron)
decomposeCombining '\x011b' = Just ('e', CombiningCaron)
decomposeCombining '\x011c' = Just ('G', CombiningCircumflexAccent)
decomposeCombining '\x011d' = Just ('g', CombiningCircumflexAccent)
decomposeCombining '\x011e' = Just ('G', CombiningBreve)
decomposeCombining '\x011f' = Just ('g', CombiningBreve)
decomposeCombining '\x0120' = Just ('G', CombiningDotAbove)
decomposeCombining '\x0121' = Just ('g', CombiningDotAbove)
decomposeCombining '\x0122' = Just ('G', CombiningCedilla)
decomposeCombining '\x0123' = Just ('g', CombiningCedilla)
decomposeCombining '\x0124' = Just ('H', CombiningCircumflexAccent)
decomposeCombining '\x0125' = Just ('h', CombiningCircumflexAccent)
decomposeCombining '\x0128' = Just ('I', CombiningTilde)
decomposeCombining '\x0129' = Just ('i', CombiningTilde)
decomposeCombining '\x012a' = Just ('I', CombiningMacron)
decomposeCombining '\x012b' = Just ('i', CombiningMacron)
decomposeCombining '\x012c' = Just ('I', CombiningBreve)
decomposeCombining '\x012d' = Just ('i', CombiningBreve)
decomposeCombining '\x012e' = Just ('I', CombiningOgonek)
decomposeCombining '\x012f' = Just ('i', CombiningOgonek)
decomposeCombining '\x0130' = Just ('I', CombiningDotAbove)
decomposeCombining '\x0134' = Just ('J', CombiningCircumflexAccent)
decomposeCombining '\x0135' = Just ('j', CombiningCircumflexAccent)
decomposeCombining '\x0136' = Just ('K', CombiningCedilla)
decomposeCombining '\x0137' = Just ('k', CombiningCedilla)
decomposeCombining '\x0139' = Just ('L', CombiningAcuteAccent)
decomposeCombining '\x013a' = Just ('l', CombiningAcuteAccent)
decomposeCombining '\x013b' = Just ('L', CombiningCedilla)
decomposeCombining '\x013c' = Just ('l', CombiningCedilla)
decomposeCombining '\x013d' = Just ('L', CombiningCaron)
decomposeCombining '\x013e' = Just ('l', CombiningCaron)
decomposeCombining '\x0143' = Just ('N', CombiningAcuteAccent)
decomposeCombining '\x0144' = Just ('n', CombiningAcuteAccent)
decomposeCombining '\x0145' = Just ('N', CombiningCedilla)
decomposeCombining '\x0146' = Just ('n', CombiningCedilla)
decomposeCombining '\x0147' = Just ('N', CombiningCaron)
decomposeCombining '\x0148' = Just ('n', CombiningCaron)
decomposeCombining '\x014c' = Just ('O', CombiningMacron)
decomposeCombining '\x014d' = Just ('o', CombiningMacron)
decomposeCombining '\x014e' = Just ('O', CombiningBreve)
decomposeCombining '\x014f' = Just ('o', CombiningBreve)
decomposeCombining '\x0150' = Just ('O', CombiningDoubleAcuteAccent)
decomposeCombining '\x0151' = Just ('o', CombiningDoubleAcuteAccent)
decomposeCombining '\x0154' = Just ('R', CombiningAcuteAccent)
decomposeCombining '\x0155' = Just ('r', CombiningAcuteAccent)
decomposeCombining '\x0156' = Just ('R', CombiningCedilla)
decomposeCombining '\x0157' = Just ('r', CombiningCedilla)
decomposeCombining '\x0158' = Just ('R', CombiningCaron)
decomposeCombining '\x0159' = Just ('r', CombiningCaron)
decomposeCombining '\x015a' = Just ('S', CombiningAcuteAccent)
decomposeCombining '\x015b' = Just ('s', CombiningAcuteAccent)
decomposeCombining '\x015c' = Just ('S', CombiningCircumflexAccent)
decomposeCombining '\x015d' = Just ('s', CombiningCircumflexAccent)
decomposeCombining '\x015e' = Just ('S', CombiningCedilla)
decomposeCombining '\x015f' = Just ('s', CombiningCedilla)
decomposeCombining '\x0160' = Just ('S', CombiningCaron)
decomposeCombining '\x0161' = Just ('s', CombiningCaron)
decomposeCombining '\x0162' = Just ('T', CombiningCedilla)
decomposeCombining '\x0163' = Just ('t', CombiningCedilla)
decomposeCombining '\x0164' = Just ('T', CombiningCaron)
decomposeCombining '\x0165' = Just ('t', CombiningCaron)
decomposeCombining '\x0168' = Just ('U', CombiningTilde)
decomposeCombining '\x0169' = Just ('u', CombiningTilde)
decomposeCombining '\x016a' = Just ('U', CombiningMacron)
decomposeCombining '\x016b' = Just ('u', CombiningMacron)
decomposeCombining '\x016c' = Just ('U', CombiningBreve)
decomposeCombining '\x016d' = Just ('u', CombiningBreve)
decomposeCombining '\x016e' = Just ('U', CombiningRingAbove)
decomposeCombining '\x016f' = Just ('u', CombiningRingAbove)
decomposeCombining '\x0170' = Just ('U', CombiningDoubleAcuteAccent)
decomposeCombining '\x0171' = Just ('u', CombiningDoubleAcuteAccent)
decomposeCombining '\x0172' = Just ('U', CombiningOgonek)
decomposeCombining '\x0173' = Just ('u', CombiningOgonek)
decomposeCombining '\x0174' = Just ('W', CombiningCircumflexAccent)
decomposeCombining '\x0175' = Just ('w', CombiningCircumflexAccent)
decomposeCombining '\x0176' = Just ('Y', CombiningCircumflexAccent)
decomposeCombining '\x0177' = Just ('y', CombiningCircumflexAccent)
decomposeCombining '\x0178' = Just ('Y', CombiningDiaeresis)
decomposeCombining '\x0179' = Just ('Z', CombiningAcuteAccent)
decomposeCombining '\x017a' = Just ('z', CombiningAcuteAccent)
decomposeCombining '\x017b' = Just ('Z', CombiningDotAbove)
decomposeCombining '\x017c' = Just ('z', CombiningDotAbove)
decomposeCombining '\x017d' = Just ('Z', CombiningCaron)
decomposeCombining '\x017e' = Just ('z', CombiningCaron)
decomposeCombining '\x01a0' = Just ('O', CombiningHorn)
decomposeCombining '\x01a1' = Just ('o', CombiningHorn)
decomposeCombining '\x01af' = Just ('U', CombiningHorn)
decomposeCombining '\x01b0' = Just ('u', CombiningHorn)
decomposeCombining '\x01cd' = Just ('A', CombiningCaron)
decomposeCombining '\x01ce' = Just ('a', CombiningCaron)
decomposeCombining '\x01cf' = Just ('I', CombiningCaron)
decomposeCombining '\x01d0' = Just ('i', CombiningCaron)
decomposeCombining '\x01d1' = Just ('O', CombiningCaron)
decomposeCombining '\x01d2' = Just ('o', CombiningCaron)
decomposeCombining '\x01d3' = Just ('U', CombiningCaron)
decomposeCombining '\x01d4' = Just ('u', CombiningCaron)
decomposeCombining '\x01d5' = Just ('\x00dc', CombiningMacron)
decomposeCombining '\x01d6' = Just ('\x00fc', CombiningMacron)
decomposeCombining '\x01d7' = Just ('\x00dc', CombiningAcuteAccent)
decomposeCombining '\x01d8' = Just ('\x00fc', CombiningAcuteAccent)
decomposeCombining '\x01d9' = Just ('\x00dc', CombiningCaron)
decomposeCombining '\x01da' = Just ('\x00fc', CombiningCaron)
decomposeCombining '\x01db' = Just ('\x00dc', CombiningGraveAccent)
decomposeCombining '\x01dc' = Just ('\x00fc', CombiningGraveAccent)
decomposeCombining '\x01de' = Just ('\x00c4', CombiningMacron)
decomposeCombining '\x01df' = Just ('\x00e4', CombiningMacron)
decomposeCombining '\x01e0' = Just ('\x0226', CombiningMacron)
decomposeCombining '\x01e1' = Just ('\x0227', CombiningMacron)
decomposeCombining '\x01e2' = Just ('\x00c6', CombiningMacron)
decomposeCombining '\x01e3' = Just ('\x00e6', CombiningMacron)
decomposeCombining '\x01e6' = Just ('G', CombiningCaron)
decomposeCombining '\x01e7' = Just ('g', CombiningCaron)
decomposeCombining '\x01e8' = Just ('K', CombiningCaron)
decomposeCombining '\x01e9' = Just ('k', CombiningCaron)
decomposeCombining '\x01ea' = Just ('O', CombiningOgonek)
decomposeCombining '\x01eb' = Just ('o', CombiningOgonek)
decomposeCombining '\x01ec' = Just ('\x01ea', CombiningMacron)
decomposeCombining '\x01ed' = Just ('\x01eb', CombiningMacron)
decomposeCombining '\x01ee' = Just ('\x01b7', CombiningCaron)
decomposeCombining '\x01ef' = Just ('\x0292', CombiningCaron)
decomposeCombining '\x01f0' = Just ('j', CombiningCaron)
decomposeCombining '\x01f4' = Just ('G', CombiningAcuteAccent)
decomposeCombining '\x01f5' = Just ('g', CombiningAcuteAccent)
decomposeCombining '\x01f8' = Just ('N', CombiningGraveAccent)
decomposeCombining '\x01f9' = Just ('n', CombiningGraveAccent)
decomposeCombining '\x01fa' = Just ('\x00c5', CombiningAcuteAccent)
decomposeCombining '\x01fb' = Just ('\x00e5', CombiningAcuteAccent)
decomposeCombining '\x01fc' = Just ('\x00c6', CombiningAcuteAccent)
decomposeCombining '\x01fd' = Just ('\x00e6', CombiningAcuteAccent)
decomposeCombining '\x01fe' = Just ('\x00d8', CombiningAcuteAccent)
decomposeCombining '\x01ff' = Just ('\x00f8', CombiningAcuteAccent)
decomposeCombining '\x0200' = Just ('A', CombiningDoubleGraveAccent)
decomposeCombining '\x0201' = Just ('a', CombiningDoubleGraveAccent)
decomposeCombining '\x0202' = Just ('A', CombiningInvertedBreve)
decomposeCombining '\x0203' = Just ('a', CombiningInvertedBreve)
decomposeCombining '\x0204' = Just ('E', CombiningDoubleGraveAccent)
decomposeCombining '\x0205' = Just ('e', CombiningDoubleGraveAccent)
decomposeCombining '\x0206' = Just ('E', CombiningInvertedBreve)
decomposeCombining '\x0207' = Just ('e', CombiningInvertedBreve)
decomposeCombining '\x0208' = Just ('I', CombiningDoubleGraveAccent)
decomposeCombining '\x0209' = Just ('i', CombiningDoubleGraveAccent)
decomposeCombining '\x020a' = Just ('I', CombiningInvertedBreve)
decomposeCombining '\x020b' = Just ('i', CombiningInvertedBreve)
decomposeCombining '\x020c' = Just ('O', CombiningDoubleGraveAccent)
decomposeCombining '\x020d' = Just ('o', CombiningDoubleGraveAccent)
decomposeCombining '\x020e' = Just ('O', CombiningInvertedBreve)
decomposeCombining '\x020f' = Just ('o', CombiningInvertedBreve)
decomposeCombining '\x0210' = Just ('R', CombiningDoubleGraveAccent)
decomposeCombining '\x0211' = Just ('r', CombiningDoubleGraveAccent)
decomposeCombining '\x0212' = Just ('R', CombiningInvertedBreve)
decomposeCombining '\x0213' = Just ('r', CombiningInvertedBreve)
decomposeCombining '\x0214' = Just ('U', CombiningDoubleGraveAccent)
decomposeCombining '\x0215' = Just ('u', CombiningDoubleGraveAccent)
decomposeCombining '\x0216' = Just ('U', CombiningInvertedBreve)
decomposeCombining '\x0217' = Just ('u', CombiningInvertedBreve)
decomposeCombining '\x0218' = Just ('S', CombiningCommaBelow)
decomposeCombining '\x0219' = Just ('s', CombiningCommaBelow)
decomposeCombining '\x021a' = Just ('T', CombiningCommaBelow)
decomposeCombining '\x021b' = Just ('t', CombiningCommaBelow)
decomposeCombining '\x021e' = Just ('H', CombiningCaron)
decomposeCombining '\x021f' = Just ('h', CombiningCaron)
decomposeCombining '\x0226' = Just ('A', CombiningDotAbove)
decomposeCombining '\x0227' = Just ('a', CombiningDotAbove)
decomposeCombining '\x0228' = Just ('E', CombiningCedilla)
decomposeCombining '\x0229' = Just ('e', CombiningCedilla)
decomposeCombining '\x022a' = Just ('\x00d6', CombiningMacron)
decomposeCombining '\x022b' = Just ('\x00f6', CombiningMacron)
decomposeCombining '\x022c' = Just ('\x00d5', CombiningMacron)
decomposeCombining '\x022d' = Just ('\x00f5', CombiningMacron)
decomposeCombining '\x022e' = Just ('O', CombiningDotAbove)
decomposeCombining '\x022f' = Just ('o', CombiningDotAbove)
decomposeCombining '\x0230' = Just ('\x022e', CombiningMacron)
decomposeCombining '\x0231' = Just ('\x022f', CombiningMacron)
decomposeCombining '\x0232' = Just ('Y', CombiningMacron)
decomposeCombining '\x0233' = Just ('y', CombiningMacron)
decomposeCombining '\x0344' = Just ('\x0308', CombiningAcuteAccent)
decomposeCombining '\x0385' = Just ('\x00a8', CombiningAcuteAccent)
decomposeCombining '\x0386' = Just ('\x0391', CombiningAcuteAccent)
decomposeCombining '\x0388' = Just ('\x0395', CombiningAcuteAccent)
decomposeCombining '\x0389' = Just ('\x0397', CombiningAcuteAccent)
decomposeCombining '\x038a' = Just ('\x0399', CombiningAcuteAccent)
decomposeCombining '\x038c' = Just ('\x039f', CombiningAcuteAccent)
decomposeCombining '\x038e' = Just ('\x03a5', CombiningAcuteAccent)
decomposeCombining '\x038f' = Just ('\x03a9', CombiningAcuteAccent)
decomposeCombining '\x0390' = Just ('\x03ca', CombiningAcuteAccent)
decomposeCombining '\x03aa' = Just ('\x0399', CombiningDiaeresis)
decomposeCombining '\x03ab' = Just ('\x03a5', CombiningDiaeresis)
decomposeCombining '\x03ac' = Just ('\x03b1', CombiningAcuteAccent)
decomposeCombining '\x03ad' = Just ('\x03b5', CombiningAcuteAccent)
decomposeCombining '\x03ae' = Just ('\x03b7', CombiningAcuteAccent)
decomposeCombining '\x03af' = Just ('\x03b9', CombiningAcuteAccent)
decomposeCombining '\x03b0' = Just ('\x03cb', CombiningAcuteAccent)
decomposeCombining '\x03ca' = Just ('\x03b9', CombiningDiaeresis)
decomposeCombining '\x03cb' = Just ('\x03c5', CombiningDiaeresis)
decomposeCombining '\x03cc' = Just ('\x03bf', CombiningAcuteAccent)
decomposeCombining '\x03cd' = Just ('\x03c5', CombiningAcuteAccent)
decomposeCombining '\x03ce' = Just ('\x03c9', CombiningAcuteAccent)
decomposeCombining '\x03d3' = Just ('\x03d2', CombiningAcuteAccent)
decomposeCombining '\x03d4' = Just ('\x03d2', CombiningDiaeresis)
decomposeCombining '\x0400' = Just ('\x0415', CombiningGraveAccent)
decomposeCombining '\x0401' = Just ('\x0415', CombiningDiaeresis)
decomposeCombining '\x0403' = Just ('\x0413', CombiningAcuteAccent)
decomposeCombining '\x0407' = Just ('\x0406', CombiningDiaeresis)
decomposeCombining '\x040c' = Just ('\x041a', CombiningAcuteAccent)
decomposeCombining '\x040d' = Just ('\x0418', CombiningGraveAccent)
decomposeCombining '\x040e' = Just ('\x0423', CombiningBreve)
decomposeCombining '\x0419' = Just ('\x0418', CombiningBreve)
decomposeCombining '\x0439' = Just ('\x0438', CombiningBreve)
decomposeCombining '\x0450' = Just ('\x0435', CombiningGraveAccent)
decomposeCombining '\x0451' = Just ('\x0435', CombiningDiaeresis)
decomposeCombining '\x0453' = Just ('\x0433', CombiningAcuteAccent)
decomposeCombining '\x0457' = Just ('\x0456', CombiningDiaeresis)
decomposeCombining '\x045c' = Just ('\x043a', CombiningAcuteAccent)
decomposeCombining '\x045d' = Just ('\x0438', CombiningGraveAccent)
decomposeCombining '\x045e' = Just ('\x0443', CombiningBreve)
decomposeCombining '\x0476' = Just ('\x0474', CombiningDoubleGraveAccent)
decomposeCombining '\x0477' = Just ('\x0475', CombiningDoubleGraveAccent)
decomposeCombining '\x04c1' = Just ('\x0416', CombiningBreve)
decomposeCombining '\x04c2' = Just ('\x0436', CombiningBreve)
decomposeCombining '\x04d0' = Just ('\x0410', CombiningBreve)
decomposeCombining '\x04d1' = Just ('\x0430', CombiningBreve)
decomposeCombining '\x04d2' = Just ('\x0410', CombiningDiaeresis)
decomposeCombining '\x04d3' = Just ('\x0430', CombiningDiaeresis)
decomposeCombining '\x04d6' = Just ('\x0415', CombiningBreve)
decomposeCombining '\x04d7' = Just ('\x0435', CombiningBreve)
decomposeCombining '\x04da' = Just ('\x04d8', CombiningDiaeresis)
decomposeCombining '\x04db' = Just ('\x04d9', CombiningDiaeresis)
decomposeCombining '\x04dc' = Just ('\x0416', CombiningDiaeresis)
decomposeCombining '\x04dd' = Just ('\x0436', CombiningDiaeresis)
decomposeCombining '\x04de' = Just ('\x0417', CombiningDiaeresis)
decomposeCombining '\x04df' = Just ('\x0437', CombiningDiaeresis)
decomposeCombining '\x04e2' = Just ('\x0418', CombiningMacron)
decomposeCombining '\x04e3' = Just ('\x0438', CombiningMacron)
decomposeCombining '\x04e4' = Just ('\x0418', CombiningDiaeresis)
decomposeCombining '\x04e5' = Just ('\x0438', CombiningDiaeresis)
decomposeCombining '\x04e6' = Just ('\x041e', CombiningDiaeresis)
decomposeCombining '\x04e7' = Just ('\x043e', CombiningDiaeresis)
decomposeCombining '\x04ea' = Just ('\x04e8', CombiningDiaeresis)
decomposeCombining '\x04eb' = Just ('\x04e9', CombiningDiaeresis)
decomposeCombining '\x04ec' = Just ('\x042d', CombiningDiaeresis)
decomposeCombining '\x04ed' = Just ('\x044d', CombiningDiaeresis)
decomposeCombining '\x04ee' = Just ('\x0423', CombiningMacron)
decomposeCombining '\x04ef' = Just ('\x0443', CombiningMacron)
decomposeCombining '\x04f0' = Just ('\x0423', CombiningDiaeresis)
decomposeCombining '\x04f1' = Just ('\x0443', CombiningDiaeresis)
decomposeCombining '\x04f2' = Just ('\x0423', CombiningDoubleAcuteAccent)
decomposeCombining '\x04f3' = Just ('\x0443', CombiningDoubleAcuteAccent)
decomposeCombining '\x04f4' = Just ('\x0427', CombiningDiaeresis)
decomposeCombining '\x04f5' = Just ('\x0447', CombiningDiaeresis)
decomposeCombining '\x04f8' = Just ('\x042b', CombiningDiaeresis)
decomposeCombining '\x04f9' = Just ('\x044b', CombiningDiaeresis)
decomposeCombining '\x0622' = Just ('\x0627', ArabicMaddahAbove)
decomposeCombining '\x0623' = Just ('\x0627', ArabicHamzaAbove)
decomposeCombining '\x0624' = Just ('\x0648', ArabicHamzaAbove)
decomposeCombining '\x0625' = Just ('\x0627', ArabicHamzaBelow)
decomposeCombining '\x0626' = Just ('\x064a', ArabicHamzaAbove)
decomposeCombining '\x06c0' = Just ('\x06d5', ArabicHamzaAbove)
decomposeCombining '\x06c2' = Just ('\x06c1', ArabicHamzaAbove)
decomposeCombining '\x06d3' = Just ('\x06d2', ArabicHamzaAbove)
decomposeCombining '\x0929' = Just ('\x0928', DevanagariSignNukta)
decomposeCombining '\x0931' = Just ('\x0930', DevanagariSignNukta)
decomposeCombining '\x0934' = Just ('\x0933', DevanagariSignNukta)
decomposeCombining '\x0958' = Just ('\x0915', DevanagariSignNukta)
decomposeCombining '\x0959' = Just ('\x0916', DevanagariSignNukta)
decomposeCombining '\x095a' = Just ('\x0917', DevanagariSignNukta)
decomposeCombining '\x095b' = Just ('\x091c', DevanagariSignNukta)
decomposeCombining '\x095c' = Just ('\x0921', DevanagariSignNukta)
decomposeCombining '\x095d' = Just ('\x0922', DevanagariSignNukta)
decomposeCombining '\x095e' = Just ('\x092b', DevanagariSignNukta)
decomposeCombining '\x095f' = Just ('\x092f', DevanagariSignNukta)
decomposeCombining '\x09cb' = Just ('\x09c7', BengaliVowelSignAa)
decomposeCombining '\x09cc' = Just ('\x09c7', BengaliAuLengthMark)
decomposeCombining '\x09dc' = Just ('\x09a1', BengaliSignNukta)
decomposeCombining '\x09dd' = Just ('\x09a2', BengaliSignNukta)
decomposeCombining '\x09df' = Just ('\x09af', BengaliSignNukta)
decomposeCombining '\x0a33' = Just ('\x0a32', GurmukhiSignNukta)
decomposeCombining '\x0a36' = Just ('\x0a38', GurmukhiSignNukta)
decomposeCombining '\x0a59' = Just ('\x0a16', GurmukhiSignNukta)
decomposeCombining '\x0a5a' = Just ('\x0a17', GurmukhiSignNukta)
decomposeCombining '\x0a5b' = Just ('\x0a1c', GurmukhiSignNukta)
decomposeCombining '\x0a5e' = Just ('\x0a2b', GurmukhiSignNukta)
decomposeCombining '\x0b48' = Just ('\x0b47', OriyaAiLengthMark)
decomposeCombining '\x0b4b' = Just ('\x0b47', OriyaVowelSignAa)
decomposeCombining '\x0b4c' = Just ('\x0b47', OriyaAuLengthMark)
decomposeCombining '\x0b5c' = Just ('\x0b21', OriyaSignNukta)
decomposeCombining '\x0b5d' = Just ('\x0b22', OriyaSignNukta)
decomposeCombining '\x0b94' = Just ('\x0b92', TamilAuLengthMark)
decomposeCombining '\x0bca' = Just ('\x0bc6', TamilVowelSignAa)
decomposeCombining '\x0bcb' = Just ('\x0bc7', TamilVowelSignAa)
decomposeCombining '\x0bcc' = Just ('\x0bc6', TamilAuLengthMark)
decomposeCombining '\x0c48' = Just ('\x0c46', TeluguAiLengthMark)
decomposeCombining '\x0cc0' = Just ('\x0cbf', KannadaLengthMark)
decomposeCombining '\x0cc7' = Just ('\x0cc6', KannadaLengthMark)
decomposeCombining '\x0cc8' = Just ('\x0cc6', KannadaAiLengthMark)
decomposeCombining '\x0cca' = Just ('\x0cc6', KannadaVowelSignUu)
decomposeCombining '\x0ccb' = Just ('\x0cca', KannadaLengthMark)
decomposeCombining '\x0d4a' = Just ('\x0d46', MalayalamVowelSignAa)
decomposeCombining '\x0d4b' = Just ('\x0d47', MalayalamVowelSignAa)
decomposeCombining '\x0d4c' = Just ('\x0d46', MalayalamAuLengthMark)
decomposeCombining '\x0dda' = Just ('\x0dd9', SinhalaSignAlLakuna)
decomposeCombining '\x0ddc' = Just ('\x0dd9', SinhalaVowelSignAelaPilla)
decomposeCombining '\x0ddd' = Just ('\x0ddc', SinhalaSignAlLakuna)
decomposeCombining '\x0dde' = Just ('\x0dd9', SinhalaVowelSignGayanukitta)
decomposeCombining '\x0f43' = Just ('\x0f42', TibetanSubjoinedLetterHa)
decomposeCombining '\x0f4d' = Just ('\x0f4c', TibetanSubjoinedLetterHa)
decomposeCombining '\x0f52' = Just ('\x0f51', TibetanSubjoinedLetterHa)
decomposeCombining '\x0f57' = Just ('\x0f56', TibetanSubjoinedLetterHa)
decomposeCombining '\x0f5c' = Just ('\x0f5b', TibetanSubjoinedLetterHa)
decomposeCombining '\x0f69' = Just ('\x0f40', TibetanSubjoinedLetterSsa)
decomposeCombining '\x0f73' = Just ('\x0f71', TibetanVowelSignI)
decomposeCombining '\x0f75' = Just ('\x0f71', TibetanVowelSignU)
decomposeCombining '\x0f76' = Just ('\x0fb2', TibetanVowelSignReversedI)
decomposeCombining '\x0f78' = Just ('\x0fb3', TibetanVowelSignReversedI)
decomposeCombining '\x0f81' = Just ('\x0f71', TibetanVowelSignReversedI)
decomposeCombining '\x0f93' = Just ('\x0f92', TibetanSubjoinedLetterHa)
decomposeCombining '\x0f9d' = Just ('\x0f9c', TibetanSubjoinedLetterHa)
decomposeCombining '\x0fa2' = Just ('\x0fa1', TibetanSubjoinedLetterHa)
decomposeCombining '\x0fa7' = Just ('\x0fa6', TibetanSubjoinedLetterHa)
decomposeCombining '\x0fac' = Just ('\x0fab', TibetanSubjoinedLetterHa)
decomposeCombining '\x0fb9' = Just ('\x0f90', TibetanSubjoinedLetterSsa)
decomposeCombining '\x1026' = Just ('\x1025', MyanmarVowelSignIi)
decomposeCombining '\x1b06' = Just ('\x1b05', BalineseVowelSignTedung)
decomposeCombining '\x1b08' = Just ('\x1b07', BalineseVowelSignTedung)
decomposeCombining '\x1b0a' = Just ('\x1b09', BalineseVowelSignTedung)
decomposeCombining '\x1b0c' = Just ('\x1b0b', BalineseVowelSignTedung)
decomposeCombining '\x1b0e' = Just ('\x1b0d', BalineseVowelSignTedung)
decomposeCombining '\x1b12' = Just ('\x1b11', BalineseVowelSignTedung)
decomposeCombining '\x1b3b' = Just ('\x1b3a', BalineseVowelSignTedung)
decomposeCombining '\x1b3d' = Just ('\x1b3c', BalineseVowelSignTedung)
decomposeCombining '\x1b40' = Just ('\x1b3e', BalineseVowelSignTedung)
decomposeCombining '\x1b41' = Just ('\x1b3f', BalineseVowelSignTedung)
decomposeCombining '\x1b43' = Just ('\x1b42', BalineseVowelSignTedung)
decomposeCombining '\x1e00' = Just ('A', CombiningRingBelow)
decomposeCombining '\x1e01' = Just ('a', CombiningRingBelow)
decomposeCombining '\x1e02' = Just ('B', CombiningDotAbove)
decomposeCombining '\x1e03' = Just ('b', CombiningDotAbove)
decomposeCombining '\x1e04' = Just ('B', CombiningDotBelow)
decomposeCombining '\x1e05' = Just ('b', CombiningDotBelow)
decomposeCombining '\x1e06' = Just ('B', CombiningMacronBelow)
decomposeCombining '\x1e07' = Just ('b', CombiningMacronBelow)
decomposeCombining '\x1e08' = Just ('\x00c7', CombiningAcuteAccent)
decomposeCombining '\x1e09' = Just ('\x00e7', CombiningAcuteAccent)
decomposeCombining '\x1e0a' = Just ('D', CombiningDotAbove)
decomposeCombining '\x1e0b' = Just ('d', CombiningDotAbove)
decomposeCombining '\x1e0c' = Just ('D', CombiningDotBelow)
decomposeCombining '\x1e0d' = Just ('d', CombiningDotBelow)
decomposeCombining '\x1e0e' = Just ('D', CombiningMacronBelow)
decomposeCombining '\x1e0f' = Just ('d', CombiningMacronBelow)
decomposeCombining '\x1e10' = Just ('D', CombiningCedilla)
decomposeCombining '\x1e11' = Just ('d', CombiningCedilla)
decomposeCombining '\x1e12' = Just ('D', CombiningCircumflexAccentBelow)
decomposeCombining '\x1e13' = Just ('d', CombiningCircumflexAccentBelow)
decomposeCombining '\x1e14' = Just ('\x0112', CombiningGraveAccent)
decomposeCombining '\x1e15' = Just ('\x0113', CombiningGraveAccent)
decomposeCombining '\x1e16' = Just ('\x0112', CombiningAcuteAccent)
decomposeCombining '\x1e17' = Just ('\x0113', CombiningAcuteAccent)
decomposeCombining '\x1e18' = Just ('E', CombiningCircumflexAccentBelow)
decomposeCombining '\x1e19' = Just ('e', CombiningCircumflexAccentBelow)
decomposeCombining '\x1e1a' = Just ('E', CombiningTildeBelow)
decomposeCombining '\x1e1b' = Just ('e', CombiningTildeBelow)
decomposeCombining '\x1e1c' = Just ('\x0228', CombiningBreve)
decomposeCombining '\x1e1d' = Just ('\x0229', CombiningBreve)
decomposeCombining '\x1e1e' = Just ('F', CombiningDotAbove)
decomposeCombining '\x1e1f' = Just ('f', CombiningDotAbove)
decomposeCombining '\x1e20' = Just ('G', CombiningMacron)
decomposeCombining '\x1e21' = Just ('g', CombiningMacron)
decomposeCombining '\x1e22' = Just ('H', CombiningDotAbove)
decomposeCombining '\x1e23' = Just ('h', CombiningDotAbove)
decomposeCombining '\x1e24' = Just ('H', CombiningDotBelow)
decomposeCombining '\x1e25' = Just ('h', CombiningDotBelow)
decomposeCombining '\x1e26' = Just ('H', CombiningDiaeresis)
decomposeCombining '\x1e27' = Just ('h', CombiningDiaeresis)
decomposeCombining '\x1e28' = Just ('H', CombiningCedilla)
decomposeCombining '\x1e29' = Just ('h', CombiningCedilla)
decomposeCombining '\x1e2a' = Just ('H', CombiningBreveBelow)
decomposeCombining '\x1e2b' = Just ('h', CombiningBreveBelow)
decomposeCombining '\x1e2c' = Just ('I', CombiningTildeBelow)
decomposeCombining '\x1e2d' = Just ('i', CombiningTildeBelow)
decomposeCombining '\x1e2e' = Just ('\x00cf', CombiningAcuteAccent)
decomposeCombining '\x1e2f' = Just ('\x00ef', CombiningAcuteAccent)
decomposeCombining '\x1e30' = Just ('K', CombiningAcuteAccent)
decomposeCombining '\x1e31' = Just ('k', CombiningAcuteAccent)
decomposeCombining '\x1e32' = Just ('K', CombiningDotBelow)
decomposeCombining '\x1e33' = Just ('k', CombiningDotBelow)
decomposeCombining '\x1e34' = Just ('K', CombiningMacronBelow)
decomposeCombining '\x1e35' = Just ('k', CombiningMacronBelow)
decomposeCombining '\x1e36' = Just ('L', CombiningDotBelow)
decomposeCombining '\x1e37' = Just ('l', CombiningDotBelow)
decomposeCombining '\x1e38' = Just ('\x1e36', CombiningMacron)
decomposeCombining '\x1e39' = Just ('\x1e37', CombiningMacron)
decomposeCombining '\x1e3a' = Just ('L', CombiningMacronBelow)
decomposeCombining '\x1e3b' = Just ('l', CombiningMacronBelow)
decomposeCombining '\x1e3c' = Just ('L', CombiningCircumflexAccentBelow)
decomposeCombining '\x1e3d' = Just ('l', CombiningCircumflexAccentBelow)
decomposeCombining '\x1e3e' = Just ('M', CombiningAcuteAccent)
decomposeCombining '\x1e3f' = Just ('m', CombiningAcuteAccent)
decomposeCombining '\x1e40' = Just ('M', CombiningDotAbove)
decomposeCombining '\x1e41' = Just ('m', CombiningDotAbove)
decomposeCombining '\x1e42' = Just ('M', CombiningDotBelow)
decomposeCombining '\x1e43' = Just ('m', CombiningDotBelow)
decomposeCombining '\x1e44' = Just ('N', CombiningDotAbove)
decomposeCombining '\x1e45' = Just ('n', CombiningDotAbove)
decomposeCombining '\x1e46' = Just ('N', CombiningDotBelow)
decomposeCombining '\x1e47' = Just ('n', CombiningDotBelow)
decomposeCombining '\x1e48' = Just ('N', CombiningMacronBelow)
decomposeCombining '\x1e49' = Just ('n', CombiningMacronBelow)
decomposeCombining '\x1e4a' = Just ('N', CombiningCircumflexAccentBelow)
decomposeCombining '\x1e4b' = Just ('n', CombiningCircumflexAccentBelow)
decomposeCombining '\x1e4c' = Just ('\x00d5', CombiningAcuteAccent)
decomposeCombining '\x1e4d' = Just ('\x00f5', CombiningAcuteAccent)
decomposeCombining '\x1e4e' = Just ('\x00d5', CombiningDiaeresis)
decomposeCombining '\x1e4f' = Just ('\x00f5', CombiningDiaeresis)
decomposeCombining '\x1e50' = Just ('\x014c', CombiningGraveAccent)
decomposeCombining '\x1e51' = Just ('\x014d', CombiningGraveAccent)
decomposeCombining '\x1e52' = Just ('\x014c', CombiningAcuteAccent)
decomposeCombining '\x1e53' = Just ('\x014d', CombiningAcuteAccent)
decomposeCombining '\x1e54' = Just ('P', CombiningAcuteAccent)
decomposeCombining '\x1e55' = Just ('p', CombiningAcuteAccent)
decomposeCombining '\x1e56' = Just ('P', CombiningDotAbove)
decomposeCombining '\x1e57' = Just ('p', CombiningDotAbove)
decomposeCombining '\x1e58' = Just ('R', CombiningDotAbove)
decomposeCombining '\x1e59' = Just ('r', CombiningDotAbove)
decomposeCombining '\x1e5a' = Just ('R', CombiningDotBelow)
decomposeCombining '\x1e5b' = Just ('r', CombiningDotBelow)
decomposeCombining '\x1e5c' = Just ('\x1e5a', CombiningMacron)
decomposeCombining '\x1e5d' = Just ('\x1e5b', CombiningMacron)
decomposeCombining '\x1e5e' = Just ('R', CombiningMacronBelow)
decomposeCombining '\x1e5f' = Just ('r', CombiningMacronBelow)
decomposeCombining '\x1e60' = Just ('S', CombiningDotAbove)
decomposeCombining '\x1e61' = Just ('s', CombiningDotAbove)
decomposeCombining '\x1e62' = Just ('S', CombiningDotBelow)
decomposeCombining '\x1e63' = Just ('s', CombiningDotBelow)
decomposeCombining '\x1e64' = Just ('\x015a', CombiningDotAbove)
decomposeCombining '\x1e65' = Just ('\x015b', CombiningDotAbove)
decomposeCombining '\x1e66' = Just ('\x0160', CombiningDotAbove)
decomposeCombining '\x1e67' = Just ('\x0161', CombiningDotAbove)
decomposeCombining '\x1e68' = Just ('\x1e62', CombiningDotAbove)
decomposeCombining '\x1e69' = Just ('\x1e63', CombiningDotAbove)
decomposeCombining '\x1e6a' = Just ('T', CombiningDotAbove)
decomposeCombining '\x1e6b' = Just ('t', CombiningDotAbove)
decomposeCombining '\x1e6c' = Just ('T', CombiningDotBelow)
decomposeCombining '\x1e6d' = Just ('t', CombiningDotBelow)
decomposeCombining '\x1e6e' = Just ('T', CombiningMacronBelow)
decomposeCombining '\x1e6f' = Just ('t', CombiningMacronBelow)
decomposeCombining '\x1e70' = Just ('T', CombiningCircumflexAccentBelow)
decomposeCombining '\x1e71' = Just ('t', CombiningCircumflexAccentBelow)
decomposeCombining '\x1e72' = Just ('U', CombiningDiaeresisBelow)
decomposeCombining '\x1e73' = Just ('u', CombiningDiaeresisBelow)
decomposeCombining '\x1e74' = Just ('U', CombiningTildeBelow)
decomposeCombining '\x1e75' = Just ('u', CombiningTildeBelow)
decomposeCombining '\x1e76' = Just ('U', CombiningCircumflexAccentBelow)
decomposeCombining '\x1e77' = Just ('u', CombiningCircumflexAccentBelow)
decomposeCombining '\x1e78' = Just ('\x0168', CombiningAcuteAccent)
decomposeCombining '\x1e79' = Just ('\x0169', CombiningAcuteAccent)
decomposeCombining '\x1e7a' = Just ('\x016a', CombiningDiaeresis)
decomposeCombining '\x1e7b' = Just ('\x016b', CombiningDiaeresis)
decomposeCombining '\x1e7c' = Just ('V', CombiningTilde)
decomposeCombining '\x1e7d' = Just ('v', CombiningTilde)
decomposeCombining '\x1e7e' = Just ('V', CombiningDotBelow)
decomposeCombining '\x1e7f' = Just ('v', CombiningDotBelow)
decomposeCombining '\x1e80' = Just ('W', CombiningGraveAccent)
decomposeCombining '\x1e81' = Just ('w', CombiningGraveAccent)
decomposeCombining '\x1e82' = Just ('W', CombiningAcuteAccent)
decomposeCombining '\x1e83' = Just ('w', CombiningAcuteAccent)
decomposeCombining '\x1e84' = Just ('W', CombiningDiaeresis)
decomposeCombining '\x1e85' = Just ('w', CombiningDiaeresis)
decomposeCombining '\x1e86' = Just ('W', CombiningDotAbove)
decomposeCombining '\x1e87' = Just ('w', CombiningDotAbove)
decomposeCombining '\x1e88' = Just ('W', CombiningDotBelow)
decomposeCombining '\x1e89' = Just ('w', CombiningDotBelow)
decomposeCombining '\x1e8a' = Just ('X', CombiningDotAbove)
decomposeCombining '\x1e8b' = Just ('x', CombiningDotAbove)
decomposeCombining '\x1e8c' = Just ('X', CombiningDiaeresis)
decomposeCombining '\x1e8d' = Just ('x', CombiningDiaeresis)
decomposeCombining '\x1e8e' = Just ('Y', CombiningDotAbove)
decomposeCombining '\x1e8f' = Just ('y', CombiningDotAbove)
decomposeCombining '\x1e90' = Just ('Z', CombiningCircumflexAccent)
decomposeCombining '\x1e91' = Just ('z', CombiningCircumflexAccent)
decomposeCombining '\x1e92' = Just ('Z', CombiningDotBelow)
decomposeCombining '\x1e93' = Just ('z', CombiningDotBelow)
decomposeCombining '\x1e94' = Just ('Z', CombiningMacronBelow)
decomposeCombining '\x1e95' = Just ('z', CombiningMacronBelow)
decomposeCombining '\x1e96' = Just ('h', CombiningMacronBelow)
decomposeCombining '\x1e97' = Just ('t', CombiningDiaeresis)
decomposeCombining '\x1e98' = Just ('w', CombiningRingAbove)
decomposeCombining '\x1e99' = Just ('y', CombiningRingAbove)
decomposeCombining '\x1e9b' = Just ('\x017f', CombiningDotAbove)
decomposeCombining '\x1ea0' = Just ('A', CombiningDotBelow)
decomposeCombining '\x1ea1' = Just ('a', CombiningDotBelow)
decomposeCombining '\x1ea2' = Just ('A', CombiningHookAbove)
decomposeCombining '\x1ea3' = Just ('a', CombiningHookAbove)
decomposeCombining '\x1ea4' = Just ('\x00c2', CombiningAcuteAccent)
decomposeCombining '\x1ea5' = Just ('\x00e2', CombiningAcuteAccent)
decomposeCombining '\x1ea6' = Just ('\x00c2', CombiningGraveAccent)
decomposeCombining '\x1ea7' = Just ('\x00e2', CombiningGraveAccent)
decomposeCombining '\x1ea8' = Just ('\x00c2', CombiningHookAbove)
decomposeCombining '\x1ea9' = Just ('\x00e2', CombiningHookAbove)
decomposeCombining '\x1eaa' = Just ('\x00c2', CombiningTilde)
decomposeCombining '\x1eab' = Just ('\x00e2', CombiningTilde)
decomposeCombining '\x1eac' = Just ('\x1ea0', CombiningCircumflexAccent)
decomposeCombining '\x1ead' = Just ('\x1ea1', CombiningCircumflexAccent)
decomposeCombining '\x1eae' = Just ('\x0102', CombiningAcuteAccent)
decomposeCombining '\x1eaf' = Just ('\x0103', CombiningAcuteAccent)
decomposeCombining '\x1eb0' = Just ('\x0102', CombiningGraveAccent)
decomposeCombining '\x1eb1' = Just ('\x0103', CombiningGraveAccent)
decomposeCombining '\x1eb2' = Just ('\x0102', CombiningHookAbove)
decomposeCombining '\x1eb3' = Just ('\x0103', CombiningHookAbove)
decomposeCombining '\x1eb4' = Just ('\x0102', CombiningTilde)
decomposeCombining '\x1eb5' = Just ('\x0103', CombiningTilde)
decomposeCombining '\x1eb6' = Just ('\x1ea0', CombiningBreve)
decomposeCombining '\x1eb7' = Just ('\x1ea1', CombiningBreve)
decomposeCombining '\x1eb8' = Just ('E', CombiningDotBelow)
decomposeCombining '\x1eb9' = Just ('e', CombiningDotBelow)
decomposeCombining '\x1eba' = Just ('E', CombiningHookAbove)
decomposeCombining '\x1ebb' = Just ('e', CombiningHookAbove)
decomposeCombining '\x1ebc' = Just ('E', CombiningTilde)
decomposeCombining '\x1ebd' = Just ('e', CombiningTilde)
decomposeCombining '\x1ebe' = Just ('\x00ca', CombiningAcuteAccent)
decomposeCombining '\x1ebf' = Just ('\x00ea', CombiningAcuteAccent)
decomposeCombining '\x1ec0' = Just ('\x00ca', CombiningGraveAccent)
decomposeCombining '\x1ec1' = Just ('\x00ea', CombiningGraveAccent)
decomposeCombining '\x1ec2' = Just ('\x00ca', CombiningHookAbove)
decomposeCombining '\x1ec3' = Just ('\x00ea', CombiningHookAbove)
decomposeCombining '\x1ec4' = Just ('\x00ca', CombiningTilde)
decomposeCombining '\x1ec5' = Just ('\x00ea', CombiningTilde)
decomposeCombining '\x1ec6' = Just ('\x1eb8', CombiningCircumflexAccent)
decomposeCombining '\x1ec7' = Just ('\x1eb9', CombiningCircumflexAccent)
decomposeCombining '\x1ec8' = Just ('I', CombiningHookAbove)
decomposeCombining '\x1ec9' = Just ('i', CombiningHookAbove)
decomposeCombining '\x1eca' = Just ('I', CombiningDotBelow)
decomposeCombining '\x1ecb' = Just ('i', CombiningDotBelow)
decomposeCombining '\x1ecc' = Just ('O', CombiningDotBelow)
decomposeCombining '\x1ecd' = Just ('o', CombiningDotBelow)
decomposeCombining '\x1ece' = Just ('O', CombiningHookAbove)
decomposeCombining '\x1ecf' = Just ('o', CombiningHookAbove)
decomposeCombining '\x1ed0' = Just ('\x00d4', CombiningAcuteAccent)
decomposeCombining '\x1ed1' = Just ('\x00f4', CombiningAcuteAccent)
decomposeCombining '\x1ed2' = Just ('\x00d4', CombiningGraveAccent)
decomposeCombining '\x1ed3' = Just ('\x00f4', CombiningGraveAccent)
decomposeCombining '\x1ed4' = Just ('\x00d4', CombiningHookAbove)
decomposeCombining '\x1ed5' = Just ('\x00f4', CombiningHookAbove)
decomposeCombining '\x1ed6' = Just ('\x00d4', CombiningTilde)
decomposeCombining '\x1ed7' = Just ('\x00f4', CombiningTilde)
decomposeCombining '\x1ed8' = Just ('\x1ecc', CombiningCircumflexAccent)
decomposeCombining '\x1ed9' = Just ('\x1ecd', CombiningCircumflexAccent)
decomposeCombining '\x1eda' = Just ('\x01a0', CombiningAcuteAccent)
decomposeCombining '\x1edb' = Just ('\x01a1', CombiningAcuteAccent)
decomposeCombining '\x1edc' = Just ('\x01a0', CombiningGraveAccent)
decomposeCombining '\x1edd' = Just ('\x01a1', CombiningGraveAccent)
decomposeCombining '\x1ede' = Just ('\x01a0', CombiningHookAbove)
decomposeCombining '\x1edf' = Just ('\x01a1', CombiningHookAbove)
decomposeCombining '\x1ee0' = Just ('\x01a0', CombiningTilde)
decomposeCombining '\x1ee1' = Just ('\x01a1', CombiningTilde)
decomposeCombining '\x1ee2' = Just ('\x01a0', CombiningDotBelow)
decomposeCombining '\x1ee3' = Just ('\x01a1', CombiningDotBelow)
decomposeCombining '\x1ee4' = Just ('U', CombiningDotBelow)
decomposeCombining '\x1ee5' = Just ('u', CombiningDotBelow)
decomposeCombining '\x1ee6' = Just ('U', CombiningHookAbove)
decomposeCombining '\x1ee7' = Just ('u', CombiningHookAbove)
decomposeCombining '\x1ee8' = Just ('\x01af', CombiningAcuteAccent)
decomposeCombining '\x1ee9' = Just ('\x01b0', CombiningAcuteAccent)
decomposeCombining '\x1eea' = Just ('\x01af', CombiningGraveAccent)
decomposeCombining '\x1eeb' = Just ('\x01b0', CombiningGraveAccent)
decomposeCombining '\x1eec' = Just ('\x01af', CombiningHookAbove)
decomposeCombining '\x1eed' = Just ('\x01b0', CombiningHookAbove)
decomposeCombining '\x1eee' = Just ('\x01af', CombiningTilde)
decomposeCombining '\x1eef' = Just ('\x01b0', CombiningTilde)
decomposeCombining '\x1ef0' = Just ('\x01af', CombiningDotBelow)
decomposeCombining '\x1ef1' = Just ('\x01b0', CombiningDotBelow)
decomposeCombining '\x1ef2' = Just ('Y', CombiningGraveAccent)
decomposeCombining '\x1ef3' = Just ('y', CombiningGraveAccent)
decomposeCombining '\x1ef4' = Just ('Y', CombiningDotBelow)
decomposeCombining '\x1ef5' = Just ('y', CombiningDotBelow)
decomposeCombining '\x1ef6' = Just ('Y', CombiningHookAbove)
decomposeCombining '\x1ef7' = Just ('y', CombiningHookAbove)
decomposeCombining '\x1ef8' = Just ('Y', CombiningTilde)
decomposeCombining '\x1ef9' = Just ('y', CombiningTilde)
decomposeCombining '\x1f00' = Just ('\x03b1', CombiningCommaAbove)
decomposeCombining '\x1f01' = Just ('\x03b1', CombiningReversedCommaAbove)
decomposeCombining '\x1f02' = Just ('\x1f00', CombiningGraveAccent)
decomposeCombining '\x1f03' = Just ('\x1f01', CombiningGraveAccent)
decomposeCombining '\x1f04' = Just ('\x1f00', CombiningAcuteAccent)
decomposeCombining '\x1f05' = Just ('\x1f01', CombiningAcuteAccent)
decomposeCombining '\x1f06' = Just ('\x1f00', CombiningGreekPerispomeni)
decomposeCombining '\x1f07' = Just ('\x1f01', CombiningGreekPerispomeni)
decomposeCombining '\x1f08' = Just ('\x0391', CombiningCommaAbove)
decomposeCombining '\x1f09' = Just ('\x0391', CombiningReversedCommaAbove)
decomposeCombining '\x1f0a' = Just ('\x1f08', CombiningGraveAccent)
decomposeCombining '\x1f0b' = Just ('\x1f09', CombiningGraveAccent)
decomposeCombining '\x1f0c' = Just ('\x1f08', CombiningAcuteAccent)
decomposeCombining '\x1f0d' = Just ('\x1f09', CombiningAcuteAccent)
decomposeCombining '\x1f0e' = Just ('\x1f08', CombiningGreekPerispomeni)
decomposeCombining '\x1f0f' = Just ('\x1f09', CombiningGreekPerispomeni)
decomposeCombining '\x1f10' = Just ('\x03b5', CombiningCommaAbove)
decomposeCombining '\x1f11' = Just ('\x03b5', CombiningReversedCommaAbove)
decomposeCombining '\x1f12' = Just ('\x1f10', CombiningGraveAccent)
decomposeCombining '\x1f13' = Just ('\x1f11', CombiningGraveAccent)
decomposeCombining '\x1f14' = Just ('\x1f10', CombiningAcuteAccent)
decomposeCombining '\x1f15' = Just ('\x1f11', CombiningAcuteAccent)
decomposeCombining '\x1f18' = Just ('\x0395', CombiningCommaAbove)
decomposeCombining '\x1f19' = Just ('\x0395', CombiningReversedCommaAbove)
decomposeCombining '\x1f1a' = Just ('\x1f18', CombiningGraveAccent)
decomposeCombining '\x1f1b' = Just ('\x1f19', CombiningGraveAccent)
decomposeCombining '\x1f1c' = Just ('\x1f18', CombiningAcuteAccent)
decomposeCombining '\x1f1d' = Just ('\x1f19', CombiningAcuteAccent)
decomposeCombining '\x1f20' = Just ('\x03b7', CombiningCommaAbove)
decomposeCombining '\x1f21' = Just ('\x03b7', CombiningReversedCommaAbove)
decomposeCombining '\x1f22' = Just ('\x1f20', CombiningGraveAccent)
decomposeCombining '\x1f23' = Just ('\x1f21', CombiningGraveAccent)
decomposeCombining '\x1f24' = Just ('\x1f20', CombiningAcuteAccent)
decomposeCombining '\x1f25' = Just ('\x1f21', CombiningAcuteAccent)
decomposeCombining '\x1f26' = Just ('\x1f20', CombiningGreekPerispomeni)
decomposeCombining '\x1f27' = Just ('\x1f21', CombiningGreekPerispomeni)
decomposeCombining '\x1f28' = Just ('\x0397', CombiningCommaAbove)
decomposeCombining '\x1f29' = Just ('\x0397', CombiningReversedCommaAbove)
decomposeCombining '\x1f2a' = Just ('\x1f28', CombiningGraveAccent)
decomposeCombining '\x1f2b' = Just ('\x1f29', CombiningGraveAccent)
decomposeCombining '\x1f2c' = Just ('\x1f28', CombiningAcuteAccent)
decomposeCombining '\x1f2d' = Just ('\x1f29', CombiningAcuteAccent)
decomposeCombining '\x1f2e' = Just ('\x1f28', CombiningGreekPerispomeni)
decomposeCombining '\x1f2f' = Just ('\x1f29', CombiningGreekPerispomeni)
decomposeCombining '\x1f30' = Just ('\x03b9', CombiningCommaAbove)
decomposeCombining '\x1f31' = Just ('\x03b9', CombiningReversedCommaAbove)
decomposeCombining '\x1f32' = Just ('\x1f30', CombiningGraveAccent)
decomposeCombining '\x1f33' = Just ('\x1f31', CombiningGraveAccent)
decomposeCombining '\x1f34' = Just ('\x1f30', CombiningAcuteAccent)
decomposeCombining '\x1f35' = Just ('\x1f31', CombiningAcuteAccent)
decomposeCombining '\x1f36' = Just ('\x1f30', CombiningGreekPerispomeni)
decomposeCombining '\x1f37' = Just ('\x1f31', CombiningGreekPerispomeni)
decomposeCombining '\x1f38' = Just ('\x0399', CombiningCommaAbove)
decomposeCombining '\x1f39' = Just ('\x0399', CombiningReversedCommaAbove)
decomposeCombining '\x1f3a' = Just ('\x1f38', CombiningGraveAccent)
decomposeCombining '\x1f3b' = Just ('\x1f39', CombiningGraveAccent)
decomposeCombining '\x1f3c' = Just ('\x1f38', CombiningAcuteAccent)
decomposeCombining '\x1f3d' = Just ('\x1f39', CombiningAcuteAccent)
decomposeCombining '\x1f3e' = Just ('\x1f38', CombiningGreekPerispomeni)
decomposeCombining '\x1f3f' = Just ('\x1f39', CombiningGreekPerispomeni)
decomposeCombining '\x1f40' = Just ('\x03bf', CombiningCommaAbove)
decomposeCombining '\x1f41' = Just ('\x03bf', CombiningReversedCommaAbove)
decomposeCombining '\x1f42' = Just ('\x1f40', CombiningGraveAccent)
decomposeCombining '\x1f43' = Just ('\x1f41', CombiningGraveAccent)
decomposeCombining '\x1f44' = Just ('\x1f40', CombiningAcuteAccent)
decomposeCombining '\x1f45' = Just ('\x1f41', CombiningAcuteAccent)
decomposeCombining '\x1f48' = Just ('\x039f', CombiningCommaAbove)
decomposeCombining '\x1f49' = Just ('\x039f', CombiningReversedCommaAbove)
decomposeCombining '\x1f4a' = Just ('\x1f48', CombiningGraveAccent)
decomposeCombining '\x1f4b' = Just ('\x1f49', CombiningGraveAccent)
decomposeCombining '\x1f4c' = Just ('\x1f48', CombiningAcuteAccent)
decomposeCombining '\x1f4d' = Just ('\x1f49', CombiningAcuteAccent)
decomposeCombining '\x1f50' = Just ('\x03c5', CombiningCommaAbove)
decomposeCombining '\x1f51' = Just ('\x03c5', CombiningReversedCommaAbove)
decomposeCombining '\x1f52' = Just ('\x1f50', CombiningGraveAccent)
decomposeCombining '\x1f53' = Just ('\x1f51', CombiningGraveAccent)
decomposeCombining '\x1f54' = Just ('\x1f50', CombiningAcuteAccent)
decomposeCombining '\x1f55' = Just ('\x1f51', CombiningAcuteAccent)
decomposeCombining '\x1f56' = Just ('\x1f50', CombiningGreekPerispomeni)
decomposeCombining '\x1f57' = Just ('\x1f51', CombiningGreekPerispomeni)
decomposeCombining '\x1f59' = Just ('\x03a5', CombiningReversedCommaAbove)
decomposeCombining '\x1f5b' = Just ('\x1f59', CombiningGraveAccent)
decomposeCombining '\x1f5d' = Just ('\x1f59', CombiningAcuteAccent)
decomposeCombining '\x1f5f' = Just ('\x1f59', CombiningGreekPerispomeni)
decomposeCombining '\x1f60' = Just ('\x03c9', CombiningCommaAbove)
decomposeCombining '\x1f61' = Just ('\x03c9', CombiningReversedCommaAbove)
decomposeCombining '\x1f62' = Just ('\x1f60', CombiningGraveAccent)
decomposeCombining '\x1f63' = Just ('\x1f61', CombiningGraveAccent)
decomposeCombining '\x1f64' = Just ('\x1f60', CombiningAcuteAccent)
decomposeCombining '\x1f65' = Just ('\x1f61', CombiningAcuteAccent)
decomposeCombining '\x1f66' = Just ('\x1f60', CombiningGreekPerispomeni)
decomposeCombining '\x1f67' = Just ('\x1f61', CombiningGreekPerispomeni)
decomposeCombining '\x1f68' = Just ('\x03a9', CombiningCommaAbove)
decomposeCombining '\x1f69' = Just ('\x03a9', CombiningReversedCommaAbove)
decomposeCombining '\x1f6a' = Just ('\x1f68', CombiningGraveAccent)
decomposeCombining '\x1f6b' = Just ('\x1f69', CombiningGraveAccent)
decomposeCombining '\x1f6c' = Just ('\x1f68', CombiningAcuteAccent)
decomposeCombining '\x1f6d' = Just ('\x1f69', CombiningAcuteAccent)
decomposeCombining '\x1f6e' = Just ('\x1f68', CombiningGreekPerispomeni)
decomposeCombining '\x1f6f' = Just ('\x1f69', CombiningGreekPerispomeni)
decomposeCombining '\x1f70' = Just ('\x03b1', CombiningGraveAccent)
decomposeCombining '\x1f72' = Just ('\x03b5', CombiningGraveAccent)
decomposeCombining '\x1f74' = Just ('\x03b7', CombiningGraveAccent)
decomposeCombining '\x1f76' = Just ('\x03b9', CombiningGraveAccent)
decomposeCombining '\x1f78' = Just ('\x03bf', CombiningGraveAccent)
decomposeCombining '\x1f7a' = Just ('\x03c5', CombiningGraveAccent)
decomposeCombining '\x1f7c' = Just ('\x03c9', CombiningGraveAccent)
decomposeCombining '\x1f80' = Just ('\x1f00', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f81' = Just ('\x1f01', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f82' = Just ('\x1f02', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f83' = Just ('\x1f03', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f84' = Just ('\x1f04', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f85' = Just ('\x1f05', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f86' = Just ('\x1f06', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f87' = Just ('\x1f07', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f88' = Just ('\x1f08', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f89' = Just ('\x1f09', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f8a' = Just ('\x1f0a', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f8b' = Just ('\x1f0b', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f8c' = Just ('\x1f0c', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f8d' = Just ('\x1f0d', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f8e' = Just ('\x1f0e', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f8f' = Just ('\x1f0f', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f90' = Just ('\x1f20', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f91' = Just ('\x1f21', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f92' = Just ('\x1f22', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f93' = Just ('\x1f23', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f94' = Just ('\x1f24', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f95' = Just ('\x1f25', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f96' = Just ('\x1f26', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f97' = Just ('\x1f27', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f98' = Just ('\x1f28', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f99' = Just ('\x1f29', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f9a' = Just ('\x1f2a', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f9b' = Just ('\x1f2b', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f9c' = Just ('\x1f2c', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f9d' = Just ('\x1f2d', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f9e' = Just ('\x1f2e', CombiningGreekYpogegrammeni)
decomposeCombining '\x1f9f' = Just ('\x1f2f', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fa0' = Just ('\x1f60', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fa1' = Just ('\x1f61', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fa2' = Just ('\x1f62', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fa3' = Just ('\x1f63', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fa4' = Just ('\x1f64', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fa5' = Just ('\x1f65', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fa6' = Just ('\x1f66', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fa7' = Just ('\x1f67', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fa8' = Just ('\x1f68', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fa9' = Just ('\x1f69', CombiningGreekYpogegrammeni)
decomposeCombining '\x1faa' = Just ('\x1f6a', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fab' = Just ('\x1f6b', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fac' = Just ('\x1f6c', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fad' = Just ('\x1f6d', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fae' = Just ('\x1f6e', CombiningGreekYpogegrammeni)
decomposeCombining '\x1faf' = Just ('\x1f6f', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fb0' = Just ('\x03b1', CombiningBreve)
decomposeCombining '\x1fb1' = Just ('\x03b1', CombiningMacron)
decomposeCombining '\x1fb2' = Just ('\x1f70', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fb3' = Just ('\x03b1', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fb4' = Just ('\x03ac', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fb6' = Just ('\x03b1', CombiningGreekPerispomeni)
decomposeCombining '\x1fb7' = Just ('\x1fb6', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fb8' = Just ('\x0391', CombiningBreve)
decomposeCombining '\x1fb9' = Just ('\x0391', CombiningMacron)
decomposeCombining '\x1fba' = Just ('\x0391', CombiningGraveAccent)
decomposeCombining '\x1fbc' = Just ('\x0391', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fc1' = Just ('\x00a8', CombiningGreekPerispomeni)
decomposeCombining '\x1fc2' = Just ('\x1f74', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fc3' = Just ('\x03b7', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fc4' = Just ('\x03ae', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fc6' = Just ('\x03b7', CombiningGreekPerispomeni)
decomposeCombining '\x1fc7' = Just ('\x1fc6', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fc8' = Just ('\x0395', CombiningGraveAccent)
decomposeCombining '\x1fca' = Just ('\x0397', CombiningGraveAccent)
decomposeCombining '\x1fcc' = Just ('\x0397', CombiningGreekYpogegrammeni)
decomposeCombining '\x1fcd' = Just ('\x1fbf', CombiningGraveAccent)
decomposeCombining '\x1fce' = Just ('\x1fbf', CombiningAcuteAccent)
decomposeCombining '\x1fcf' = Just ('\x1fbf', CombiningGreekPerispomeni)
decomposeCombining '\x1fd0' = Just ('\x03b9', CombiningBreve)
decomposeCombining '\x1fd1' = Just ('\x03b9', CombiningMacron)
decomposeCombining '\x1fd2' = Just ('\x03ca', CombiningGraveAccent)
decomposeCombining '\x1fd6' = Just ('\x03b9', CombiningGreekPerispomeni)
decomposeCombining '\x1fd7' = Just ('\x03ca', CombiningGreekPerispomeni)
decomposeCombining '\x1fd8' = Just ('\x0399', CombiningBreve)
decomposeCombining '\x1fd9' = Just ('\x0399', CombiningMacron)
decomposeCombining '\x1fda' = Just ('\x0399', CombiningGraveAccent)
decomposeCombining '\x1fdd' = Just ('\x1ffe', CombiningGraveAccent)
decomposeCombining '\x1fde' = Just ('\x1ffe', CombiningAcuteAccent)
decomposeCombining '\x1fdf' = Just ('\x1ffe', CombiningGreekPerispomeni)
decomposeCombining '\x1fe0' = Just ('\x03c5', CombiningBreve)
decomposeCombining '\x1fe1' = Just ('\x03c5', CombiningMacron)
decomposeCombining '\x1fe2' = Just ('\x03cb', CombiningGraveAccent)
decomposeCombining '\x1fe4' = Just ('\x03c1', CombiningCommaAbove)
decomposeCombining '\x1fe5' = Just ('\x03c1', CombiningReversedCommaAbove)
decomposeCombining '\x1fe6' = Just ('\x03c5', CombiningGreekPerispomeni)
decomposeCombining '\x1fe7' = Just ('\x03cb', CombiningGreekPerispomeni)
decomposeCombining '\x1fe8' = Just ('\x03a5', CombiningBreve)
decomposeCombining '\x1fe9' = Just ('\x03a5', CombiningMacron)
decomposeCombining '\x1fea' = Just ('\x03a5', CombiningGraveAccent)
decomposeCombining '\x1fec' = Just ('\x03a1', CombiningReversedCommaAbove)
decomposeCombining '\x1fed' = Just ('\x00a8', CombiningGraveAccent)
decomposeCombining '\x1ff2' = Just ('\x1f7c', CombiningGreekYpogegrammeni)
decomposeCombining '\x1ff3' = Just ('\x03c9', CombiningGreekYpogegrammeni)
decomposeCombining '\x1ff4' = Just ('\x03ce', CombiningGreekYpogegrammeni)
decomposeCombining '\x1ff6' = Just ('\x03c9', CombiningGreekPerispomeni)
decomposeCombining '\x1ff7' = Just ('\x1ff6', CombiningGreekYpogegrammeni)
decomposeCombining '\x1ff8' = Just ('\x039f', CombiningGraveAccent)
decomposeCombining '\x1ffa' = Just ('\x03a9', CombiningGraveAccent)
decomposeCombining '\x1ffc' = Just ('\x03a9', CombiningGreekYpogegrammeni)
decomposeCombining '\x219a' = Just ('\x2190', CombiningLongSolidusOverlay)
decomposeCombining '\x219b' = Just ('\x2192', CombiningLongSolidusOverlay)
decomposeCombining '\x21ae' = Just ('\x2194', CombiningLongSolidusOverlay)
decomposeCombining '\x21cd' = Just ('\x21d0', CombiningLongSolidusOverlay)
decomposeCombining '\x21ce' = Just ('\x21d4', CombiningLongSolidusOverlay)
decomposeCombining '\x21cf' = Just ('\x21d2', CombiningLongSolidusOverlay)
decomposeCombining '\x2204' = Just ('\x2203', CombiningLongSolidusOverlay)
decomposeCombining '\x2209' = Just ('\x2208', CombiningLongSolidusOverlay)
decomposeCombining '\x220c' = Just ('\x220b', CombiningLongSolidusOverlay)
decomposeCombining '\x2224' = Just ('\x2223', CombiningLongSolidusOverlay)
decomposeCombining '\x2226' = Just ('\x2225', CombiningLongSolidusOverlay)
decomposeCombining '\x2241' = Just ('\x223c', CombiningLongSolidusOverlay)
decomposeCombining '\x2244' = Just ('\x2243', CombiningLongSolidusOverlay)
decomposeCombining '\x2247' = Just ('\x2245', CombiningLongSolidusOverlay)
decomposeCombining '\x2249' = Just ('\x2248', CombiningLongSolidusOverlay)
decomposeCombining '\x2260' = Just ('=', CombiningLongSolidusOverlay)
decomposeCombining '\x2262' = Just ('\x2261', CombiningLongSolidusOverlay)
decomposeCombining '\x226d' = Just ('\x224d', CombiningLongSolidusOverlay)
decomposeCombining '\x226e' = Just ('<', CombiningLongSolidusOverlay)
decomposeCombining '\x226f' = Just ('>', CombiningLongSolidusOverlay)
decomposeCombining '\x2270' = Just ('\x2264', CombiningLongSolidusOverlay)
decomposeCombining '\x2271' = Just ('\x2265', CombiningLongSolidusOverlay)
decomposeCombining '\x2274' = Just ('\x2272', CombiningLongSolidusOverlay)
decomposeCombining '\x2275' = Just ('\x2273', CombiningLongSolidusOverlay)
decomposeCombining '\x2278' = Just ('\x2276', CombiningLongSolidusOverlay)
decomposeCombining '\x2279' = Just ('\x2277', CombiningLongSolidusOverlay)
decomposeCombining '\x2280' = Just ('\x227a', CombiningLongSolidusOverlay)
decomposeCombining '\x2281' = Just ('\x227b', CombiningLongSolidusOverlay)
decomposeCombining '\x2284' = Just ('\x2282', CombiningLongSolidusOverlay)
decomposeCombining '\x2285' = Just ('\x2283', CombiningLongSolidusOverlay)
decomposeCombining '\x2288' = Just ('\x2286', CombiningLongSolidusOverlay)
decomposeCombining '\x2289' = Just ('\x2287', CombiningLongSolidusOverlay)
decomposeCombining '\x22ac' = Just ('\x22a2', CombiningLongSolidusOverlay)
decomposeCombining '\x22ad' = Just ('\x22a8', CombiningLongSolidusOverlay)
decomposeCombining '\x22ae' = Just ('\x22a9', CombiningLongSolidusOverlay)
decomposeCombining '\x22af' = Just ('\x22ab', CombiningLongSolidusOverlay)
decomposeCombining '\x22e0' = Just ('\x227c', CombiningLongSolidusOverlay)
decomposeCombining '\x22e1' = Just ('\x227d', CombiningLongSolidusOverlay)
decomposeCombining '\x22e2' = Just ('\x2291', CombiningLongSolidusOverlay)
decomposeCombining '\x22e3' = Just ('\x2292', CombiningLongSolidusOverlay)
decomposeCombining '\x22ea' = Just ('\x22b2', CombiningLongSolidusOverlay)
decomposeCombining '\x22eb' = Just ('\x22b3', CombiningLongSolidusOverlay)
decomposeCombining '\x22ec' = Just ('\x22b4', CombiningLongSolidusOverlay)
decomposeCombining '\x22ed' = Just ('\x22b5', CombiningLongSolidusOverlay)
decomposeCombining '\x2adc' = Just ('\x2add', CombiningLongSolidusOverlay)
decomposeCombining '\x304c' = Just ('\x304b', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x304e' = Just ('\x304d', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x3050' = Just ('\x304f', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x3052' = Just ('\x3051', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x3054' = Just ('\x3053', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x3056' = Just ('\x3055', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x3058' = Just ('\x3057', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x305a' = Just ('\x3059', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x305c' = Just ('\x305b', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x305e' = Just ('\x305d', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x3060' = Just ('\x305f', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x3062' = Just ('\x3061', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x3065' = Just ('\x3064', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x3067' = Just ('\x3066', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x3069' = Just ('\x3068', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x3070' = Just ('\x306f', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x3071' = Just ('\x306f', CombiningKatakanaHiraganaSemiVoicedSoundMark)
decomposeCombining '\x3073' = Just ('\x3072', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x3074' = Just ('\x3072', CombiningKatakanaHiraganaSemiVoicedSoundMark)
decomposeCombining '\x3076' = Just ('\x3075', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x3077' = Just ('\x3075', CombiningKatakanaHiraganaSemiVoicedSoundMark)
decomposeCombining '\x3079' = Just ('\x3078', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x307a' = Just ('\x3078', CombiningKatakanaHiraganaSemiVoicedSoundMark)
decomposeCombining '\x307c' = Just ('\x307b', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x307d' = Just ('\x307b', CombiningKatakanaHiraganaSemiVoicedSoundMark)
decomposeCombining '\x3094' = Just ('\x3046', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x309e' = Just ('\x309d', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30ac' = Just ('\x30ab', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30ae' = Just ('\x30ad', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30b0' = Just ('\x30af', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30b2' = Just ('\x30b1', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30b4' = Just ('\x30b3', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30b6' = Just ('\x30b5', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30b8' = Just ('\x30b7', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30ba' = Just ('\x30b9', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30bc' = Just ('\x30bb', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30be' = Just ('\x30bd', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30c0' = Just ('\x30bf', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30c2' = Just ('\x30c1', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30c5' = Just ('\x30c4', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30c7' = Just ('\x30c6', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30c9' = Just ('\x30c8', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30d0' = Just ('\x30cf', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30d1' = Just ('\x30cf', CombiningKatakanaHiraganaSemiVoicedSoundMark)
decomposeCombining '\x30d3' = Just ('\x30d2', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30d4' = Just ('\x30d2', CombiningKatakanaHiraganaSemiVoicedSoundMark)
decomposeCombining '\x30d6' = Just ('\x30d5', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30d7' = Just ('\x30d5', CombiningKatakanaHiraganaSemiVoicedSoundMark)
decomposeCombining '\x30d9' = Just ('\x30d8', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30da' = Just ('\x30d8', CombiningKatakanaHiraganaSemiVoicedSoundMark)
decomposeCombining '\x30dc' = Just ('\x30db', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30dd' = Just ('\x30db', CombiningKatakanaHiraganaSemiVoicedSoundMark)
decomposeCombining '\x30f4' = Just ('\x30a6', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30f7' = Just ('\x30ef', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30f8' = Just ('\x30f0', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30f9' = Just ('\x30f1', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30fa' = Just ('\x30f2', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\x30fe' = Just ('\x30fd', CombiningKatakanaHiraganaVoicedSoundMark)
decomposeCombining '\xfb1d' = Just ('\x05d9', HebrewPointHiriq)
decomposeCombining '\xfb1f' = Just ('\x05f2', HebrewPointPatah)
decomposeCombining '\xfb2a' = Just ('\x05e9', HebrewPointShinDot)
decomposeCombining '\xfb2b' = Just ('\x05e9', HebrewPointSinDot)
decomposeCombining '\xfb2c' = Just ('\xfb49', HebrewPointShinDot)
decomposeCombining '\xfb2d' = Just ('\xfb49', HebrewPointSinDot)
decomposeCombining '\xfb2e' = Just ('\x05d0', HebrewPointPatah)
decomposeCombining '\xfb2f' = Just ('\x05d0', HebrewPointQamats)
decomposeCombining '\xfb30' = Just ('\x05d0', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb31' = Just ('\x05d1', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb32' = Just ('\x05d2', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb33' = Just ('\x05d3', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb34' = Just ('\x05d4', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb35' = Just ('\x05d5', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb36' = Just ('\x05d6', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb38' = Just ('\x05d8', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb39' = Just ('\x05d9', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb3a' = Just ('\x05da', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb3b' = Just ('\x05db', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb3c' = Just ('\x05dc', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb3e' = Just ('\x05de', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb40' = Just ('\x05e0', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb41' = Just ('\x05e1', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb43' = Just ('\x05e3', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb44' = Just ('\x05e4', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb46' = Just ('\x05e6', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb47' = Just ('\x05e7', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb48' = Just ('\x05e8', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb49' = Just ('\x05e9', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb4a' = Just ('\x05ea', HebrewPointDageshOrMapiq)
decomposeCombining '\xfb4b' = Just ('\x05d5', HebrewPointHolam)
decomposeCombining '\xfb4c' = Just ('\x05d1', HebrewPointRafe)
decomposeCombining '\xfb4d' = Just ('\x05db', HebrewPointRafe)
decomposeCombining '\xfb4e' = Just ('\x05e4', HebrewPointRafe)
decomposeCombining '\x1109a' = Just ('\x11099', KaithiSignNukta)
decomposeCombining '\x1109c' = Just ('\x1109b', KaithiSignNukta)
decomposeCombining '\x110ab' = Just ('\x110a5', KaithiSignNukta)
decomposeCombining '\x1112e' = Just ('\x11131', ChakmaVowelSignA)
decomposeCombining '\x1112f' = Just ('\x11132', ChakmaVowelSignA)
decomposeCombining '\x1134b' = Just ('\x11347', GranthaVowelSignAa)
decomposeCombining '\x1134c' = Just ('\x11347', GranthaAuLengthMark)
decomposeCombining '\x114bb' = Just ('\x114b9', TirhutaVowelSignShortE)
decomposeCombining '\x114bc' = Just ('\x114b9', TirhutaVowelSignAa)
decomposeCombining '\x114be' = Just ('\x114b9', TirhutaVowelSignShortO)
decomposeCombining '\x115ba' = Just ('\x115b8', SiddhamVowelSignAa)
decomposeCombining '\x115bb' = Just ('\x115b9', SiddhamVowelSignAa)
decomposeCombining '\x1d15e' = Just ('\x1d157', MusicalSymbolCombiningStem)
decomposeCombining '\x1d15f' = Just ('\x1d158', MusicalSymbolCombiningStem)
decomposeCombining '\x1d160' = Just ('\x1d15f', MusicalSymbolCombiningFlag1)
decomposeCombining '\x1d161' = Just ('\x1d15f', MusicalSymbolCombiningFlag2)
decomposeCombining '\x1d162' = Just ('\x1d15f', MusicalSymbolCombiningFlag3)
decomposeCombining '\x1d163' = Just ('\x1d15f', MusicalSymbolCombiningFlag4)
decomposeCombining '\x1d164' = Just ('\x1d15f', MusicalSymbolCombiningFlag5)
decomposeCombining '\x1d1bb' = Just ('\x1d1b9', MusicalSymbolCombiningStem)
decomposeCombining '\x1d1bc' = Just ('\x1d1ba', MusicalSymbolCombiningStem)
decomposeCombining '\x1d1bd' = Just ('\x1d1bb', MusicalSymbolCombiningFlag1)
decomposeCombining '\x1d1be' = Just ('\x1d1bc', MusicalSymbolCombiningFlag1)
decomposeCombining '\x1d1bf' = Just ('\x1d1bb', MusicalSymbolCombiningFlag2)
decomposeCombining '\x1d1c0' = Just ('\x1d1bc', MusicalSymbolCombiningFlag2)
decomposeCombining _ = Nothing

-- | Try to combine the given character with the 'CombiningCharacter's in the
-- list, by applying 'composeCombining' on the items left-to-right. The result
-- is a 2-tuple with a more dedicated 'Char'acter (if possible), and a list
-- of 'CombiningCharacter's that could not be applied. This is a flipped version
-- of 'composeCombiningSequence''.
composeCombiningSequence
  :: Char  -- ^ The given 'Char'acter to start with.
  -> [CombiningCharacter]  -- ^ The list of 'CombiningCharacter's to apply.
  -> (Char, [CombiningCharacter]) -- A 2-tuple that contains a (possible) more dedicated character, and the list of 'CombiningCharacter's that could not be used.
composeCombiningSequence = flip composeCombiningSequence'

-- | Try to combine the given character with the 'CombiningCharacter's in the
-- list, by applying 'composeCombining' on the items left-to-right. The result
-- is a 2-tuple with a more dedicated 'Char'acter (if possible), and a list
-- of 'CombiningCharacter's that could not be applied. This is a flipped version
-- of 'composeCombiningSequence'.
composeCombiningSequence'
  :: [CombiningCharacter]  -- ^ The list of 'CombiningCharacter's to apply.
  -> Char  -- ^ The given 'Char'acter to start with.
  -> (Char, [CombiningCharacter]) -- A 2-tuple that contains a (possible) more dedicated character, and the list of 'CombiningCharacter's that could not be used.
composeCombiningSequence' [] c = (c, [])
composeCombiningSequence' ca@(cc:ccs) c
  | Just c' <- composeCombining' cc c = composeCombiningSequence' ccs c'
  | otherwise = (c, ca)

-- | Check if for the given 'Char' and the given 'CombiningCharacter' a
-- dedicated 'Char' exists that is the equivalent. If so, that dedicated
-- character is wrapped in a 'Just'; 'Nothing' otherwise. This is a flipped
-- version of 'composeCombining''.
composeCombining
  :: Char  -- ^ The given 'Char'acter that is combined with a 'CombiningCharacter'.
  -> CombiningCharacter  -- ^ The given 'CombiningCharacter' that is applied to the given 'Char'acter.
  -> Maybe Char  -- ^ A dedicated 'Char'acter wrapped in a 'Just' if such character exists, 'Nothing' otherwise.
composeCombining = flip composeCombining'

-- | Check if for the given 'Char' and the given 'CombiningCharacter' a
-- dedicated 'Char' exists that is the equivalent. If so, that dedicated
-- character is wrapped in a 'Just'; 'Nothing' otherwise. This is a flipped
-- version of 'composeCombining'.
composeCombining'
  :: CombiningCharacter  -- ^ The given 'CombiningCharacter' that is applied to the given 'Char'acter.
  -> Char  -- ^ The given 'Char'acter that is combined with a 'CombiningCharacter'.
  -> Maybe Char  -- ^ A dedicated 'Char'acter wrapped in a 'Just' if such character exists, 'Nothing' otherwise.
composeCombining' CombiningGraveAccent 'A' = Just '\x00c0'
composeCombining' CombiningAcuteAccent 'A' = Just '\x00c1'
composeCombining' CombiningCircumflexAccent 'A' = Just '\x00c2'
composeCombining' CombiningTilde 'A' = Just '\x00c3'
composeCombining' CombiningDiaeresis 'A' = Just '\x00c4'
composeCombining' CombiningRingAbove 'A' = Just '\x00c5'
composeCombining' CombiningCedilla 'C' = Just '\x00c7'
composeCombining' CombiningGraveAccent 'E' = Just '\x00c8'
composeCombining' CombiningAcuteAccent 'E' = Just '\x00c9'
composeCombining' CombiningCircumflexAccent 'E' = Just '\x00ca'
composeCombining' CombiningDiaeresis 'E' = Just '\x00cb'
composeCombining' CombiningGraveAccent 'I' = Just '\x00cc'
composeCombining' CombiningAcuteAccent 'I' = Just '\x00cd'
composeCombining' CombiningCircumflexAccent 'I' = Just '\x00ce'
composeCombining' CombiningDiaeresis 'I' = Just '\x00cf'
composeCombining' CombiningTilde 'N' = Just '\x00d1'
composeCombining' CombiningGraveAccent 'O' = Just '\x00d2'
composeCombining' CombiningAcuteAccent 'O' = Just '\x00d3'
composeCombining' CombiningCircumflexAccent 'O' = Just '\x00d4'
composeCombining' CombiningTilde 'O' = Just '\x00d5'
composeCombining' CombiningDiaeresis 'O' = Just '\x00d6'
composeCombining' CombiningGraveAccent 'U' = Just '\x00d9'
composeCombining' CombiningAcuteAccent 'U' = Just '\x00da'
composeCombining' CombiningCircumflexAccent 'U' = Just '\x00db'
composeCombining' CombiningDiaeresis 'U' = Just '\x00dc'
composeCombining' CombiningAcuteAccent 'Y' = Just '\x00dd'
composeCombining' CombiningGraveAccent 'a' = Just '\x00e0'
composeCombining' CombiningAcuteAccent 'a' = Just '\x00e1'
composeCombining' CombiningCircumflexAccent 'a' = Just '\x00e2'
composeCombining' CombiningTilde 'a' = Just '\x00e3'
composeCombining' CombiningDiaeresis 'a' = Just '\x00e4'
composeCombining' CombiningRingAbove 'a' = Just '\x00e5'
composeCombining' CombiningCedilla 'c' = Just '\x00e7'
composeCombining' CombiningGraveAccent 'e' = Just '\x00e8'
composeCombining' CombiningAcuteAccent 'e' = Just '\x00e9'
composeCombining' CombiningCircumflexAccent 'e' = Just '\x00ea'
composeCombining' CombiningDiaeresis 'e' = Just '\x00eb'
composeCombining' CombiningGraveAccent 'i' = Just '\x00ec'
composeCombining' CombiningAcuteAccent 'i' = Just '\x00ed'
composeCombining' CombiningCircumflexAccent 'i' = Just '\x00ee'
composeCombining' CombiningDiaeresis 'i' = Just '\x00ef'
composeCombining' CombiningTilde 'n' = Just '\x00f1'
composeCombining' CombiningGraveAccent 'o' = Just '\x00f2'
composeCombining' CombiningAcuteAccent 'o' = Just '\x00f3'
composeCombining' CombiningCircumflexAccent 'o' = Just '\x00f4'
composeCombining' CombiningTilde 'o' = Just '\x00f5'
composeCombining' CombiningDiaeresis 'o' = Just '\x00f6'
composeCombining' CombiningGraveAccent 'u' = Just '\x00f9'
composeCombining' CombiningAcuteAccent 'u' = Just '\x00fa'
composeCombining' CombiningCircumflexAccent 'u' = Just '\x00fb'
composeCombining' CombiningDiaeresis 'u' = Just '\x00fc'
composeCombining' CombiningAcuteAccent 'y' = Just '\x00fd'
composeCombining' CombiningDiaeresis 'y' = Just '\x00ff'
composeCombining' CombiningMacron 'A' = Just '\x0100'
composeCombining' CombiningMacron 'a' = Just '\x0101'
composeCombining' CombiningBreve 'A' = Just '\x0102'
composeCombining' CombiningBreve 'a' = Just '\x0103'
composeCombining' CombiningOgonek 'A' = Just '\x0104'
composeCombining' CombiningOgonek 'a' = Just '\x0105'
composeCombining' CombiningAcuteAccent 'C' = Just '\x0106'
composeCombining' CombiningAcuteAccent 'c' = Just '\x0107'
composeCombining' CombiningCircumflexAccent 'C' = Just '\x0108'
composeCombining' CombiningCircumflexAccent 'c' = Just '\x0109'
composeCombining' CombiningDotAbove 'C' = Just '\x010a'
composeCombining' CombiningDotAbove 'c' = Just '\x010b'
composeCombining' CombiningCaron 'C' = Just '\x010c'
composeCombining' CombiningCaron 'c' = Just '\x010d'
composeCombining' CombiningCaron 'D' = Just '\x010e'
composeCombining' CombiningCaron 'd' = Just '\x010f'
composeCombining' CombiningMacron 'E' = Just '\x0112'
composeCombining' CombiningMacron 'e' = Just '\x0113'
composeCombining' CombiningBreve 'E' = Just '\x0114'
composeCombining' CombiningBreve 'e' = Just '\x0115'
composeCombining' CombiningDotAbove 'E' = Just '\x0116'
composeCombining' CombiningDotAbove 'e' = Just '\x0117'
composeCombining' CombiningOgonek 'E' = Just '\x0118'
composeCombining' CombiningOgonek 'e' = Just '\x0119'
composeCombining' CombiningCaron 'E' = Just '\x011a'
composeCombining' CombiningCaron 'e' = Just '\x011b'
composeCombining' CombiningCircumflexAccent 'G' = Just '\x011c'
composeCombining' CombiningCircumflexAccent 'g' = Just '\x011d'
composeCombining' CombiningBreve 'G' = Just '\x011e'
composeCombining' CombiningBreve 'g' = Just '\x011f'
composeCombining' CombiningDotAbove 'G' = Just '\x0120'
composeCombining' CombiningDotAbove 'g' = Just '\x0121'
composeCombining' CombiningCedilla 'G' = Just '\x0122'
composeCombining' CombiningCedilla 'g' = Just '\x0123'
composeCombining' CombiningCircumflexAccent 'H' = Just '\x0124'
composeCombining' CombiningCircumflexAccent 'h' = Just '\x0125'
composeCombining' CombiningTilde 'I' = Just '\x0128'
composeCombining' CombiningTilde 'i' = Just '\x0129'
composeCombining' CombiningMacron 'I' = Just '\x012a'
composeCombining' CombiningMacron 'i' = Just '\x012b'
composeCombining' CombiningBreve 'I' = Just '\x012c'
composeCombining' CombiningBreve 'i' = Just '\x012d'
composeCombining' CombiningOgonek 'I' = Just '\x012e'
composeCombining' CombiningOgonek 'i' = Just '\x012f'
composeCombining' CombiningDotAbove 'I' = Just '\x0130'
composeCombining' CombiningCircumflexAccent 'J' = Just '\x0134'
composeCombining' CombiningCircumflexAccent 'j' = Just '\x0135'
composeCombining' CombiningCedilla 'K' = Just '\x0136'
composeCombining' CombiningCedilla 'k' = Just '\x0137'
composeCombining' CombiningAcuteAccent 'L' = Just '\x0139'
composeCombining' CombiningAcuteAccent 'l' = Just '\x013a'
composeCombining' CombiningCedilla 'L' = Just '\x013b'
composeCombining' CombiningCedilla 'l' = Just '\x013c'
composeCombining' CombiningCaron 'L' = Just '\x013d'
composeCombining' CombiningCaron 'l' = Just '\x013e'
composeCombining' CombiningAcuteAccent 'N' = Just '\x0143'
composeCombining' CombiningAcuteAccent 'n' = Just '\x0144'
composeCombining' CombiningCedilla 'N' = Just '\x0145'
composeCombining' CombiningCedilla 'n' = Just '\x0146'
composeCombining' CombiningCaron 'N' = Just '\x0147'
composeCombining' CombiningCaron 'n' = Just '\x0148'
composeCombining' CombiningMacron 'O' = Just '\x014c'
composeCombining' CombiningMacron 'o' = Just '\x014d'
composeCombining' CombiningBreve 'O' = Just '\x014e'
composeCombining' CombiningBreve 'o' = Just '\x014f'
composeCombining' CombiningDoubleAcuteAccent 'O' = Just '\x0150'
composeCombining' CombiningDoubleAcuteAccent 'o' = Just '\x0151'
composeCombining' CombiningAcuteAccent 'R' = Just '\x0154'
composeCombining' CombiningAcuteAccent 'r' = Just '\x0155'
composeCombining' CombiningCedilla 'R' = Just '\x0156'
composeCombining' CombiningCedilla 'r' = Just '\x0157'
composeCombining' CombiningCaron 'R' = Just '\x0158'
composeCombining' CombiningCaron 'r' = Just '\x0159'
composeCombining' CombiningAcuteAccent 'S' = Just '\x015a'
composeCombining' CombiningAcuteAccent 's' = Just '\x015b'
composeCombining' CombiningCircumflexAccent 'S' = Just '\x015c'
composeCombining' CombiningCircumflexAccent 's' = Just '\x015d'
composeCombining' CombiningCedilla 'S' = Just '\x015e'
composeCombining' CombiningCedilla 's' = Just '\x015f'
composeCombining' CombiningCaron 'S' = Just '\x0160'
composeCombining' CombiningCaron 's' = Just '\x0161'
composeCombining' CombiningCedilla 'T' = Just '\x0162'
composeCombining' CombiningCedilla 't' = Just '\x0163'
composeCombining' CombiningCaron 'T' = Just '\x0164'
composeCombining' CombiningCaron 't' = Just '\x0165'
composeCombining' CombiningTilde 'U' = Just '\x0168'
composeCombining' CombiningTilde 'u' = Just '\x0169'
composeCombining' CombiningMacron 'U' = Just '\x016a'
composeCombining' CombiningMacron 'u' = Just '\x016b'
composeCombining' CombiningBreve 'U' = Just '\x016c'
composeCombining' CombiningBreve 'u' = Just '\x016d'
composeCombining' CombiningRingAbove 'U' = Just '\x016e'
composeCombining' CombiningRingAbove 'u' = Just '\x016f'
composeCombining' CombiningDoubleAcuteAccent 'U' = Just '\x0170'
composeCombining' CombiningDoubleAcuteAccent 'u' = Just '\x0171'
composeCombining' CombiningOgonek 'U' = Just '\x0172'
composeCombining' CombiningOgonek 'u' = Just '\x0173'
composeCombining' CombiningCircumflexAccent 'W' = Just '\x0174'
composeCombining' CombiningCircumflexAccent 'w' = Just '\x0175'
composeCombining' CombiningCircumflexAccent 'Y' = Just '\x0176'
composeCombining' CombiningCircumflexAccent 'y' = Just '\x0177'
composeCombining' CombiningDiaeresis 'Y' = Just '\x0178'
composeCombining' CombiningAcuteAccent 'Z' = Just '\x0179'
composeCombining' CombiningAcuteAccent 'z' = Just '\x017a'
composeCombining' CombiningDotAbove 'Z' = Just '\x017b'
composeCombining' CombiningDotAbove 'z' = Just '\x017c'
composeCombining' CombiningCaron 'Z' = Just '\x017d'
composeCombining' CombiningCaron 'z' = Just '\x017e'
composeCombining' CombiningHorn 'O' = Just '\x01a0'
composeCombining' CombiningHorn 'o' = Just '\x01a1'
composeCombining' CombiningHorn 'U' = Just '\x01af'
composeCombining' CombiningHorn 'u' = Just '\x01b0'
composeCombining' CombiningCaron 'A' = Just '\x01cd'
composeCombining' CombiningCaron 'a' = Just '\x01ce'
composeCombining' CombiningCaron 'I' = Just '\x01cf'
composeCombining' CombiningCaron 'i' = Just '\x01d0'
composeCombining' CombiningCaron 'O' = Just '\x01d1'
composeCombining' CombiningCaron 'o' = Just '\x01d2'
composeCombining' CombiningCaron 'U' = Just '\x01d3'
composeCombining' CombiningCaron 'u' = Just '\x01d4'
composeCombining' CombiningMacron '\x00dc' = Just '\x01d5'
composeCombining' CombiningMacron '\x00fc' = Just '\x01d6'
composeCombining' CombiningAcuteAccent '\x00dc' = Just '\x01d7'
composeCombining' CombiningAcuteAccent '\x00fc' = Just '\x01d8'
composeCombining' CombiningCaron '\x00dc' = Just '\x01d9'
composeCombining' CombiningCaron '\x00fc' = Just '\x01da'
composeCombining' CombiningGraveAccent '\x00dc' = Just '\x01db'
composeCombining' CombiningGraveAccent '\x00fc' = Just '\x01dc'
composeCombining' CombiningMacron '\x00c4' = Just '\x01de'
composeCombining' CombiningMacron '\x00e4' = Just '\x01df'
composeCombining' CombiningMacron '\x0226' = Just '\x01e0'
composeCombining' CombiningMacron '\x0227' = Just '\x01e1'
composeCombining' CombiningMacron '\x00c6' = Just '\x01e2'
composeCombining' CombiningMacron '\x00e6' = Just '\x01e3'
composeCombining' CombiningCaron 'G' = Just '\x01e6'
composeCombining' CombiningCaron 'g' = Just '\x01e7'
composeCombining' CombiningCaron 'K' = Just '\x01e8'
composeCombining' CombiningCaron 'k' = Just '\x01e9'
composeCombining' CombiningOgonek 'O' = Just '\x01ea'
composeCombining' CombiningOgonek 'o' = Just '\x01eb'
composeCombining' CombiningMacron '\x01ea' = Just '\x01ec'
composeCombining' CombiningMacron '\x01eb' = Just '\x01ed'
composeCombining' CombiningCaron '\x01b7' = Just '\x01ee'
composeCombining' CombiningCaron '\x0292' = Just '\x01ef'
composeCombining' CombiningCaron 'j' = Just '\x01f0'
composeCombining' CombiningAcuteAccent 'G' = Just '\x01f4'
composeCombining' CombiningAcuteAccent 'g' = Just '\x01f5'
composeCombining' CombiningGraveAccent 'N' = Just '\x01f8'
composeCombining' CombiningGraveAccent 'n' = Just '\x01f9'
composeCombining' CombiningAcuteAccent '\x00c5' = Just '\x01fa'
composeCombining' CombiningAcuteAccent '\x00e5' = Just '\x01fb'
composeCombining' CombiningAcuteAccent '\x00c6' = Just '\x01fc'
composeCombining' CombiningAcuteAccent '\x00e6' = Just '\x01fd'
composeCombining' CombiningAcuteAccent '\x00d8' = Just '\x01fe'
composeCombining' CombiningAcuteAccent '\x00f8' = Just '\x01ff'
composeCombining' CombiningDoubleGraveAccent 'A' = Just '\x0200'
composeCombining' CombiningDoubleGraveAccent 'a' = Just '\x0201'
composeCombining' CombiningInvertedBreve 'A' = Just '\x0202'
composeCombining' CombiningInvertedBreve 'a' = Just '\x0203'
composeCombining' CombiningDoubleGraveAccent 'E' = Just '\x0204'
composeCombining' CombiningDoubleGraveAccent 'e' = Just '\x0205'
composeCombining' CombiningInvertedBreve 'E' = Just '\x0206'
composeCombining' CombiningInvertedBreve 'e' = Just '\x0207'
composeCombining' CombiningDoubleGraveAccent 'I' = Just '\x0208'
composeCombining' CombiningDoubleGraveAccent 'i' = Just '\x0209'
composeCombining' CombiningInvertedBreve 'I' = Just '\x020a'
composeCombining' CombiningInvertedBreve 'i' = Just '\x020b'
composeCombining' CombiningDoubleGraveAccent 'O' = Just '\x020c'
composeCombining' CombiningDoubleGraveAccent 'o' = Just '\x020d'
composeCombining' CombiningInvertedBreve 'O' = Just '\x020e'
composeCombining' CombiningInvertedBreve 'o' = Just '\x020f'
composeCombining' CombiningDoubleGraveAccent 'R' = Just '\x0210'
composeCombining' CombiningDoubleGraveAccent 'r' = Just '\x0211'
composeCombining' CombiningInvertedBreve 'R' = Just '\x0212'
composeCombining' CombiningInvertedBreve 'r' = Just '\x0213'
composeCombining' CombiningDoubleGraveAccent 'U' = Just '\x0214'
composeCombining' CombiningDoubleGraveAccent 'u' = Just '\x0215'
composeCombining' CombiningInvertedBreve 'U' = Just '\x0216'
composeCombining' CombiningInvertedBreve 'u' = Just '\x0217'
composeCombining' CombiningCommaBelow 'S' = Just '\x0218'
composeCombining' CombiningCommaBelow 's' = Just '\x0219'
composeCombining' CombiningCommaBelow 'T' = Just '\x021a'
composeCombining' CombiningCommaBelow 't' = Just '\x021b'
composeCombining' CombiningCaron 'H' = Just '\x021e'
composeCombining' CombiningCaron 'h' = Just '\x021f'
composeCombining' CombiningDotAbove 'A' = Just '\x0226'
composeCombining' CombiningDotAbove 'a' = Just '\x0227'
composeCombining' CombiningCedilla 'E' = Just '\x0228'
composeCombining' CombiningCedilla 'e' = Just '\x0229'
composeCombining' CombiningMacron '\x00d6' = Just '\x022a'
composeCombining' CombiningMacron '\x00f6' = Just '\x022b'
composeCombining' CombiningMacron '\x00d5' = Just '\x022c'
composeCombining' CombiningMacron '\x00f5' = Just '\x022d'
composeCombining' CombiningDotAbove 'O' = Just '\x022e'
composeCombining' CombiningDotAbove 'o' = Just '\x022f'
composeCombining' CombiningMacron '\x022e' = Just '\x0230'
composeCombining' CombiningMacron '\x022f' = Just '\x0231'
composeCombining' CombiningMacron 'Y' = Just '\x0232'
composeCombining' CombiningMacron 'y' = Just '\x0233'
composeCombining' CombiningAcuteAccent '\x0308' = Just '\x0344'
composeCombining' CombiningAcuteAccent '\x00a8' = Just '\x0385'
composeCombining' CombiningAcuteAccent '\x0391' = Just '\x0386'
composeCombining' CombiningAcuteAccent '\x0395' = Just '\x0388'
composeCombining' CombiningAcuteAccent '\x0397' = Just '\x0389'
composeCombining' CombiningAcuteAccent '\x0399' = Just '\x038a'
composeCombining' CombiningAcuteAccent '\x039f' = Just '\x038c'
composeCombining' CombiningAcuteAccent '\x03a5' = Just '\x038e'
composeCombining' CombiningAcuteAccent '\x03a9' = Just '\x038f'
composeCombining' CombiningAcuteAccent '\x03ca' = Just '\x0390'
composeCombining' CombiningDiaeresis '\x0399' = Just '\x03aa'
composeCombining' CombiningDiaeresis '\x03a5' = Just '\x03ab'
composeCombining' CombiningAcuteAccent '\x03b1' = Just '\x03ac'
composeCombining' CombiningAcuteAccent '\x03b5' = Just '\x03ad'
composeCombining' CombiningAcuteAccent '\x03b7' = Just '\x03ae'
composeCombining' CombiningAcuteAccent '\x03b9' = Just '\x03af'
composeCombining' CombiningAcuteAccent '\x03cb' = Just '\x03b0'
composeCombining' CombiningDiaeresis '\x03b9' = Just '\x03ca'
composeCombining' CombiningDiaeresis '\x03c5' = Just '\x03cb'
composeCombining' CombiningAcuteAccent '\x03bf' = Just '\x03cc'
composeCombining' CombiningAcuteAccent '\x03c5' = Just '\x03cd'
composeCombining' CombiningAcuteAccent '\x03c9' = Just '\x03ce'
composeCombining' CombiningAcuteAccent '\x03d2' = Just '\x03d3'
composeCombining' CombiningDiaeresis '\x03d2' = Just '\x03d4'
composeCombining' CombiningGraveAccent '\x0415' = Just '\x0400'
composeCombining' CombiningDiaeresis '\x0415' = Just '\x0401'
composeCombining' CombiningAcuteAccent '\x0413' = Just '\x0403'
composeCombining' CombiningDiaeresis '\x0406' = Just '\x0407'
composeCombining' CombiningAcuteAccent '\x041a' = Just '\x040c'
composeCombining' CombiningGraveAccent '\x0418' = Just '\x040d'
composeCombining' CombiningBreve '\x0423' = Just '\x040e'
composeCombining' CombiningBreve '\x0418' = Just '\x0419'
composeCombining' CombiningBreve '\x0438' = Just '\x0439'
composeCombining' CombiningGraveAccent '\x0435' = Just '\x0450'
composeCombining' CombiningDiaeresis '\x0435' = Just '\x0451'
composeCombining' CombiningAcuteAccent '\x0433' = Just '\x0453'
composeCombining' CombiningDiaeresis '\x0456' = Just '\x0457'
composeCombining' CombiningAcuteAccent '\x043a' = Just '\x045c'
composeCombining' CombiningGraveAccent '\x0438' = Just '\x045d'
composeCombining' CombiningBreve '\x0443' = Just '\x045e'
composeCombining' CombiningDoubleGraveAccent '\x0474' = Just '\x0476'
composeCombining' CombiningDoubleGraveAccent '\x0475' = Just '\x0477'
composeCombining' CombiningBreve '\x0416' = Just '\x04c1'
composeCombining' CombiningBreve '\x0436' = Just '\x04c2'
composeCombining' CombiningBreve '\x0410' = Just '\x04d0'
composeCombining' CombiningBreve '\x0430' = Just '\x04d1'
composeCombining' CombiningDiaeresis '\x0410' = Just '\x04d2'
composeCombining' CombiningDiaeresis '\x0430' = Just '\x04d3'
composeCombining' CombiningBreve '\x0415' = Just '\x04d6'
composeCombining' CombiningBreve '\x0435' = Just '\x04d7'
composeCombining' CombiningDiaeresis '\x04d8' = Just '\x04da'
composeCombining' CombiningDiaeresis '\x04d9' = Just '\x04db'
composeCombining' CombiningDiaeresis '\x0416' = Just '\x04dc'
composeCombining' CombiningDiaeresis '\x0436' = Just '\x04dd'
composeCombining' CombiningDiaeresis '\x0417' = Just '\x04de'
composeCombining' CombiningDiaeresis '\x0437' = Just '\x04df'
composeCombining' CombiningMacron '\x0418' = Just '\x04e2'
composeCombining' CombiningMacron '\x0438' = Just '\x04e3'
composeCombining' CombiningDiaeresis '\x0418' = Just '\x04e4'
composeCombining' CombiningDiaeresis '\x0438' = Just '\x04e5'
composeCombining' CombiningDiaeresis '\x041e' = Just '\x04e6'
composeCombining' CombiningDiaeresis '\x043e' = Just '\x04e7'
composeCombining' CombiningDiaeresis '\x04e8' = Just '\x04ea'
composeCombining' CombiningDiaeresis '\x04e9' = Just '\x04eb'
composeCombining' CombiningDiaeresis '\x042d' = Just '\x04ec'
composeCombining' CombiningDiaeresis '\x044d' = Just '\x04ed'
composeCombining' CombiningMacron '\x0423' = Just '\x04ee'
composeCombining' CombiningMacron '\x0443' = Just '\x04ef'
composeCombining' CombiningDiaeresis '\x0423' = Just '\x04f0'
composeCombining' CombiningDiaeresis '\x0443' = Just '\x04f1'
composeCombining' CombiningDoubleAcuteAccent '\x0423' = Just '\x04f2'
composeCombining' CombiningDoubleAcuteAccent '\x0443' = Just '\x04f3'
composeCombining' CombiningDiaeresis '\x0427' = Just '\x04f4'
composeCombining' CombiningDiaeresis '\x0447' = Just '\x04f5'
composeCombining' CombiningDiaeresis '\x042b' = Just '\x04f8'
composeCombining' CombiningDiaeresis '\x044b' = Just '\x04f9'
composeCombining' ArabicMaddahAbove '\x0627' = Just '\x0622'
composeCombining' ArabicHamzaAbove '\x0627' = Just '\x0623'
composeCombining' ArabicHamzaAbove '\x0648' = Just '\x0624'
composeCombining' ArabicHamzaBelow '\x0627' = Just '\x0625'
composeCombining' ArabicHamzaAbove '\x064a' = Just '\x0626'
composeCombining' ArabicHamzaAbove '\x06d5' = Just '\x06c0'
composeCombining' ArabicHamzaAbove '\x06c1' = Just '\x06c2'
composeCombining' ArabicHamzaAbove '\x06d2' = Just '\x06d3'
composeCombining' DevanagariSignNukta '\x0928' = Just '\x0929'
composeCombining' DevanagariSignNukta '\x0930' = Just '\x0931'
composeCombining' DevanagariSignNukta '\x0933' = Just '\x0934'
composeCombining' DevanagariSignNukta '\x0915' = Just '\x0958'
composeCombining' DevanagariSignNukta '\x0916' = Just '\x0959'
composeCombining' DevanagariSignNukta '\x0917' = Just '\x095a'
composeCombining' DevanagariSignNukta '\x091c' = Just '\x095b'
composeCombining' DevanagariSignNukta '\x0921' = Just '\x095c'
composeCombining' DevanagariSignNukta '\x0922' = Just '\x095d'
composeCombining' DevanagariSignNukta '\x092b' = Just '\x095e'
composeCombining' DevanagariSignNukta '\x092f' = Just '\x095f'
composeCombining' BengaliVowelSignAa '\x09c7' = Just '\x09cb'
composeCombining' BengaliAuLengthMark '\x09c7' = Just '\x09cc'
composeCombining' BengaliSignNukta '\x09a1' = Just '\x09dc'
composeCombining' BengaliSignNukta '\x09a2' = Just '\x09dd'
composeCombining' BengaliSignNukta '\x09af' = Just '\x09df'
composeCombining' GurmukhiSignNukta '\x0a32' = Just '\x0a33'
composeCombining' GurmukhiSignNukta '\x0a38' = Just '\x0a36'
composeCombining' GurmukhiSignNukta '\x0a16' = Just '\x0a59'
composeCombining' GurmukhiSignNukta '\x0a17' = Just '\x0a5a'
composeCombining' GurmukhiSignNukta '\x0a1c' = Just '\x0a5b'
composeCombining' GurmukhiSignNukta '\x0a2b' = Just '\x0a5e'
composeCombining' OriyaAiLengthMark '\x0b47' = Just '\x0b48'
composeCombining' OriyaVowelSignAa '\x0b47' = Just '\x0b4b'
composeCombining' OriyaAuLengthMark '\x0b47' = Just '\x0b4c'
composeCombining' OriyaSignNukta '\x0b21' = Just '\x0b5c'
composeCombining' OriyaSignNukta '\x0b22' = Just '\x0b5d'
composeCombining' TamilAuLengthMark '\x0b92' = Just '\x0b94'
composeCombining' TamilVowelSignAa '\x0bc6' = Just '\x0bca'
composeCombining' TamilVowelSignAa '\x0bc7' = Just '\x0bcb'
composeCombining' TamilAuLengthMark '\x0bc6' = Just '\x0bcc'
composeCombining' TeluguAiLengthMark '\x0c46' = Just '\x0c48'
composeCombining' KannadaLengthMark '\x0cbf' = Just '\x0cc0'
composeCombining' KannadaLengthMark '\x0cc6' = Just '\x0cc7'
composeCombining' KannadaAiLengthMark '\x0cc6' = Just '\x0cc8'
composeCombining' KannadaVowelSignUu '\x0cc6' = Just '\x0cca'
composeCombining' KannadaLengthMark '\x0cca' = Just '\x0ccb'
composeCombining' MalayalamVowelSignAa '\x0d46' = Just '\x0d4a'
composeCombining' MalayalamVowelSignAa '\x0d47' = Just '\x0d4b'
composeCombining' MalayalamAuLengthMark '\x0d46' = Just '\x0d4c'
composeCombining' SinhalaSignAlLakuna '\x0dd9' = Just '\x0dda'
composeCombining' SinhalaVowelSignAelaPilla '\x0dd9' = Just '\x0ddc'
composeCombining' SinhalaSignAlLakuna '\x0ddc' = Just '\x0ddd'
composeCombining' SinhalaVowelSignGayanukitta '\x0dd9' = Just '\x0dde'
composeCombining' TibetanSubjoinedLetterHa '\x0f42' = Just '\x0f43'
composeCombining' TibetanSubjoinedLetterHa '\x0f4c' = Just '\x0f4d'
composeCombining' TibetanSubjoinedLetterHa '\x0f51' = Just '\x0f52'
composeCombining' TibetanSubjoinedLetterHa '\x0f56' = Just '\x0f57'
composeCombining' TibetanSubjoinedLetterHa '\x0f5b' = Just '\x0f5c'
composeCombining' TibetanSubjoinedLetterSsa '\x0f40' = Just '\x0f69'
composeCombining' TibetanVowelSignI '\x0f71' = Just '\x0f73'
composeCombining' TibetanVowelSignU '\x0f71' = Just '\x0f75'
composeCombining' TibetanVowelSignReversedI '\x0fb2' = Just '\x0f76'
composeCombining' TibetanVowelSignReversedI '\x0fb3' = Just '\x0f78'
composeCombining' TibetanVowelSignReversedI '\x0f71' = Just '\x0f81'
composeCombining' TibetanSubjoinedLetterHa '\x0f92' = Just '\x0f93'
composeCombining' TibetanSubjoinedLetterHa '\x0f9c' = Just '\x0f9d'
composeCombining' TibetanSubjoinedLetterHa '\x0fa1' = Just '\x0fa2'
composeCombining' TibetanSubjoinedLetterHa '\x0fa6' = Just '\x0fa7'
composeCombining' TibetanSubjoinedLetterHa '\x0fab' = Just '\x0fac'
composeCombining' TibetanSubjoinedLetterSsa '\x0f90' = Just '\x0fb9'
composeCombining' MyanmarVowelSignIi '\x1025' = Just '\x1026'
composeCombining' BalineseVowelSignTedung '\x1b05' = Just '\x1b06'
composeCombining' BalineseVowelSignTedung '\x1b07' = Just '\x1b08'
composeCombining' BalineseVowelSignTedung '\x1b09' = Just '\x1b0a'
composeCombining' BalineseVowelSignTedung '\x1b0b' = Just '\x1b0c'
composeCombining' BalineseVowelSignTedung '\x1b0d' = Just '\x1b0e'
composeCombining' BalineseVowelSignTedung '\x1b11' = Just '\x1b12'
composeCombining' BalineseVowelSignTedung '\x1b3a' = Just '\x1b3b'
composeCombining' BalineseVowelSignTedung '\x1b3c' = Just '\x1b3d'
composeCombining' BalineseVowelSignTedung '\x1b3e' = Just '\x1b40'
composeCombining' BalineseVowelSignTedung '\x1b3f' = Just '\x1b41'
composeCombining' BalineseVowelSignTedung '\x1b42' = Just '\x1b43'
composeCombining' CombiningRingBelow 'A' = Just '\x1e00'
composeCombining' CombiningRingBelow 'a' = Just '\x1e01'
composeCombining' CombiningDotAbove 'B' = Just '\x1e02'
composeCombining' CombiningDotAbove 'b' = Just '\x1e03'
composeCombining' CombiningDotBelow 'B' = Just '\x1e04'
composeCombining' CombiningDotBelow 'b' = Just '\x1e05'
composeCombining' CombiningMacronBelow 'B' = Just '\x1e06'
composeCombining' CombiningMacronBelow 'b' = Just '\x1e07'
composeCombining' CombiningAcuteAccent '\x00c7' = Just '\x1e08'
composeCombining' CombiningAcuteAccent '\x00e7' = Just '\x1e09'
composeCombining' CombiningDotAbove 'D' = Just '\x1e0a'
composeCombining' CombiningDotAbove 'd' = Just '\x1e0b'
composeCombining' CombiningDotBelow 'D' = Just '\x1e0c'
composeCombining' CombiningDotBelow 'd' = Just '\x1e0d'
composeCombining' CombiningMacronBelow 'D' = Just '\x1e0e'
composeCombining' CombiningMacronBelow 'd' = Just '\x1e0f'
composeCombining' CombiningCedilla 'D' = Just '\x1e10'
composeCombining' CombiningCedilla 'd' = Just '\x1e11'
composeCombining' CombiningCircumflexAccentBelow 'D' = Just '\x1e12'
composeCombining' CombiningCircumflexAccentBelow 'd' = Just '\x1e13'
composeCombining' CombiningGraveAccent '\x0112' = Just '\x1e14'
composeCombining' CombiningGraveAccent '\x0113' = Just '\x1e15'
composeCombining' CombiningAcuteAccent '\x0112' = Just '\x1e16'
composeCombining' CombiningAcuteAccent '\x0113' = Just '\x1e17'
composeCombining' CombiningCircumflexAccentBelow 'E' = Just '\x1e18'
composeCombining' CombiningCircumflexAccentBelow 'e' = Just '\x1e19'
composeCombining' CombiningTildeBelow 'E' = Just '\x1e1a'
composeCombining' CombiningTildeBelow 'e' = Just '\x1e1b'
composeCombining' CombiningBreve '\x0228' = Just '\x1e1c'
composeCombining' CombiningBreve '\x0229' = Just '\x1e1d'
composeCombining' CombiningDotAbove 'F' = Just '\x1e1e'
composeCombining' CombiningDotAbove 'f' = Just '\x1e1f'
composeCombining' CombiningMacron 'G' = Just '\x1e20'
composeCombining' CombiningMacron 'g' = Just '\x1e21'
composeCombining' CombiningDotAbove 'H' = Just '\x1e22'
composeCombining' CombiningDotAbove 'h' = Just '\x1e23'
composeCombining' CombiningDotBelow 'H' = Just '\x1e24'
composeCombining' CombiningDotBelow 'h' = Just '\x1e25'
composeCombining' CombiningDiaeresis 'H' = Just '\x1e26'
composeCombining' CombiningDiaeresis 'h' = Just '\x1e27'
composeCombining' CombiningCedilla 'H' = Just '\x1e28'
composeCombining' CombiningCedilla 'h' = Just '\x1e29'
composeCombining' CombiningBreveBelow 'H' = Just '\x1e2a'
composeCombining' CombiningBreveBelow 'h' = Just '\x1e2b'
composeCombining' CombiningTildeBelow 'I' = Just '\x1e2c'
composeCombining' CombiningTildeBelow 'i' = Just '\x1e2d'
composeCombining' CombiningAcuteAccent '\x00cf' = Just '\x1e2e'
composeCombining' CombiningAcuteAccent '\x00ef' = Just '\x1e2f'
composeCombining' CombiningAcuteAccent 'K' = Just '\x1e30'
composeCombining' CombiningAcuteAccent 'k' = Just '\x1e31'
composeCombining' CombiningDotBelow 'K' = Just '\x1e32'
composeCombining' CombiningDotBelow 'k' = Just '\x1e33'
composeCombining' CombiningMacronBelow 'K' = Just '\x1e34'
composeCombining' CombiningMacronBelow 'k' = Just '\x1e35'
composeCombining' CombiningDotBelow 'L' = Just '\x1e36'
composeCombining' CombiningDotBelow 'l' = Just '\x1e37'
composeCombining' CombiningMacron '\x1e36' = Just '\x1e38'
composeCombining' CombiningMacron '\x1e37' = Just '\x1e39'
composeCombining' CombiningMacronBelow 'L' = Just '\x1e3a'
composeCombining' CombiningMacronBelow 'l' = Just '\x1e3b'
composeCombining' CombiningCircumflexAccentBelow 'L' = Just '\x1e3c'
composeCombining' CombiningCircumflexAccentBelow 'l' = Just '\x1e3d'
composeCombining' CombiningAcuteAccent 'M' = Just '\x1e3e'
composeCombining' CombiningAcuteAccent 'm' = Just '\x1e3f'
composeCombining' CombiningDotAbove 'M' = Just '\x1e40'
composeCombining' CombiningDotAbove 'm' = Just '\x1e41'
composeCombining' CombiningDotBelow 'M' = Just '\x1e42'
composeCombining' CombiningDotBelow 'm' = Just '\x1e43'
composeCombining' CombiningDotAbove 'N' = Just '\x1e44'
composeCombining' CombiningDotAbove 'n' = Just '\x1e45'
composeCombining' CombiningDotBelow 'N' = Just '\x1e46'
composeCombining' CombiningDotBelow 'n' = Just '\x1e47'
composeCombining' CombiningMacronBelow 'N' = Just '\x1e48'
composeCombining' CombiningMacronBelow 'n' = Just '\x1e49'
composeCombining' CombiningCircumflexAccentBelow 'N' = Just '\x1e4a'
composeCombining' CombiningCircumflexAccentBelow 'n' = Just '\x1e4b'
composeCombining' CombiningAcuteAccent '\x00d5' = Just '\x1e4c'
composeCombining' CombiningAcuteAccent '\x00f5' = Just '\x1e4d'
composeCombining' CombiningDiaeresis '\x00d5' = Just '\x1e4e'
composeCombining' CombiningDiaeresis '\x00f5' = Just '\x1e4f'
composeCombining' CombiningGraveAccent '\x014c' = Just '\x1e50'
composeCombining' CombiningGraveAccent '\x014d' = Just '\x1e51'
composeCombining' CombiningAcuteAccent '\x014c' = Just '\x1e52'
composeCombining' CombiningAcuteAccent '\x014d' = Just '\x1e53'
composeCombining' CombiningAcuteAccent 'P' = Just '\x1e54'
composeCombining' CombiningAcuteAccent 'p' = Just '\x1e55'
composeCombining' CombiningDotAbove 'P' = Just '\x1e56'
composeCombining' CombiningDotAbove 'p' = Just '\x1e57'
composeCombining' CombiningDotAbove 'R' = Just '\x1e58'
composeCombining' CombiningDotAbove 'r' = Just '\x1e59'
composeCombining' CombiningDotBelow 'R' = Just '\x1e5a'
composeCombining' CombiningDotBelow 'r' = Just '\x1e5b'
composeCombining' CombiningMacron '\x1e5a' = Just '\x1e5c'
composeCombining' CombiningMacron '\x1e5b' = Just '\x1e5d'
composeCombining' CombiningMacronBelow 'R' = Just '\x1e5e'
composeCombining' CombiningMacronBelow 'r' = Just '\x1e5f'
composeCombining' CombiningDotAbove 'S' = Just '\x1e60'
composeCombining' CombiningDotAbove 's' = Just '\x1e61'
composeCombining' CombiningDotBelow 'S' = Just '\x1e62'
composeCombining' CombiningDotBelow 's' = Just '\x1e63'
composeCombining' CombiningDotAbove '\x015a' = Just '\x1e64'
composeCombining' CombiningDotAbove '\x015b' = Just '\x1e65'
composeCombining' CombiningDotAbove '\x0160' = Just '\x1e66'
composeCombining' CombiningDotAbove '\x0161' = Just '\x1e67'
composeCombining' CombiningDotAbove '\x1e62' = Just '\x1e68'
composeCombining' CombiningDotAbove '\x1e63' = Just '\x1e69'
composeCombining' CombiningDotAbove 'T' = Just '\x1e6a'
composeCombining' CombiningDotAbove 't' = Just '\x1e6b'
composeCombining' CombiningDotBelow 'T' = Just '\x1e6c'
composeCombining' CombiningDotBelow 't' = Just '\x1e6d'
composeCombining' CombiningMacronBelow 'T' = Just '\x1e6e'
composeCombining' CombiningMacronBelow 't' = Just '\x1e6f'
composeCombining' CombiningCircumflexAccentBelow 'T' = Just '\x1e70'
composeCombining' CombiningCircumflexAccentBelow 't' = Just '\x1e71'
composeCombining' CombiningDiaeresisBelow 'U' = Just '\x1e72'
composeCombining' CombiningDiaeresisBelow 'u' = Just '\x1e73'
composeCombining' CombiningTildeBelow 'U' = Just '\x1e74'
composeCombining' CombiningTildeBelow 'u' = Just '\x1e75'
composeCombining' CombiningCircumflexAccentBelow 'U' = Just '\x1e76'
composeCombining' CombiningCircumflexAccentBelow 'u' = Just '\x1e77'
composeCombining' CombiningAcuteAccent '\x0168' = Just '\x1e78'
composeCombining' CombiningAcuteAccent '\x0169' = Just '\x1e79'
composeCombining' CombiningDiaeresis '\x016a' = Just '\x1e7a'
composeCombining' CombiningDiaeresis '\x016b' = Just '\x1e7b'
composeCombining' CombiningTilde 'V' = Just '\x1e7c'
composeCombining' CombiningTilde 'v' = Just '\x1e7d'
composeCombining' CombiningDotBelow 'V' = Just '\x1e7e'
composeCombining' CombiningDotBelow 'v' = Just '\x1e7f'
composeCombining' CombiningGraveAccent 'W' = Just '\x1e80'
composeCombining' CombiningGraveAccent 'w' = Just '\x1e81'
composeCombining' CombiningAcuteAccent 'W' = Just '\x1e82'
composeCombining' CombiningAcuteAccent 'w' = Just '\x1e83'
composeCombining' CombiningDiaeresis 'W' = Just '\x1e84'
composeCombining' CombiningDiaeresis 'w' = Just '\x1e85'
composeCombining' CombiningDotAbove 'W' = Just '\x1e86'
composeCombining' CombiningDotAbove 'w' = Just '\x1e87'
composeCombining' CombiningDotBelow 'W' = Just '\x1e88'
composeCombining' CombiningDotBelow 'w' = Just '\x1e89'
composeCombining' CombiningDotAbove 'X' = Just '\x1e8a'
composeCombining' CombiningDotAbove 'x' = Just '\x1e8b'
composeCombining' CombiningDiaeresis 'X' = Just '\x1e8c'
composeCombining' CombiningDiaeresis 'x' = Just '\x1e8d'
composeCombining' CombiningDotAbove 'Y' = Just '\x1e8e'
composeCombining' CombiningDotAbove 'y' = Just '\x1e8f'
composeCombining' CombiningCircumflexAccent 'Z' = Just '\x1e90'
composeCombining' CombiningCircumflexAccent 'z' = Just '\x1e91'
composeCombining' CombiningDotBelow 'Z' = Just '\x1e92'
composeCombining' CombiningDotBelow 'z' = Just '\x1e93'
composeCombining' CombiningMacronBelow 'Z' = Just '\x1e94'
composeCombining' CombiningMacronBelow 'z' = Just '\x1e95'
composeCombining' CombiningMacronBelow 'h' = Just '\x1e96'
composeCombining' CombiningDiaeresis 't' = Just '\x1e97'
composeCombining' CombiningRingAbove 'w' = Just '\x1e98'
composeCombining' CombiningRingAbove 'y' = Just '\x1e99'
composeCombining' CombiningDotAbove '\x017f' = Just '\x1e9b'
composeCombining' CombiningDotBelow 'A' = Just '\x1ea0'
composeCombining' CombiningDotBelow 'a' = Just '\x1ea1'
composeCombining' CombiningHookAbove 'A' = Just '\x1ea2'
composeCombining' CombiningHookAbove 'a' = Just '\x1ea3'
composeCombining' CombiningAcuteAccent '\x00c2' = Just '\x1ea4'
composeCombining' CombiningAcuteAccent '\x00e2' = Just '\x1ea5'
composeCombining' CombiningGraveAccent '\x00c2' = Just '\x1ea6'
composeCombining' CombiningGraveAccent '\x00e2' = Just '\x1ea7'
composeCombining' CombiningHookAbove '\x00c2' = Just '\x1ea8'
composeCombining' CombiningHookAbove '\x00e2' = Just '\x1ea9'
composeCombining' CombiningTilde '\x00c2' = Just '\x1eaa'
composeCombining' CombiningTilde '\x00e2' = Just '\x1eab'
composeCombining' CombiningCircumflexAccent '\x1ea0' = Just '\x1eac'
composeCombining' CombiningCircumflexAccent '\x1ea1' = Just '\x1ead'
composeCombining' CombiningAcuteAccent '\x0102' = Just '\x1eae'
composeCombining' CombiningAcuteAccent '\x0103' = Just '\x1eaf'
composeCombining' CombiningGraveAccent '\x0102' = Just '\x1eb0'
composeCombining' CombiningGraveAccent '\x0103' = Just '\x1eb1'
composeCombining' CombiningHookAbove '\x0102' = Just '\x1eb2'
composeCombining' CombiningHookAbove '\x0103' = Just '\x1eb3'
composeCombining' CombiningTilde '\x0102' = Just '\x1eb4'
composeCombining' CombiningTilde '\x0103' = Just '\x1eb5'
composeCombining' CombiningBreve '\x1ea0' = Just '\x1eb6'
composeCombining' CombiningBreve '\x1ea1' = Just '\x1eb7'
composeCombining' CombiningDotBelow 'E' = Just '\x1eb8'
composeCombining' CombiningDotBelow 'e' = Just '\x1eb9'
composeCombining' CombiningHookAbove 'E' = Just '\x1eba'
composeCombining' CombiningHookAbove 'e' = Just '\x1ebb'
composeCombining' CombiningTilde 'E' = Just '\x1ebc'
composeCombining' CombiningTilde 'e' = Just '\x1ebd'
composeCombining' CombiningAcuteAccent '\x00ca' = Just '\x1ebe'
composeCombining' CombiningAcuteAccent '\x00ea' = Just '\x1ebf'
composeCombining' CombiningGraveAccent '\x00ca' = Just '\x1ec0'
composeCombining' CombiningGraveAccent '\x00ea' = Just '\x1ec1'
composeCombining' CombiningHookAbove '\x00ca' = Just '\x1ec2'
composeCombining' CombiningHookAbove '\x00ea' = Just '\x1ec3'
composeCombining' CombiningTilde '\x00ca' = Just '\x1ec4'
composeCombining' CombiningTilde '\x00ea' = Just '\x1ec5'
composeCombining' CombiningCircumflexAccent '\x1eb8' = Just '\x1ec6'
composeCombining' CombiningCircumflexAccent '\x1eb9' = Just '\x1ec7'
composeCombining' CombiningHookAbove 'I' = Just '\x1ec8'
composeCombining' CombiningHookAbove 'i' = Just '\x1ec9'
composeCombining' CombiningDotBelow 'I' = Just '\x1eca'
composeCombining' CombiningDotBelow 'i' = Just '\x1ecb'
composeCombining' CombiningDotBelow 'O' = Just '\x1ecc'
composeCombining' CombiningDotBelow 'o' = Just '\x1ecd'
composeCombining' CombiningHookAbove 'O' = Just '\x1ece'
composeCombining' CombiningHookAbove 'o' = Just '\x1ecf'
composeCombining' CombiningAcuteAccent '\x00d4' = Just '\x1ed0'
composeCombining' CombiningAcuteAccent '\x00f4' = Just '\x1ed1'
composeCombining' CombiningGraveAccent '\x00d4' = Just '\x1ed2'
composeCombining' CombiningGraveAccent '\x00f4' = Just '\x1ed3'
composeCombining' CombiningHookAbove '\x00d4' = Just '\x1ed4'
composeCombining' CombiningHookAbove '\x00f4' = Just '\x1ed5'
composeCombining' CombiningTilde '\x00d4' = Just '\x1ed6'
composeCombining' CombiningTilde '\x00f4' = Just '\x1ed7'
composeCombining' CombiningCircumflexAccent '\x1ecc' = Just '\x1ed8'
composeCombining' CombiningCircumflexAccent '\x1ecd' = Just '\x1ed9'
composeCombining' CombiningAcuteAccent '\x01a0' = Just '\x1eda'
composeCombining' CombiningAcuteAccent '\x01a1' = Just '\x1edb'
composeCombining' CombiningGraveAccent '\x01a0' = Just '\x1edc'
composeCombining' CombiningGraveAccent '\x01a1' = Just '\x1edd'
composeCombining' CombiningHookAbove '\x01a0' = Just '\x1ede'
composeCombining' CombiningHookAbove '\x01a1' = Just '\x1edf'
composeCombining' CombiningTilde '\x01a0' = Just '\x1ee0'
composeCombining' CombiningTilde '\x01a1' = Just '\x1ee1'
composeCombining' CombiningDotBelow '\x01a0' = Just '\x1ee2'
composeCombining' CombiningDotBelow '\x01a1' = Just '\x1ee3'
composeCombining' CombiningDotBelow 'U' = Just '\x1ee4'
composeCombining' CombiningDotBelow 'u' = Just '\x1ee5'
composeCombining' CombiningHookAbove 'U' = Just '\x1ee6'
composeCombining' CombiningHookAbove 'u' = Just '\x1ee7'
composeCombining' CombiningAcuteAccent '\x01af' = Just '\x1ee8'
composeCombining' CombiningAcuteAccent '\x01b0' = Just '\x1ee9'
composeCombining' CombiningGraveAccent '\x01af' = Just '\x1eea'
composeCombining' CombiningGraveAccent '\x01b0' = Just '\x1eeb'
composeCombining' CombiningHookAbove '\x01af' = Just '\x1eec'
composeCombining' CombiningHookAbove '\x01b0' = Just '\x1eed'
composeCombining' CombiningTilde '\x01af' = Just '\x1eee'
composeCombining' CombiningTilde '\x01b0' = Just '\x1eef'
composeCombining' CombiningDotBelow '\x01af' = Just '\x1ef0'
composeCombining' CombiningDotBelow '\x01b0' = Just '\x1ef1'
composeCombining' CombiningGraveAccent 'Y' = Just '\x1ef2'
composeCombining' CombiningGraveAccent 'y' = Just '\x1ef3'
composeCombining' CombiningDotBelow 'Y' = Just '\x1ef4'
composeCombining' CombiningDotBelow 'y' = Just '\x1ef5'
composeCombining' CombiningHookAbove 'Y' = Just '\x1ef6'
composeCombining' CombiningHookAbove 'y' = Just '\x1ef7'
composeCombining' CombiningTilde 'Y' = Just '\x1ef8'
composeCombining' CombiningTilde 'y' = Just '\x1ef9'
composeCombining' CombiningCommaAbove '\x03b1' = Just '\x1f00'
composeCombining' CombiningReversedCommaAbove '\x03b1' = Just '\x1f01'
composeCombining' CombiningGraveAccent '\x1f00' = Just '\x1f02'
composeCombining' CombiningGraveAccent '\x1f01' = Just '\x1f03'
composeCombining' CombiningAcuteAccent '\x1f00' = Just '\x1f04'
composeCombining' CombiningAcuteAccent '\x1f01' = Just '\x1f05'
composeCombining' CombiningGreekPerispomeni '\x1f00' = Just '\x1f06'
composeCombining' CombiningGreekPerispomeni '\x1f01' = Just '\x1f07'
composeCombining' CombiningCommaAbove '\x0391' = Just '\x1f08'
composeCombining' CombiningReversedCommaAbove '\x0391' = Just '\x1f09'
composeCombining' CombiningGraveAccent '\x1f08' = Just '\x1f0a'
composeCombining' CombiningGraveAccent '\x1f09' = Just '\x1f0b'
composeCombining' CombiningAcuteAccent '\x1f08' = Just '\x1f0c'
composeCombining' CombiningAcuteAccent '\x1f09' = Just '\x1f0d'
composeCombining' CombiningGreekPerispomeni '\x1f08' = Just '\x1f0e'
composeCombining' CombiningGreekPerispomeni '\x1f09' = Just '\x1f0f'
composeCombining' CombiningCommaAbove '\x03b5' = Just '\x1f10'
composeCombining' CombiningReversedCommaAbove '\x03b5' = Just '\x1f11'
composeCombining' CombiningGraveAccent '\x1f10' = Just '\x1f12'
composeCombining' CombiningGraveAccent '\x1f11' = Just '\x1f13'
composeCombining' CombiningAcuteAccent '\x1f10' = Just '\x1f14'
composeCombining' CombiningAcuteAccent '\x1f11' = Just '\x1f15'
composeCombining' CombiningCommaAbove '\x0395' = Just '\x1f18'
composeCombining' CombiningReversedCommaAbove '\x0395' = Just '\x1f19'
composeCombining' CombiningGraveAccent '\x1f18' = Just '\x1f1a'
composeCombining' CombiningGraveAccent '\x1f19' = Just '\x1f1b'
composeCombining' CombiningAcuteAccent '\x1f18' = Just '\x1f1c'
composeCombining' CombiningAcuteAccent '\x1f19' = Just '\x1f1d'
composeCombining' CombiningCommaAbove '\x03b7' = Just '\x1f20'
composeCombining' CombiningReversedCommaAbove '\x03b7' = Just '\x1f21'
composeCombining' CombiningGraveAccent '\x1f20' = Just '\x1f22'
composeCombining' CombiningGraveAccent '\x1f21' = Just '\x1f23'
composeCombining' CombiningAcuteAccent '\x1f20' = Just '\x1f24'
composeCombining' CombiningAcuteAccent '\x1f21' = Just '\x1f25'
composeCombining' CombiningGreekPerispomeni '\x1f20' = Just '\x1f26'
composeCombining' CombiningGreekPerispomeni '\x1f21' = Just '\x1f27'
composeCombining' CombiningCommaAbove '\x0397' = Just '\x1f28'
composeCombining' CombiningReversedCommaAbove '\x0397' = Just '\x1f29'
composeCombining' CombiningGraveAccent '\x1f28' = Just '\x1f2a'
composeCombining' CombiningGraveAccent '\x1f29' = Just '\x1f2b'
composeCombining' CombiningAcuteAccent '\x1f28' = Just '\x1f2c'
composeCombining' CombiningAcuteAccent '\x1f29' = Just '\x1f2d'
composeCombining' CombiningGreekPerispomeni '\x1f28' = Just '\x1f2e'
composeCombining' CombiningGreekPerispomeni '\x1f29' = Just '\x1f2f'
composeCombining' CombiningCommaAbove '\x03b9' = Just '\x1f30'
composeCombining' CombiningReversedCommaAbove '\x03b9' = Just '\x1f31'
composeCombining' CombiningGraveAccent '\x1f30' = Just '\x1f32'
composeCombining' CombiningGraveAccent '\x1f31' = Just '\x1f33'
composeCombining' CombiningAcuteAccent '\x1f30' = Just '\x1f34'
composeCombining' CombiningAcuteAccent '\x1f31' = Just '\x1f35'
composeCombining' CombiningGreekPerispomeni '\x1f30' = Just '\x1f36'
composeCombining' CombiningGreekPerispomeni '\x1f31' = Just '\x1f37'
composeCombining' CombiningCommaAbove '\x0399' = Just '\x1f38'
composeCombining' CombiningReversedCommaAbove '\x0399' = Just '\x1f39'
composeCombining' CombiningGraveAccent '\x1f38' = Just '\x1f3a'
composeCombining' CombiningGraveAccent '\x1f39' = Just '\x1f3b'
composeCombining' CombiningAcuteAccent '\x1f38' = Just '\x1f3c'
composeCombining' CombiningAcuteAccent '\x1f39' = Just '\x1f3d'
composeCombining' CombiningGreekPerispomeni '\x1f38' = Just '\x1f3e'
composeCombining' CombiningGreekPerispomeni '\x1f39' = Just '\x1f3f'
composeCombining' CombiningCommaAbove '\x03bf' = Just '\x1f40'
composeCombining' CombiningReversedCommaAbove '\x03bf' = Just '\x1f41'
composeCombining' CombiningGraveAccent '\x1f40' = Just '\x1f42'
composeCombining' CombiningGraveAccent '\x1f41' = Just '\x1f43'
composeCombining' CombiningAcuteAccent '\x1f40' = Just '\x1f44'
composeCombining' CombiningAcuteAccent '\x1f41' = Just '\x1f45'
composeCombining' CombiningCommaAbove '\x039f' = Just '\x1f48'
composeCombining' CombiningReversedCommaAbove '\x039f' = Just '\x1f49'
composeCombining' CombiningGraveAccent '\x1f48' = Just '\x1f4a'
composeCombining' CombiningGraveAccent '\x1f49' = Just '\x1f4b'
composeCombining' CombiningAcuteAccent '\x1f48' = Just '\x1f4c'
composeCombining' CombiningAcuteAccent '\x1f49' = Just '\x1f4d'
composeCombining' CombiningCommaAbove '\x03c5' = Just '\x1f50'
composeCombining' CombiningReversedCommaAbove '\x03c5' = Just '\x1f51'
composeCombining' CombiningGraveAccent '\x1f50' = Just '\x1f52'
composeCombining' CombiningGraveAccent '\x1f51' = Just '\x1f53'
composeCombining' CombiningAcuteAccent '\x1f50' = Just '\x1f54'
composeCombining' CombiningAcuteAccent '\x1f51' = Just '\x1f55'
composeCombining' CombiningGreekPerispomeni '\x1f50' = Just '\x1f56'
composeCombining' CombiningGreekPerispomeni '\x1f51' = Just '\x1f57'
composeCombining' CombiningReversedCommaAbove '\x03a5' = Just '\x1f59'
composeCombining' CombiningGraveAccent '\x1f59' = Just '\x1f5b'
composeCombining' CombiningAcuteAccent '\x1f59' = Just '\x1f5d'
composeCombining' CombiningGreekPerispomeni '\x1f59' = Just '\x1f5f'
composeCombining' CombiningCommaAbove '\x03c9' = Just '\x1f60'
composeCombining' CombiningReversedCommaAbove '\x03c9' = Just '\x1f61'
composeCombining' CombiningGraveAccent '\x1f60' = Just '\x1f62'
composeCombining' CombiningGraveAccent '\x1f61' = Just '\x1f63'
composeCombining' CombiningAcuteAccent '\x1f60' = Just '\x1f64'
composeCombining' CombiningAcuteAccent '\x1f61' = Just '\x1f65'
composeCombining' CombiningGreekPerispomeni '\x1f60' = Just '\x1f66'
composeCombining' CombiningGreekPerispomeni '\x1f61' = Just '\x1f67'
composeCombining' CombiningCommaAbove '\x03a9' = Just '\x1f68'
composeCombining' CombiningReversedCommaAbove '\x03a9' = Just '\x1f69'
composeCombining' CombiningGraveAccent '\x1f68' = Just '\x1f6a'
composeCombining' CombiningGraveAccent '\x1f69' = Just '\x1f6b'
composeCombining' CombiningAcuteAccent '\x1f68' = Just '\x1f6c'
composeCombining' CombiningAcuteAccent '\x1f69' = Just '\x1f6d'
composeCombining' CombiningGreekPerispomeni '\x1f68' = Just '\x1f6e'
composeCombining' CombiningGreekPerispomeni '\x1f69' = Just '\x1f6f'
composeCombining' CombiningGraveAccent '\x03b1' = Just '\x1f70'
composeCombining' CombiningGraveAccent '\x03b5' = Just '\x1f72'
composeCombining' CombiningGraveAccent '\x03b7' = Just '\x1f74'
composeCombining' CombiningGraveAccent '\x03b9' = Just '\x1f76'
composeCombining' CombiningGraveAccent '\x03bf' = Just '\x1f78'
composeCombining' CombiningGraveAccent '\x03c5' = Just '\x1f7a'
composeCombining' CombiningGraveAccent '\x03c9' = Just '\x1f7c'
composeCombining' CombiningGreekYpogegrammeni '\x1f00' = Just '\x1f80'
composeCombining' CombiningGreekYpogegrammeni '\x1f01' = Just '\x1f81'
composeCombining' CombiningGreekYpogegrammeni '\x1f02' = Just '\x1f82'
composeCombining' CombiningGreekYpogegrammeni '\x1f03' = Just '\x1f83'
composeCombining' CombiningGreekYpogegrammeni '\x1f04' = Just '\x1f84'
composeCombining' CombiningGreekYpogegrammeni '\x1f05' = Just '\x1f85'
composeCombining' CombiningGreekYpogegrammeni '\x1f06' = Just '\x1f86'
composeCombining' CombiningGreekYpogegrammeni '\x1f07' = Just '\x1f87'
composeCombining' CombiningGreekYpogegrammeni '\x1f08' = Just '\x1f88'
composeCombining' CombiningGreekYpogegrammeni '\x1f09' = Just '\x1f89'
composeCombining' CombiningGreekYpogegrammeni '\x1f0a' = Just '\x1f8a'
composeCombining' CombiningGreekYpogegrammeni '\x1f0b' = Just '\x1f8b'
composeCombining' CombiningGreekYpogegrammeni '\x1f0c' = Just '\x1f8c'
composeCombining' CombiningGreekYpogegrammeni '\x1f0d' = Just '\x1f8d'
composeCombining' CombiningGreekYpogegrammeni '\x1f0e' = Just '\x1f8e'
composeCombining' CombiningGreekYpogegrammeni '\x1f0f' = Just '\x1f8f'
composeCombining' CombiningGreekYpogegrammeni '\x1f20' = Just '\x1f90'
composeCombining' CombiningGreekYpogegrammeni '\x1f21' = Just '\x1f91'
composeCombining' CombiningGreekYpogegrammeni '\x1f22' = Just '\x1f92'
composeCombining' CombiningGreekYpogegrammeni '\x1f23' = Just '\x1f93'
composeCombining' CombiningGreekYpogegrammeni '\x1f24' = Just '\x1f94'
composeCombining' CombiningGreekYpogegrammeni '\x1f25' = Just '\x1f95'
composeCombining' CombiningGreekYpogegrammeni '\x1f26' = Just '\x1f96'
composeCombining' CombiningGreekYpogegrammeni '\x1f27' = Just '\x1f97'
composeCombining' CombiningGreekYpogegrammeni '\x1f28' = Just '\x1f98'
composeCombining' CombiningGreekYpogegrammeni '\x1f29' = Just '\x1f99'
composeCombining' CombiningGreekYpogegrammeni '\x1f2a' = Just '\x1f9a'
composeCombining' CombiningGreekYpogegrammeni '\x1f2b' = Just '\x1f9b'
composeCombining' CombiningGreekYpogegrammeni '\x1f2c' = Just '\x1f9c'
composeCombining' CombiningGreekYpogegrammeni '\x1f2d' = Just '\x1f9d'
composeCombining' CombiningGreekYpogegrammeni '\x1f2e' = Just '\x1f9e'
composeCombining' CombiningGreekYpogegrammeni '\x1f2f' = Just '\x1f9f'
composeCombining' CombiningGreekYpogegrammeni '\x1f60' = Just '\x1fa0'
composeCombining' CombiningGreekYpogegrammeni '\x1f61' = Just '\x1fa1'
composeCombining' CombiningGreekYpogegrammeni '\x1f62' = Just '\x1fa2'
composeCombining' CombiningGreekYpogegrammeni '\x1f63' = Just '\x1fa3'
composeCombining' CombiningGreekYpogegrammeni '\x1f64' = Just '\x1fa4'
composeCombining' CombiningGreekYpogegrammeni '\x1f65' = Just '\x1fa5'
composeCombining' CombiningGreekYpogegrammeni '\x1f66' = Just '\x1fa6'
composeCombining' CombiningGreekYpogegrammeni '\x1f67' = Just '\x1fa7'
composeCombining' CombiningGreekYpogegrammeni '\x1f68' = Just '\x1fa8'
composeCombining' CombiningGreekYpogegrammeni '\x1f69' = Just '\x1fa9'
composeCombining' CombiningGreekYpogegrammeni '\x1f6a' = Just '\x1faa'
composeCombining' CombiningGreekYpogegrammeni '\x1f6b' = Just '\x1fab'
composeCombining' CombiningGreekYpogegrammeni '\x1f6c' = Just '\x1fac'
composeCombining' CombiningGreekYpogegrammeni '\x1f6d' = Just '\x1fad'
composeCombining' CombiningGreekYpogegrammeni '\x1f6e' = Just '\x1fae'
composeCombining' CombiningGreekYpogegrammeni '\x1f6f' = Just '\x1faf'
composeCombining' CombiningBreve '\x03b1' = Just '\x1fb0'
composeCombining' CombiningMacron '\x03b1' = Just '\x1fb1'
composeCombining' CombiningGreekYpogegrammeni '\x1f70' = Just '\x1fb2'
composeCombining' CombiningGreekYpogegrammeni '\x03b1' = Just '\x1fb3'
composeCombining' CombiningGreekYpogegrammeni '\x03ac' = Just '\x1fb4'
composeCombining' CombiningGreekPerispomeni '\x03b1' = Just '\x1fb6'
composeCombining' CombiningGreekYpogegrammeni '\x1fb6' = Just '\x1fb7'
composeCombining' CombiningBreve '\x0391' = Just '\x1fb8'
composeCombining' CombiningMacron '\x0391' = Just '\x1fb9'
composeCombining' CombiningGraveAccent '\x0391' = Just '\x1fba'
composeCombining' CombiningGreekYpogegrammeni '\x0391' = Just '\x1fbc'
composeCombining' CombiningGreekPerispomeni '\x00a8' = Just '\x1fc1'
composeCombining' CombiningGreekYpogegrammeni '\x1f74' = Just '\x1fc2'
composeCombining' CombiningGreekYpogegrammeni '\x03b7' = Just '\x1fc3'
composeCombining' CombiningGreekYpogegrammeni '\x03ae' = Just '\x1fc4'
composeCombining' CombiningGreekPerispomeni '\x03b7' = Just '\x1fc6'
composeCombining' CombiningGreekYpogegrammeni '\x1fc6' = Just '\x1fc7'
composeCombining' CombiningGraveAccent '\x0395' = Just '\x1fc8'
composeCombining' CombiningGraveAccent '\x0397' = Just '\x1fca'
composeCombining' CombiningGreekYpogegrammeni '\x0397' = Just '\x1fcc'
composeCombining' CombiningGraveAccent '\x1fbf' = Just '\x1fcd'
composeCombining' CombiningAcuteAccent '\x1fbf' = Just '\x1fce'
composeCombining' CombiningGreekPerispomeni '\x1fbf' = Just '\x1fcf'
composeCombining' CombiningBreve '\x03b9' = Just '\x1fd0'
composeCombining' CombiningMacron '\x03b9' = Just '\x1fd1'
composeCombining' CombiningGraveAccent '\x03ca' = Just '\x1fd2'
composeCombining' CombiningGreekPerispomeni '\x03b9' = Just '\x1fd6'
composeCombining' CombiningGreekPerispomeni '\x03ca' = Just '\x1fd7'
composeCombining' CombiningBreve '\x0399' = Just '\x1fd8'
composeCombining' CombiningMacron '\x0399' = Just '\x1fd9'
composeCombining' CombiningGraveAccent '\x0399' = Just '\x1fda'
composeCombining' CombiningGraveAccent '\x1ffe' = Just '\x1fdd'
composeCombining' CombiningAcuteAccent '\x1ffe' = Just '\x1fde'
composeCombining' CombiningGreekPerispomeni '\x1ffe' = Just '\x1fdf'
composeCombining' CombiningBreve '\x03c5' = Just '\x1fe0'
composeCombining' CombiningMacron '\x03c5' = Just '\x1fe1'
composeCombining' CombiningGraveAccent '\x03cb' = Just '\x1fe2'
composeCombining' CombiningCommaAbove '\x03c1' = Just '\x1fe4'
composeCombining' CombiningReversedCommaAbove '\x03c1' = Just '\x1fe5'
composeCombining' CombiningGreekPerispomeni '\x03c5' = Just '\x1fe6'
composeCombining' CombiningGreekPerispomeni '\x03cb' = Just '\x1fe7'
composeCombining' CombiningBreve '\x03a5' = Just '\x1fe8'
composeCombining' CombiningMacron '\x03a5' = Just '\x1fe9'
composeCombining' CombiningGraveAccent '\x03a5' = Just '\x1fea'
composeCombining' CombiningReversedCommaAbove '\x03a1' = Just '\x1fec'
composeCombining' CombiningGraveAccent '\x00a8' = Just '\x1fed'
composeCombining' CombiningGreekYpogegrammeni '\x1f7c' = Just '\x1ff2'
composeCombining' CombiningGreekYpogegrammeni '\x03c9' = Just '\x1ff3'
composeCombining' CombiningGreekYpogegrammeni '\x03ce' = Just '\x1ff4'
composeCombining' CombiningGreekPerispomeni '\x03c9' = Just '\x1ff6'
composeCombining' CombiningGreekYpogegrammeni '\x1ff6' = Just '\x1ff7'
composeCombining' CombiningGraveAccent '\x039f' = Just '\x1ff8'
composeCombining' CombiningGraveAccent '\x03a9' = Just '\x1ffa'
composeCombining' CombiningGreekYpogegrammeni '\x03a9' = Just '\x1ffc'
composeCombining' CombiningLongSolidusOverlay '\x2190' = Just '\x219a'
composeCombining' CombiningLongSolidusOverlay '\x2192' = Just '\x219b'
composeCombining' CombiningLongSolidusOverlay '\x2194' = Just '\x21ae'
composeCombining' CombiningLongSolidusOverlay '\x21d0' = Just '\x21cd'
composeCombining' CombiningLongSolidusOverlay '\x21d4' = Just '\x21ce'
composeCombining' CombiningLongSolidusOverlay '\x21d2' = Just '\x21cf'
composeCombining' CombiningLongSolidusOverlay '\x2203' = Just '\x2204'
composeCombining' CombiningLongSolidusOverlay '\x2208' = Just '\x2209'
composeCombining' CombiningLongSolidusOverlay '\x220b' = Just '\x220c'
composeCombining' CombiningLongSolidusOverlay '\x2223' = Just '\x2224'
composeCombining' CombiningLongSolidusOverlay '\x2225' = Just '\x2226'
composeCombining' CombiningLongSolidusOverlay '\x223c' = Just '\x2241'
composeCombining' CombiningLongSolidusOverlay '\x2243' = Just '\x2244'
composeCombining' CombiningLongSolidusOverlay '\x2245' = Just '\x2247'
composeCombining' CombiningLongSolidusOverlay '\x2248' = Just '\x2249'
composeCombining' CombiningLongSolidusOverlay '=' = Just '\x2260'
composeCombining' CombiningLongSolidusOverlay '\x2261' = Just '\x2262'
composeCombining' CombiningLongSolidusOverlay '\x224d' = Just '\x226d'
composeCombining' CombiningLongSolidusOverlay '<' = Just '\x226e'
composeCombining' CombiningLongSolidusOverlay '>' = Just '\x226f'
composeCombining' CombiningLongSolidusOverlay '\x2264' = Just '\x2270'
composeCombining' CombiningLongSolidusOverlay '\x2265' = Just '\x2271'
composeCombining' CombiningLongSolidusOverlay '\x2272' = Just '\x2274'
composeCombining' CombiningLongSolidusOverlay '\x2273' = Just '\x2275'
composeCombining' CombiningLongSolidusOverlay '\x2276' = Just '\x2278'
composeCombining' CombiningLongSolidusOverlay '\x2277' = Just '\x2279'
composeCombining' CombiningLongSolidusOverlay '\x227a' = Just '\x2280'
composeCombining' CombiningLongSolidusOverlay '\x227b' = Just '\x2281'
composeCombining' CombiningLongSolidusOverlay '\x2282' = Just '\x2284'
composeCombining' CombiningLongSolidusOverlay '\x2283' = Just '\x2285'
composeCombining' CombiningLongSolidusOverlay '\x2286' = Just '\x2288'
composeCombining' CombiningLongSolidusOverlay '\x2287' = Just '\x2289'
composeCombining' CombiningLongSolidusOverlay '\x22a2' = Just '\x22ac'
composeCombining' CombiningLongSolidusOverlay '\x22a8' = Just '\x22ad'
composeCombining' CombiningLongSolidusOverlay '\x22a9' = Just '\x22ae'
composeCombining' CombiningLongSolidusOverlay '\x22ab' = Just '\x22af'
composeCombining' CombiningLongSolidusOverlay '\x227c' = Just '\x22e0'
composeCombining' CombiningLongSolidusOverlay '\x227d' = Just '\x22e1'
composeCombining' CombiningLongSolidusOverlay '\x2291' = Just '\x22e2'
composeCombining' CombiningLongSolidusOverlay '\x2292' = Just '\x22e3'
composeCombining' CombiningLongSolidusOverlay '\x22b2' = Just '\x22ea'
composeCombining' CombiningLongSolidusOverlay '\x22b3' = Just '\x22eb'
composeCombining' CombiningLongSolidusOverlay '\x22b4' = Just '\x22ec'
composeCombining' CombiningLongSolidusOverlay '\x22b5' = Just '\x22ed'
composeCombining' CombiningLongSolidusOverlay '\x2add' = Just '\x2adc'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x304b' = Just '\x304c'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x304d' = Just '\x304e'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x304f' = Just '\x3050'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x3051' = Just '\x3052'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x3053' = Just '\x3054'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x3055' = Just '\x3056'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x3057' = Just '\x3058'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x3059' = Just '\x305a'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x305b' = Just '\x305c'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x305d' = Just '\x305e'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x305f' = Just '\x3060'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x3061' = Just '\x3062'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x3064' = Just '\x3065'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x3066' = Just '\x3067'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x3068' = Just '\x3069'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x306f' = Just '\x3070'
composeCombining' CombiningKatakanaHiraganaSemiVoicedSoundMark '\x306f' = Just '\x3071'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x3072' = Just '\x3073'
composeCombining' CombiningKatakanaHiraganaSemiVoicedSoundMark '\x3072' = Just '\x3074'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x3075' = Just '\x3076'
composeCombining' CombiningKatakanaHiraganaSemiVoicedSoundMark '\x3075' = Just '\x3077'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x3078' = Just '\x3079'
composeCombining' CombiningKatakanaHiraganaSemiVoicedSoundMark '\x3078' = Just '\x307a'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x307b' = Just '\x307c'
composeCombining' CombiningKatakanaHiraganaSemiVoicedSoundMark '\x307b' = Just '\x307d'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x3046' = Just '\x3094'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x309d' = Just '\x309e'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30ab' = Just '\x30ac'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30ad' = Just '\x30ae'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30af' = Just '\x30b0'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30b1' = Just '\x30b2'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30b3' = Just '\x30b4'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30b5' = Just '\x30b6'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30b7' = Just '\x30b8'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30b9' = Just '\x30ba'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30bb' = Just '\x30bc'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30bd' = Just '\x30be'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30bf' = Just '\x30c0'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30c1' = Just '\x30c2'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30c4' = Just '\x30c5'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30c6' = Just '\x30c7'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30c8' = Just '\x30c9'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30cf' = Just '\x30d0'
composeCombining' CombiningKatakanaHiraganaSemiVoicedSoundMark '\x30cf' = Just '\x30d1'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30d2' = Just '\x30d3'
composeCombining' CombiningKatakanaHiraganaSemiVoicedSoundMark '\x30d2' = Just '\x30d4'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30d5' = Just '\x30d6'
composeCombining' CombiningKatakanaHiraganaSemiVoicedSoundMark '\x30d5' = Just '\x30d7'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30d8' = Just '\x30d9'
composeCombining' CombiningKatakanaHiraganaSemiVoicedSoundMark '\x30d8' = Just '\x30da'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30db' = Just '\x30dc'
composeCombining' CombiningKatakanaHiraganaSemiVoicedSoundMark '\x30db' = Just '\x30dd'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30a6' = Just '\x30f4'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30ef' = Just '\x30f7'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30f0' = Just '\x30f8'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30f1' = Just '\x30f9'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30f2' = Just '\x30fa'
composeCombining' CombiningKatakanaHiraganaVoicedSoundMark '\x30fd' = Just '\x30fe'
composeCombining' HebrewPointHiriq '\x05d9' = Just '\xfb1d'
composeCombining' HebrewPointPatah '\x05f2' = Just '\xfb1f'
composeCombining' HebrewPointShinDot '\x05e9' = Just '\xfb2a'
composeCombining' HebrewPointSinDot '\x05e9' = Just '\xfb2b'
composeCombining' HebrewPointShinDot '\xfb49' = Just '\xfb2c'
composeCombining' HebrewPointSinDot '\xfb49' = Just '\xfb2d'
composeCombining' HebrewPointPatah '\x05d0' = Just '\xfb2e'
composeCombining' HebrewPointQamats '\x05d0' = Just '\xfb2f'
composeCombining' HebrewPointDageshOrMapiq '\x05d0' = Just '\xfb30'
composeCombining' HebrewPointDageshOrMapiq '\x05d1' = Just '\xfb31'
composeCombining' HebrewPointDageshOrMapiq '\x05d2' = Just '\xfb32'
composeCombining' HebrewPointDageshOrMapiq '\x05d3' = Just '\xfb33'
composeCombining' HebrewPointDageshOrMapiq '\x05d4' = Just '\xfb34'
composeCombining' HebrewPointDageshOrMapiq '\x05d5' = Just '\xfb35'
composeCombining' HebrewPointDageshOrMapiq '\x05d6' = Just '\xfb36'
composeCombining' HebrewPointDageshOrMapiq '\x05d8' = Just '\xfb38'
composeCombining' HebrewPointDageshOrMapiq '\x05d9' = Just '\xfb39'
composeCombining' HebrewPointDageshOrMapiq '\x05da' = Just '\xfb3a'
composeCombining' HebrewPointDageshOrMapiq '\x05db' = Just '\xfb3b'
composeCombining' HebrewPointDageshOrMapiq '\x05dc' = Just '\xfb3c'
composeCombining' HebrewPointDageshOrMapiq '\x05de' = Just '\xfb3e'
composeCombining' HebrewPointDageshOrMapiq '\x05e0' = Just '\xfb40'
composeCombining' HebrewPointDageshOrMapiq '\x05e1' = Just '\xfb41'
composeCombining' HebrewPointDageshOrMapiq '\x05e3' = Just '\xfb43'
composeCombining' HebrewPointDageshOrMapiq '\x05e4' = Just '\xfb44'
composeCombining' HebrewPointDageshOrMapiq '\x05e6' = Just '\xfb46'
composeCombining' HebrewPointDageshOrMapiq '\x05e7' = Just '\xfb47'
composeCombining' HebrewPointDageshOrMapiq '\x05e8' = Just '\xfb48'
composeCombining' HebrewPointDageshOrMapiq '\x05e9' = Just '\xfb49'
composeCombining' HebrewPointDageshOrMapiq '\x05ea' = Just '\xfb4a'
composeCombining' HebrewPointHolam '\x05d5' = Just '\xfb4b'
composeCombining' HebrewPointRafe '\x05d1' = Just '\xfb4c'
composeCombining' HebrewPointRafe '\x05db' = Just '\xfb4d'
composeCombining' HebrewPointRafe '\x05e4' = Just '\xfb4e'
composeCombining' KaithiSignNukta '\x11099' = Just '\x1109a'
composeCombining' KaithiSignNukta '\x1109b' = Just '\x1109c'
composeCombining' KaithiSignNukta '\x110a5' = Just '\x110ab'
composeCombining' ChakmaVowelSignA '\x11131' = Just '\x1112e'
composeCombining' ChakmaVowelSignA '\x11132' = Just '\x1112f'
composeCombining' GranthaVowelSignAa '\x11347' = Just '\x1134b'
composeCombining' GranthaAuLengthMark '\x11347' = Just '\x1134c'
composeCombining' TirhutaVowelSignShortE '\x114b9' = Just '\x114bb'
composeCombining' TirhutaVowelSignAa '\x114b9' = Just '\x114bc'
composeCombining' TirhutaVowelSignShortO '\x114b9' = Just '\x114be'
composeCombining' SiddhamVowelSignAa '\x115b8' = Just '\x115ba'
composeCombining' SiddhamVowelSignAa '\x115b9' = Just '\x115bb'
composeCombining' MusicalSymbolCombiningStem '\x1d157' = Just '\x1d15e'
composeCombining' MusicalSymbolCombiningStem '\x1d158' = Just '\x1d15f'
composeCombining' MusicalSymbolCombiningFlag1 '\x1d15f' = Just '\x1d160'
composeCombining' MusicalSymbolCombiningFlag2 '\x1d15f' = Just '\x1d161'
composeCombining' MusicalSymbolCombiningFlag3 '\x1d15f' = Just '\x1d162'
composeCombining' MusicalSymbolCombiningFlag4 '\x1d15f' = Just '\x1d163'
composeCombining' MusicalSymbolCombiningFlag5 '\x1d15f' = Just '\x1d164'
composeCombining' MusicalSymbolCombiningStem '\x1d1b9' = Just '\x1d1bb'
composeCombining' MusicalSymbolCombiningStem '\x1d1ba' = Just '\x1d1bc'
composeCombining' MusicalSymbolCombiningFlag1 '\x1d1bb' = Just '\x1d1bd'
composeCombining' MusicalSymbolCombiningFlag1 '\x1d1bc' = Just '\x1d1be'
composeCombining' MusicalSymbolCombiningFlag2 '\x1d1bb' = Just '\x1d1bf'
composeCombining' MusicalSymbolCombiningFlag2 '\x1d1bc' = Just '\x1d1c0'
composeCombining' _ _ = Nothing

-- | A pattern synonym for 'CombiningGraveAccent', the name without the @Combining@ part, defined by @'\\x0300'@ (&#x2022;&#x0300;).
pattern GraveAccent :: CombiningCharacter
pattern GraveAccent = CombiningGraveAccent

-- | A pattern synonym for 'CombiningAcuteAccent', the name without the @Combining@ part, defined by @'\\x0301'@ (&#x2022;&#x0301;).
pattern AcuteAccent :: CombiningCharacter
pattern AcuteAccent = CombiningAcuteAccent

-- | A pattern synonym for 'CombiningCircumflexAccent', the name without the @Combining@ part, defined by @'\\x0302'@ (&#x2022;&#x0302;).
pattern CircumflexAccent :: CombiningCharacter
pattern CircumflexAccent = CombiningCircumflexAccent

-- | A pattern synonym for 'CombiningTilde', the name without the @Combining@ part, defined by @'\\x0303'@ (&#x2022;&#x0303;).
pattern Tilde :: CombiningCharacter
pattern Tilde = CombiningTilde

-- | A pattern synonym for 'CombiningMacron', the name without the @Combining@ part, defined by @'\\x0304'@ (&#x2022;&#x0304;).
pattern Macron :: CombiningCharacter
pattern Macron = CombiningMacron

-- | A pattern synonym for 'CombiningOverline', the name without the @Combining@ part, defined by @'\\x0305'@ (&#x2022;&#x0305;).
pattern Overline :: CombiningCharacter
pattern Overline = CombiningOverline

-- | A pattern synonym for 'CombiningBreve', the name without the @Combining@ part, defined by @'\\x0306'@ (&#x2022;&#x0306;).
pattern Breve :: CombiningCharacter
pattern Breve = CombiningBreve

-- | A pattern synonym for 'CombiningDotAbove', the name without the @Combining@ part, defined by @'\\x0307'@ (&#x2022;&#x0307;).
pattern DotAbove :: CombiningCharacter
pattern DotAbove = CombiningDotAbove

-- | A pattern synonym for 'CombiningDiaeresis', the name without the @Combining@ part, defined by @'\\x0308'@ (&#x2022;&#x0308;).
pattern Diaeresis :: CombiningCharacter
pattern Diaeresis = CombiningDiaeresis

-- | A pattern synonym for 'CombiningHookAbove', the name without the @Combining@ part, defined by @'\\x0309'@ (&#x2022;&#x0309;).
pattern HookAbove :: CombiningCharacter
pattern HookAbove = CombiningHookAbove

-- | A pattern synonym for 'CombiningRingAbove', the name without the @Combining@ part, defined by @'\\x030a'@ (&#x2022;&#x030a;).
pattern RingAbove :: CombiningCharacter
pattern RingAbove = CombiningRingAbove

-- | A pattern synonym for 'CombiningDoubleAcuteAccent', the name without the @Combining@ part, defined by @'\\x030b'@ (&#x2022;&#x030b;).
pattern DoubleAcuteAccent :: CombiningCharacter
pattern DoubleAcuteAccent = CombiningDoubleAcuteAccent

-- | A pattern synonym for 'CombiningCaron', the name without the @Combining@ part, defined by @'\\x030c'@ (&#x2022;&#x030c;).
pattern Caron :: CombiningCharacter
pattern Caron = CombiningCaron

-- | A pattern synonym for 'CombiningVerticalLineAbove', the name without the @Combining@ part, defined by @'\\x030d'@ (&#x2022;&#x030d;).
pattern VerticalLineAbove :: CombiningCharacter
pattern VerticalLineAbove = CombiningVerticalLineAbove

-- | A pattern synonym for 'CombiningDoubleVerticalLineAbove', the name without the @Combining@ part, defined by @'\\x030e'@ (&#x2022;&#x030e;).
pattern DoubleVerticalLineAbove :: CombiningCharacter
pattern DoubleVerticalLineAbove = CombiningDoubleVerticalLineAbove

-- | A pattern synonym for 'CombiningDoubleGraveAccent', the name without the @Combining@ part, defined by @'\\x030f'@ (&#x2022;&#x030f;).
pattern DoubleGraveAccent :: CombiningCharacter
pattern DoubleGraveAccent = CombiningDoubleGraveAccent

-- | A pattern synonym for 'CombiningCandrabindu', the name without the @Combining@ part, defined by @'\\x0310'@ (&#x2022;&#x0310;).
pattern Candrabindu :: CombiningCharacter
pattern Candrabindu = CombiningCandrabindu

-- | A pattern synonym for 'CombiningInvertedBreve', the name without the @Combining@ part, defined by @'\\x0311'@ (&#x2022;&#x0311;).
pattern InvertedBreve :: CombiningCharacter
pattern InvertedBreve = CombiningInvertedBreve

-- | A pattern synonym for 'CombiningTurnedCommaAbove', the name without the @Combining@ part, defined by @'\\x0312'@ (&#x2022;&#x0312;).
pattern TurnedCommaAbove :: CombiningCharacter
pattern TurnedCommaAbove = CombiningTurnedCommaAbove

-- | A pattern synonym for 'CombiningCommaAbove', the name without the @Combining@ part, defined by @'\\x0313'@ (&#x2022;&#x0313;).
pattern CommaAbove :: CombiningCharacter
pattern CommaAbove = CombiningCommaAbove

-- | A pattern synonym for 'CombiningReversedCommaAbove', the name without the @Combining@ part, defined by @'\\x0314'@ (&#x2022;&#x0314;).
pattern ReversedCommaAbove :: CombiningCharacter
pattern ReversedCommaAbove = CombiningReversedCommaAbove

-- | A pattern synonym for 'CombiningCommaAboveRight', the name without the @Combining@ part, defined by @'\\x0315'@ (&#x2022;&#x0315;).
pattern CommaAboveRight :: CombiningCharacter
pattern CommaAboveRight = CombiningCommaAboveRight

-- | A pattern synonym for 'CombiningGraveAccentBelow', the name without the @Combining@ part, defined by @'\\x0316'@ (&#x2022;&#x0316;).
pattern GraveAccentBelow :: CombiningCharacter
pattern GraveAccentBelow = CombiningGraveAccentBelow

-- | A pattern synonym for 'CombiningAcuteAccentBelow', the name without the @Combining@ part, defined by @'\\x0317'@ (&#x2022;&#x0317;).
pattern AcuteAccentBelow :: CombiningCharacter
pattern AcuteAccentBelow = CombiningAcuteAccentBelow

-- | A pattern synonym for 'CombiningLeftTackBelow', the name without the @Combining@ part, defined by @'\\x0318'@ (&#x2022;&#x0318;).
pattern LeftTackBelow :: CombiningCharacter
pattern LeftTackBelow = CombiningLeftTackBelow

-- | A pattern synonym for 'CombiningRightTackBelow', the name without the @Combining@ part, defined by @'\\x0319'@ (&#x2022;&#x0319;).
pattern RightTackBelow :: CombiningCharacter
pattern RightTackBelow = CombiningRightTackBelow

-- | A pattern synonym for 'CombiningLeftAngleAbove', the name without the @Combining@ part, defined by @'\\x031a'@ (&#x2022;&#x031a;).
pattern LeftAngleAbove :: CombiningCharacter
pattern LeftAngleAbove = CombiningLeftAngleAbove

-- | A pattern synonym for 'CombiningHorn', the name without the @Combining@ part, defined by @'\\x031b'@ (&#x2022;&#x031b;).
pattern Horn :: CombiningCharacter
pattern Horn = CombiningHorn

-- | A pattern synonym for 'CombiningLeftHalfRingBelow', the name without the @Combining@ part, defined by @'\\x031c'@ (&#x2022;&#x031c;).
pattern LeftHalfRingBelow :: CombiningCharacter
pattern LeftHalfRingBelow = CombiningLeftHalfRingBelow

-- | A pattern synonym for 'CombiningUpTackBelow', the name without the @Combining@ part, defined by @'\\x031d'@ (&#x2022;&#x031d;).
pattern UpTackBelow :: CombiningCharacter
pattern UpTackBelow = CombiningUpTackBelow

-- | A pattern synonym for 'CombiningDownTackBelow', the name without the @Combining@ part, defined by @'\\x031e'@ (&#x2022;&#x031e;).
pattern DownTackBelow :: CombiningCharacter
pattern DownTackBelow = CombiningDownTackBelow

-- | A pattern synonym for 'CombiningPlusSignBelow', the name without the @Combining@ part, defined by @'\\x031f'@ (&#x2022;&#x031f;).
pattern PlusSignBelow :: CombiningCharacter
pattern PlusSignBelow = CombiningPlusSignBelow

-- | A pattern synonym for 'CombiningMinusSignBelow', the name without the @Combining@ part, defined by @'\\x0320'@ (&#x2022;&#x0320;).
pattern MinusSignBelow :: CombiningCharacter
pattern MinusSignBelow = CombiningMinusSignBelow

-- | A pattern synonym for 'CombiningPalatalizedHookBelow', the name without the @Combining@ part, defined by @'\\x0321'@ (&#x2022;&#x0321;).
pattern PalatalizedHookBelow :: CombiningCharacter
pattern PalatalizedHookBelow = CombiningPalatalizedHookBelow

-- | A pattern synonym for 'CombiningRetroflexHookBelow', the name without the @Combining@ part, defined by @'\\x0322'@ (&#x2022;&#x0322;).
pattern RetroflexHookBelow :: CombiningCharacter
pattern RetroflexHookBelow = CombiningRetroflexHookBelow

-- | A pattern synonym for 'CombiningDotBelow', the name without the @Combining@ part, defined by @'\\x0323'@ (&#x2022;&#x0323;).
pattern DotBelow :: CombiningCharacter
pattern DotBelow = CombiningDotBelow

-- | A pattern synonym for 'CombiningDiaeresisBelow', the name without the @Combining@ part, defined by @'\\x0324'@ (&#x2022;&#x0324;).
pattern DiaeresisBelow :: CombiningCharacter
pattern DiaeresisBelow = CombiningDiaeresisBelow

-- | A pattern synonym for 'CombiningRingBelow', the name without the @Combining@ part, defined by @'\\x0325'@ (&#x2022;&#x0325;).
pattern RingBelow :: CombiningCharacter
pattern RingBelow = CombiningRingBelow

-- | A pattern synonym for 'CombiningCommaBelow', the name without the @Combining@ part, defined by @'\\x0326'@ (&#x2022;&#x0326;).
pattern CommaBelow :: CombiningCharacter
pattern CommaBelow = CombiningCommaBelow

-- | A pattern synonym for 'CombiningCedilla', the name without the @Combining@ part, defined by @'\\x0327'@ (&#x2022;&#x0327;).
pattern Cedilla :: CombiningCharacter
pattern Cedilla = CombiningCedilla

-- | A pattern synonym for 'CombiningOgonek', the name without the @Combining@ part, defined by @'\\x0328'@ (&#x2022;&#x0328;).
pattern Ogonek :: CombiningCharacter
pattern Ogonek = CombiningOgonek

-- | A pattern synonym for 'CombiningVerticalLineBelow', the name without the @Combining@ part, defined by @'\\x0329'@ (&#x2022;&#x0329;).
pattern VerticalLineBelow :: CombiningCharacter
pattern VerticalLineBelow = CombiningVerticalLineBelow

-- | A pattern synonym for 'CombiningBridgeBelow', the name without the @Combining@ part, defined by @'\\x032a'@ (&#x2022;&#x032a;).
pattern BridgeBelow :: CombiningCharacter
pattern BridgeBelow = CombiningBridgeBelow

-- | A pattern synonym for 'CombiningInvertedDoubleArchBelow', the name without the @Combining@ part, defined by @'\\x032b'@ (&#x2022;&#x032b;).
pattern InvertedDoubleArchBelow :: CombiningCharacter
pattern InvertedDoubleArchBelow = CombiningInvertedDoubleArchBelow

-- | A pattern synonym for 'CombiningCaronBelow', the name without the @Combining@ part, defined by @'\\x032c'@ (&#x2022;&#x032c;).
pattern CaronBelow :: CombiningCharacter
pattern CaronBelow = CombiningCaronBelow

-- | A pattern synonym for 'CombiningCircumflexAccentBelow', the name without the @Combining@ part, defined by @'\\x032d'@ (&#x2022;&#x032d;).
pattern CircumflexAccentBelow :: CombiningCharacter
pattern CircumflexAccentBelow = CombiningCircumflexAccentBelow

-- | A pattern synonym for 'CombiningBreveBelow', the name without the @Combining@ part, defined by @'\\x032e'@ (&#x2022;&#x032e;).
pattern BreveBelow :: CombiningCharacter
pattern BreveBelow = CombiningBreveBelow

-- | A pattern synonym for 'CombiningInvertedBreveBelow', the name without the @Combining@ part, defined by @'\\x032f'@ (&#x2022;&#x032f;).
pattern InvertedBreveBelow :: CombiningCharacter
pattern InvertedBreveBelow = CombiningInvertedBreveBelow

-- | A pattern synonym for 'CombiningTildeBelow', the name without the @Combining@ part, defined by @'\\x0330'@ (&#x2022;&#x0330;).
pattern TildeBelow :: CombiningCharacter
pattern TildeBelow = CombiningTildeBelow

-- | A pattern synonym for 'CombiningMacronBelow', the name without the @Combining@ part, defined by @'\\x0331'@ (&#x2022;&#x0331;).
pattern MacronBelow :: CombiningCharacter
pattern MacronBelow = CombiningMacronBelow

-- | A pattern synonym for 'CombiningLowLine', the name without the @Combining@ part, defined by @'\\x0332'@ (&#x2022;&#x0332;).
pattern LowLine :: CombiningCharacter
pattern LowLine = CombiningLowLine

-- | A pattern synonym for 'CombiningDoubleLowLine', the name without the @Combining@ part, defined by @'\\x0333'@ (&#x2022;&#x0333;).
pattern DoubleLowLine :: CombiningCharacter
pattern DoubleLowLine = CombiningDoubleLowLine

-- | A pattern synonym for 'CombiningTildeOverlay', the name without the @Combining@ part, defined by @'\\x0334'@ (&#x2022;&#x0334;).
pattern TildeOverlay :: CombiningCharacter
pattern TildeOverlay = CombiningTildeOverlay

-- | A pattern synonym for 'CombiningShortStrokeOverlay', the name without the @Combining@ part, defined by @'\\x0335'@ (&#x2022;&#x0335;).
pattern ShortStrokeOverlay :: CombiningCharacter
pattern ShortStrokeOverlay = CombiningShortStrokeOverlay

-- | A pattern synonym for 'CombiningLongStrokeOverlay', the name without the @Combining@ part, defined by @'\\x0336'@ (&#x2022;&#x0336;).
pattern LongStrokeOverlay :: CombiningCharacter
pattern LongStrokeOverlay = CombiningLongStrokeOverlay

-- | A pattern synonym for 'CombiningShortSolidusOverlay', the name without the @Combining@ part, defined by @'\\x0337'@ (&#x2022;&#x0337;).
pattern ShortSolidusOverlay :: CombiningCharacter
pattern ShortSolidusOverlay = CombiningShortSolidusOverlay

-- | A pattern synonym for 'CombiningLongSolidusOverlay', the name without the @Combining@ part, defined by @'\\x0338'@ (&#x2022;&#x0338;).
pattern LongSolidusOverlay :: CombiningCharacter
pattern LongSolidusOverlay = CombiningLongSolidusOverlay

-- | A pattern synonym for 'CombiningRightHalfRingBelow', the name without the @Combining@ part, defined by @'\\x0339'@ (&#x2022;&#x0339;).
pattern RightHalfRingBelow :: CombiningCharacter
pattern RightHalfRingBelow = CombiningRightHalfRingBelow

-- | A pattern synonym for 'CombiningInvertedBridgeBelow', the name without the @Combining@ part, defined by @'\\x033a'@ (&#x2022;&#x033a;).
pattern InvertedBridgeBelow :: CombiningCharacter
pattern InvertedBridgeBelow = CombiningInvertedBridgeBelow

-- | A pattern synonym for 'CombiningSquareBelow', the name without the @Combining@ part, defined by @'\\x033b'@ (&#x2022;&#x033b;).
pattern SquareBelow :: CombiningCharacter
pattern SquareBelow = CombiningSquareBelow

-- | A pattern synonym for 'CombiningSeagullBelow', the name without the @Combining@ part, defined by @'\\x033c'@ (&#x2022;&#x033c;).
pattern SeagullBelow :: CombiningCharacter
pattern SeagullBelow = CombiningSeagullBelow

-- | A pattern synonym for 'CombiningXAbove', the name without the @Combining@ part, defined by @'\\x033d'@ (&#x2022;&#x033d;).
pattern XAbove :: CombiningCharacter
pattern XAbove = CombiningXAbove

-- | A pattern synonym for 'CombiningVerticalTilde', the name without the @Combining@ part, defined by @'\\x033e'@ (&#x2022;&#x033e;).
pattern VerticalTilde :: CombiningCharacter
pattern VerticalTilde = CombiningVerticalTilde

-- | A pattern synonym for 'CombiningDoubleOverline', the name without the @Combining@ part, defined by @'\\x033f'@ (&#x2022;&#x033f;).
pattern DoubleOverline :: CombiningCharacter
pattern DoubleOverline = CombiningDoubleOverline

-- | A pattern synonym for 'CombiningGraveToneMark', the name without the @Combining@ part, defined by @'\\x0340'@ (&#x2022;&#x0340;).
pattern GraveToneMark :: CombiningCharacter
pattern GraveToneMark = CombiningGraveToneMark

-- | A pattern synonym for 'CombiningAcuteToneMark', the name without the @Combining@ part, defined by @'\\x0341'@ (&#x2022;&#x0341;).
pattern AcuteToneMark :: CombiningCharacter
pattern AcuteToneMark = CombiningAcuteToneMark

-- | A pattern synonym for 'CombiningGreekPerispomeni', the name without the @Combining@ part, defined by @'\\x0342'@ (&#x2022;&#x0342;).
pattern GreekPerispomeni :: CombiningCharacter
pattern GreekPerispomeni = CombiningGreekPerispomeni

-- | A pattern synonym for 'CombiningGreekKoronis', the name without the @Combining@ part, defined by @'\\x0343'@ (&#x2022;&#x0343;).
pattern GreekKoronis :: CombiningCharacter
pattern GreekKoronis = CombiningGreekKoronis

-- | A pattern synonym for 'CombiningGreekDialytikaTonos', the name without the @Combining@ part, defined by @'\\x0344'@ (&#x2022;&#x0344;).
pattern GreekDialytikaTonos :: CombiningCharacter
pattern GreekDialytikaTonos = CombiningGreekDialytikaTonos

-- | A pattern synonym for 'CombiningGreekYpogegrammeni', the name without the @Combining@ part, defined by @'\\x0345'@ (&#x2022;&#x0345;).
pattern GreekYpogegrammeni :: CombiningCharacter
pattern GreekYpogegrammeni = CombiningGreekYpogegrammeni

-- | A pattern synonym for 'CombiningBridgeAbove', the name without the @Combining@ part, defined by @'\\x0346'@ (&#x2022;&#x0346;).
pattern BridgeAbove :: CombiningCharacter
pattern BridgeAbove = CombiningBridgeAbove

-- | A pattern synonym for 'CombiningEqualsSignBelow', the name without the @Combining@ part, defined by @'\\x0347'@ (&#x2022;&#x0347;).
pattern EqualsSignBelow :: CombiningCharacter
pattern EqualsSignBelow = CombiningEqualsSignBelow

-- | A pattern synonym for 'CombiningDoubleVerticalLineBelow', the name without the @Combining@ part, defined by @'\\x0348'@ (&#x2022;&#x0348;).
pattern DoubleVerticalLineBelow :: CombiningCharacter
pattern DoubleVerticalLineBelow = CombiningDoubleVerticalLineBelow

-- | A pattern synonym for 'CombiningLeftAngleBelow', the name without the @Combining@ part, defined by @'\\x0349'@ (&#x2022;&#x0349;).
pattern LeftAngleBelow :: CombiningCharacter
pattern LeftAngleBelow = CombiningLeftAngleBelow

-- | A pattern synonym for 'CombiningNotTildeAbove', the name without the @Combining@ part, defined by @'\\x034a'@ (&#x2022;&#x034a;).
pattern NotTildeAbove :: CombiningCharacter
pattern NotTildeAbove = CombiningNotTildeAbove

-- | A pattern synonym for 'CombiningHomotheticAbove', the name without the @Combining@ part, defined by @'\\x034b'@ (&#x2022;&#x034b;).
pattern HomotheticAbove :: CombiningCharacter
pattern HomotheticAbove = CombiningHomotheticAbove

-- | A pattern synonym for 'CombiningAlmostEqualToAbove', the name without the @Combining@ part, defined by @'\\x034c'@ (&#x2022;&#x034c;).
pattern AlmostEqualToAbove :: CombiningCharacter
pattern AlmostEqualToAbove = CombiningAlmostEqualToAbove

-- | A pattern synonym for 'CombiningLeftRightArrowBelow', the name without the @Combining@ part, defined by @'\\x034d'@ (&#x2022;&#x034d;).
pattern LeftRightArrowBelow :: CombiningCharacter
pattern LeftRightArrowBelow = CombiningLeftRightArrowBelow

-- | A pattern synonym for 'CombiningUpwardsArrowBelow', the name without the @Combining@ part, defined by @'\\x034e'@ (&#x2022;&#x034e;).
pattern UpwardsArrowBelow :: CombiningCharacter
pattern UpwardsArrowBelow = CombiningUpwardsArrowBelow

-- | A pattern synonym for 'CombiningRightArrowheadAbove', the name without the @Combining@ part, defined by @'\\x0350'@ (&#x2022;&#x0350;).
pattern RightArrowheadAbove :: CombiningCharacter
pattern RightArrowheadAbove = CombiningRightArrowheadAbove

-- | A pattern synonym for 'CombiningLeftHalfRingAbove', the name without the @Combining@ part, defined by @'\\x0351'@ (&#x2022;&#x0351;).
pattern LeftHalfRingAbove :: CombiningCharacter
pattern LeftHalfRingAbove = CombiningLeftHalfRingAbove

-- | A pattern synonym for 'CombiningFermata', the name without the @Combining@ part, defined by @'\\x0352'@ (&#x2022;&#x0352;).
pattern Fermata :: CombiningCharacter
pattern Fermata = CombiningFermata

-- | A pattern synonym for 'CombiningXBelow', the name without the @Combining@ part, defined by @'\\x0353'@ (&#x2022;&#x0353;).
pattern XBelow :: CombiningCharacter
pattern XBelow = CombiningXBelow

-- | A pattern synonym for 'CombiningLeftArrowheadBelow', the name without the @Combining@ part, defined by @'\\x0354'@ (&#x2022;&#x0354;).
pattern LeftArrowheadBelow :: CombiningCharacter
pattern LeftArrowheadBelow = CombiningLeftArrowheadBelow

-- | A pattern synonym for 'CombiningRightArrowheadBelow', the name without the @Combining@ part, defined by @'\\x0355'@ (&#x2022;&#x0355;).
pattern RightArrowheadBelow :: CombiningCharacter
pattern RightArrowheadBelow = CombiningRightArrowheadBelow

-- | A pattern synonym for 'CombiningRightArrowheadAndUpArrowheadBelow', the name without the @Combining@ part, defined by @'\\x0356'@ (&#x2022;&#x0356;).
pattern RightArrowheadAndUpArrowheadBelow :: CombiningCharacter
pattern RightArrowheadAndUpArrowheadBelow = CombiningRightArrowheadAndUpArrowheadBelow

-- | A pattern synonym for 'CombiningRightHalfRingAbove', the name without the @Combining@ part, defined by @'\\x0357'@ (&#x2022;&#x0357;).
pattern RightHalfRingAbove :: CombiningCharacter
pattern RightHalfRingAbove = CombiningRightHalfRingAbove

-- | A pattern synonym for 'CombiningDotAboveRight', the name without the @Combining@ part, defined by @'\\x0358'@ (&#x2022;&#x0358;).
pattern DotAboveRight :: CombiningCharacter
pattern DotAboveRight = CombiningDotAboveRight

-- | A pattern synonym for 'CombiningAsteriskBelow', the name without the @Combining@ part, defined by @'\\x0359'@ (&#x2022;&#x0359;).
pattern AsteriskBelow :: CombiningCharacter
pattern AsteriskBelow = CombiningAsteriskBelow

-- | A pattern synonym for 'CombiningDoubleRingBelow', the name without the @Combining@ part, defined by @'\\x035a'@ (&#x2022;&#x035a;).
pattern DoubleRingBelow :: CombiningCharacter
pattern DoubleRingBelow = CombiningDoubleRingBelow

-- | A pattern synonym for 'CombiningZigzagAbove', the name without the @Combining@ part, defined by @'\\x035b'@ (&#x2022;&#x035b;).
pattern ZigzagAbove :: CombiningCharacter
pattern ZigzagAbove = CombiningZigzagAbove

-- | A pattern synonym for 'CombiningDoubleBreveBelow', the name without the @Combining@ part, defined by @'\\x035c'@ (&#x2022;&#x035c;).
pattern DoubleBreveBelow :: CombiningCharacter
pattern DoubleBreveBelow = CombiningDoubleBreveBelow

-- | A pattern synonym for 'CombiningDoubleBreve', the name without the @Combining@ part, defined by @'\\x035d'@ (&#x2022;&#x035d;).
pattern DoubleBreve :: CombiningCharacter
pattern DoubleBreve = CombiningDoubleBreve

-- | A pattern synonym for 'CombiningDoubleMacron', the name without the @Combining@ part, defined by @'\\x035e'@ (&#x2022;&#x035e;).
pattern DoubleMacron :: CombiningCharacter
pattern DoubleMacron = CombiningDoubleMacron

-- | A pattern synonym for 'CombiningDoubleMacronBelow', the name without the @Combining@ part, defined by @'\\x035f'@ (&#x2022;&#x035f;).
pattern DoubleMacronBelow :: CombiningCharacter
pattern DoubleMacronBelow = CombiningDoubleMacronBelow

-- | A pattern synonym for 'CombiningDoubleTilde', the name without the @Combining@ part, defined by @'\\x0360'@ (&#x2022;&#x0360;).
pattern DoubleTilde :: CombiningCharacter
pattern DoubleTilde = CombiningDoubleTilde

-- | A pattern synonym for 'CombiningDoubleInvertedBreve', the name without the @Combining@ part, defined by @'\\x0361'@ (&#x2022;&#x0361;).
pattern DoubleInvertedBreve :: CombiningCharacter
pattern DoubleInvertedBreve = CombiningDoubleInvertedBreve

-- | A pattern synonym for 'CombiningDoubleRightwardsArrowBelow', the name without the @Combining@ part, defined by @'\\x0362'@ (&#x2022;&#x0362;).
pattern DoubleRightwardsArrowBelow :: CombiningCharacter
pattern DoubleRightwardsArrowBelow = CombiningDoubleRightwardsArrowBelow

-- | A pattern synonym for 'CombiningLatinSmallLetterA', the name without the @Combining@ part, defined by @'\\x0363'@ (&#x2022;&#x0363;).
pattern LatinSmallLetterA :: CombiningCharacter
pattern LatinSmallLetterA = CombiningLatinSmallLetterA

-- | A pattern synonym for 'CombiningLatinSmallLetterE', the name without the @Combining@ part, defined by @'\\x0364'@ (&#x2022;&#x0364;).
pattern LatinSmallLetterE :: CombiningCharacter
pattern LatinSmallLetterE = CombiningLatinSmallLetterE

-- | A pattern synonym for 'CombiningLatinSmallLetterI', the name without the @Combining@ part, defined by @'\\x0365'@ (&#x2022;&#x0365;).
pattern LatinSmallLetterI :: CombiningCharacter
pattern LatinSmallLetterI = CombiningLatinSmallLetterI

-- | A pattern synonym for 'CombiningLatinSmallLetterO', the name without the @Combining@ part, defined by @'\\x0366'@ (&#x2022;&#x0366;).
pattern LatinSmallLetterO :: CombiningCharacter
pattern LatinSmallLetterO = CombiningLatinSmallLetterO

-- | A pattern synonym for 'CombiningLatinSmallLetterU', the name without the @Combining@ part, defined by @'\\x0367'@ (&#x2022;&#x0367;).
pattern LatinSmallLetterU :: CombiningCharacter
pattern LatinSmallLetterU = CombiningLatinSmallLetterU

-- | A pattern synonym for 'CombiningLatinSmallLetterC', the name without the @Combining@ part, defined by @'\\x0368'@ (&#x2022;&#x0368;).
pattern LatinSmallLetterC :: CombiningCharacter
pattern LatinSmallLetterC = CombiningLatinSmallLetterC

-- | A pattern synonym for 'CombiningLatinSmallLetterD', the name without the @Combining@ part, defined by @'\\x0369'@ (&#x2022;&#x0369;).
pattern LatinSmallLetterD :: CombiningCharacter
pattern LatinSmallLetterD = CombiningLatinSmallLetterD

-- | A pattern synonym for 'CombiningLatinSmallLetterH', the name without the @Combining@ part, defined by @'\\x036a'@ (&#x2022;&#x036a;).
pattern LatinSmallLetterH :: CombiningCharacter
pattern LatinSmallLetterH = CombiningLatinSmallLetterH

-- | A pattern synonym for 'CombiningLatinSmallLetterM', the name without the @Combining@ part, defined by @'\\x036b'@ (&#x2022;&#x036b;).
pattern LatinSmallLetterM :: CombiningCharacter
pattern LatinSmallLetterM = CombiningLatinSmallLetterM

-- | A pattern synonym for 'CombiningLatinSmallLetterR', the name without the @Combining@ part, defined by @'\\x036c'@ (&#x2022;&#x036c;).
pattern LatinSmallLetterR :: CombiningCharacter
pattern LatinSmallLetterR = CombiningLatinSmallLetterR

-- | A pattern synonym for 'CombiningLatinSmallLetterT', the name without the @Combining@ part, defined by @'\\x036d'@ (&#x2022;&#x036d;).
pattern LatinSmallLetterT :: CombiningCharacter
pattern LatinSmallLetterT = CombiningLatinSmallLetterT

-- | A pattern synonym for 'CombiningLatinSmallLetterV', the name without the @Combining@ part, defined by @'\\x036e'@ (&#x2022;&#x036e;).
pattern LatinSmallLetterV :: CombiningCharacter
pattern LatinSmallLetterV = CombiningLatinSmallLetterV

-- | A pattern synonym for 'CombiningLatinSmallLetterX', the name without the @Combining@ part, defined by @'\\x036f'@ (&#x2022;&#x036f;).
pattern LatinSmallLetterX :: CombiningCharacter
pattern LatinSmallLetterX = CombiningLatinSmallLetterX

-- | A pattern synonym for 'CombiningCyrillicTitlo', the name without the @Combining@ part, defined by @'\\x0483'@ (&#x2022;&#x0483;).
pattern CyrillicTitlo :: CombiningCharacter
pattern CyrillicTitlo = CombiningCyrillicTitlo

-- | A pattern synonym for 'CombiningCyrillicPalatalization', the name without the @Combining@ part, defined by @'\\x0484'@ (&#x2022;&#x0484;).
pattern CyrillicPalatalization :: CombiningCharacter
pattern CyrillicPalatalization = CombiningCyrillicPalatalization

-- | A pattern synonym for 'CombiningCyrillicDasiaPneumata', the name without the @Combining@ part, defined by @'\\x0485'@ (&#x2022;&#x0485;).
pattern CyrillicDasiaPneumata :: CombiningCharacter
pattern CyrillicDasiaPneumata = CombiningCyrillicDasiaPneumata

-- | A pattern synonym for 'CombiningCyrillicPsiliPneumata', the name without the @Combining@ part, defined by @'\\x0486'@ (&#x2022;&#x0486;).
pattern CyrillicPsiliPneumata :: CombiningCharacter
pattern CyrillicPsiliPneumata = CombiningCyrillicPsiliPneumata

-- | A pattern synonym for 'CombiningCyrillicPokrytie', the name without the @Combining@ part, defined by @'\\x0487'@ (&#x2022;&#x0487;).
pattern CyrillicPokrytie :: CombiningCharacter
pattern CyrillicPokrytie = CombiningCyrillicPokrytie

-- | A pattern synonym for 'NkoCombiningShortHighTone', the name without the @Combining@ part, defined by @'\\x07eb'@ (&#x2022;&#x07eb;).
pattern NkoShortHighTone :: CombiningCharacter
pattern NkoShortHighTone = NkoCombiningShortHighTone

-- | A pattern synonym for 'NkoCombiningShortLowTone', the name without the @Combining@ part, defined by @'\\x07ec'@ (&#x2022;&#x07ec;).
pattern NkoShortLowTone :: CombiningCharacter
pattern NkoShortLowTone = NkoCombiningShortLowTone

-- | A pattern synonym for 'NkoCombiningShortRisingTone', the name without the @Combining@ part, defined by @'\\x07ed'@ (&#x2022;&#x07ed;).
pattern NkoShortRisingTone :: CombiningCharacter
pattern NkoShortRisingTone = NkoCombiningShortRisingTone

-- | A pattern synonym for 'NkoCombiningLongDescendingTone', the name without the @Combining@ part, defined by @'\\x07ee'@ (&#x2022;&#x07ee;).
pattern NkoLongDescendingTone :: CombiningCharacter
pattern NkoLongDescendingTone = NkoCombiningLongDescendingTone

-- | A pattern synonym for 'NkoCombiningLongHighTone', the name without the @Combining@ part, defined by @'\\x07ef'@ (&#x2022;&#x07ef;).
pattern NkoLongHighTone :: CombiningCharacter
pattern NkoLongHighTone = NkoCombiningLongHighTone

-- | A pattern synonym for 'NkoCombiningLongLowTone', the name without the @Combining@ part, defined by @'\\x07f0'@ (&#x2022;&#x07f0;).
pattern NkoLongLowTone :: CombiningCharacter
pattern NkoLongLowTone = NkoCombiningLongLowTone

-- | A pattern synonym for 'NkoCombiningLongRisingTone', the name without the @Combining@ part, defined by @'\\x07f1'@ (&#x2022;&#x07f1;).
pattern NkoLongRisingTone :: CombiningCharacter
pattern NkoLongRisingTone = NkoCombiningLongRisingTone

-- | A pattern synonym for 'NkoCombiningNasalizationMark', the name without the @Combining@ part, defined by @'\\x07f2'@ (&#x2022;&#x07f2;).
pattern NkoNasalizationMark :: CombiningCharacter
pattern NkoNasalizationMark = NkoCombiningNasalizationMark

-- | A pattern synonym for 'NkoCombiningDoubleDotAbove', the name without the @Combining@ part, defined by @'\\x07f3'@ (&#x2022;&#x07f3;).
pattern NkoDoubleDotAbove :: CombiningCharacter
pattern NkoDoubleDotAbove = NkoCombiningDoubleDotAbove

-- | A pattern synonym for 'EthiopicCombiningGeminationAndVowelLengthMark', the name without the @Combining@ part, defined by @'\\x135d'@ (&#x2022;&#x135d;).
pattern EthiopicGeminationAndVowelLengthMark :: CombiningCharacter
pattern EthiopicGeminationAndVowelLengthMark = EthiopicCombiningGeminationAndVowelLengthMark

-- | A pattern synonym for 'EthiopicCombiningVowelLengthMark', the name without the @Combining@ part, defined by @'\\x135e'@ (&#x2022;&#x135e;).
pattern EthiopicVowelLengthMark :: CombiningCharacter
pattern EthiopicVowelLengthMark = EthiopicCombiningVowelLengthMark

-- | A pattern synonym for 'EthiopicCombiningGeminationMark', the name without the @Combining@ part, defined by @'\\x135f'@ (&#x2022;&#x135f;).
pattern EthiopicGeminationMark :: CombiningCharacter
pattern EthiopicGeminationMark = EthiopicCombiningGeminationMark

-- | A pattern synonym for 'TaiThamCombiningCryptogrammicDot', the name without the @Combining@ part, defined by @'\\x1a7f'@ (&#x2022;&#x1a7f;).
pattern TaiThamCryptogrammicDot :: CombiningCharacter
pattern TaiThamCryptogrammicDot = TaiThamCombiningCryptogrammicDot

-- | A pattern synonym for 'CombiningDoubledCircumflexAccent', the name without the @Combining@ part, defined by @'\\x1ab0'@ (&#x2022;&#x1ab0;).
pattern DoubledCircumflexAccent :: CombiningCharacter
pattern DoubledCircumflexAccent = CombiningDoubledCircumflexAccent

-- | A pattern synonym for 'CombiningDiaeresisRing', the name without the @Combining@ part, defined by @'\\x1ab1'@ (&#x2022;&#x1ab1;).
pattern DiaeresisRing :: CombiningCharacter
pattern DiaeresisRing = CombiningDiaeresisRing

-- | A pattern synonym for 'CombiningInfinity', the name without the @Combining@ part, defined by @'\\x1ab2'@ (&#x2022;&#x1ab2;).
pattern Infinity :: CombiningCharacter
pattern Infinity = CombiningInfinity

-- | A pattern synonym for 'CombiningDownwardsArrow', the name without the @Combining@ part, defined by @'\\x1ab3'@ (&#x2022;&#x1ab3;).
pattern DownwardsArrow :: CombiningCharacter
pattern DownwardsArrow = CombiningDownwardsArrow

-- | A pattern synonym for 'CombiningTripleDot', the name without the @Combining@ part, defined by @'\\x1ab4'@ (&#x2022;&#x1ab4;).
pattern TripleDot :: CombiningCharacter
pattern TripleDot = CombiningTripleDot

-- | A pattern synonym for 'CombiningXXBelow', the name without the @Combining@ part, defined by @'\\x1ab5'@ (&#x2022;&#x1ab5;).
pattern XXBelow :: CombiningCharacter
pattern XXBelow = CombiningXXBelow

-- | A pattern synonym for 'CombiningWigglyLineBelow', the name without the @Combining@ part, defined by @'\\x1ab6'@ (&#x2022;&#x1ab6;).
pattern WigglyLineBelow :: CombiningCharacter
pattern WigglyLineBelow = CombiningWigglyLineBelow

-- | A pattern synonym for 'CombiningOpenMarkBelow', the name without the @Combining@ part, defined by @'\\x1ab7'@ (&#x2022;&#x1ab7;).
pattern OpenMarkBelow :: CombiningCharacter
pattern OpenMarkBelow = CombiningOpenMarkBelow

-- | A pattern synonym for 'CombiningDoubleOpenMarkBelow', the name without the @Combining@ part, defined by @'\\x1ab8'@ (&#x2022;&#x1ab8;).
pattern DoubleOpenMarkBelow :: CombiningCharacter
pattern DoubleOpenMarkBelow = CombiningDoubleOpenMarkBelow

-- | A pattern synonym for 'CombiningLightCentralizationStrokeBelow', the name without the @Combining@ part, defined by @'\\x1ab9'@ (&#x2022;&#x1ab9;).
pattern LightCentralizationStrokeBelow :: CombiningCharacter
pattern LightCentralizationStrokeBelow = CombiningLightCentralizationStrokeBelow

-- | A pattern synonym for 'CombiningStrongCentralizationStrokeBelow', the name without the @Combining@ part, defined by @'\\x1aba'@ (&#x2022;&#x1aba;).
pattern StrongCentralizationStrokeBelow :: CombiningCharacter
pattern StrongCentralizationStrokeBelow = CombiningStrongCentralizationStrokeBelow

-- | A pattern synonym for 'CombiningParenthesesAbove', the name without the @Combining@ part, defined by @'\\x1abb'@ (&#x2022;&#x1abb;).
pattern ParenthesesAbove :: CombiningCharacter
pattern ParenthesesAbove = CombiningParenthesesAbove

-- | A pattern synonym for 'CombiningDoubleParenthesesAbove', the name without the @Combining@ part, defined by @'\\x1abc'@ (&#x2022;&#x1abc;).
pattern DoubleParenthesesAbove :: CombiningCharacter
pattern DoubleParenthesesAbove = CombiningDoubleParenthesesAbove

-- | A pattern synonym for 'CombiningParenthesesBelow', the name without the @Combining@ part, defined by @'\\x1abd'@ (&#x2022;&#x1abd;).
pattern ParenthesesBelow :: CombiningCharacter
pattern ParenthesesBelow = CombiningParenthesesBelow

-- | A pattern synonym for 'BalineseMusicalSymbolCombiningTegeh', the name without the @Combining@ part, defined by @'\\x1b6b'@ (&#x2022;&#x1b6b;).
pattern BalineseMusicalSymbolTegeh :: CombiningCharacter
pattern BalineseMusicalSymbolTegeh = BalineseMusicalSymbolCombiningTegeh

-- | A pattern synonym for 'BalineseMusicalSymbolCombiningEndep', the name without the @Combining@ part, defined by @'\\x1b6c'@ (&#x2022;&#x1b6c;).
pattern BalineseMusicalSymbolEndep :: CombiningCharacter
pattern BalineseMusicalSymbolEndep = BalineseMusicalSymbolCombiningEndep

-- | A pattern synonym for 'BalineseMusicalSymbolCombiningKempul', the name without the @Combining@ part, defined by @'\\x1b6d'@ (&#x2022;&#x1b6d;).
pattern BalineseMusicalSymbolKempul :: CombiningCharacter
pattern BalineseMusicalSymbolKempul = BalineseMusicalSymbolCombiningKempul

-- | A pattern synonym for 'BalineseMusicalSymbolCombiningKempli', the name without the @Combining@ part, defined by @'\\x1b6e'@ (&#x2022;&#x1b6e;).
pattern BalineseMusicalSymbolKempli :: CombiningCharacter
pattern BalineseMusicalSymbolKempli = BalineseMusicalSymbolCombiningKempli

-- | A pattern synonym for 'BalineseMusicalSymbolCombiningJegogan', the name without the @Combining@ part, defined by @'\\x1b6f'@ (&#x2022;&#x1b6f;).
pattern BalineseMusicalSymbolJegogan :: CombiningCharacter
pattern BalineseMusicalSymbolJegogan = BalineseMusicalSymbolCombiningJegogan

-- | A pattern synonym for 'BalineseMusicalSymbolCombiningKempulWithJegogan', the name without the @Combining@ part, defined by @'\\x1b70'@ (&#x2022;&#x1b70;).
pattern BalineseMusicalSymbolKempulWithJegogan :: CombiningCharacter
pattern BalineseMusicalSymbolKempulWithJegogan = BalineseMusicalSymbolCombiningKempulWithJegogan

-- | A pattern synonym for 'BalineseMusicalSymbolCombiningKempliWithJegogan', the name without the @Combining@ part, defined by @'\\x1b71'@ (&#x2022;&#x1b71;).
pattern BalineseMusicalSymbolKempliWithJegogan :: CombiningCharacter
pattern BalineseMusicalSymbolKempliWithJegogan = BalineseMusicalSymbolCombiningKempliWithJegogan

-- | A pattern synonym for 'BalineseMusicalSymbolCombiningBende', the name without the @Combining@ part, defined by @'\\x1b72'@ (&#x2022;&#x1b72;).
pattern BalineseMusicalSymbolBende :: CombiningCharacter
pattern BalineseMusicalSymbolBende = BalineseMusicalSymbolCombiningBende

-- | A pattern synonym for 'BalineseMusicalSymbolCombiningGong', the name without the @Combining@ part, defined by @'\\x1b73'@ (&#x2022;&#x1b73;).
pattern BalineseMusicalSymbolGong :: CombiningCharacter
pattern BalineseMusicalSymbolGong = BalineseMusicalSymbolCombiningGong

-- | A pattern synonym for 'CombiningDottedGraveAccent', the name without the @Combining@ part, defined by @'\\x1dc0'@ (&#x2022;&#x1dc0;).
pattern DottedGraveAccent :: CombiningCharacter
pattern DottedGraveAccent = CombiningDottedGraveAccent

-- | A pattern synonym for 'CombiningDottedAcuteAccent', the name without the @Combining@ part, defined by @'\\x1dc1'@ (&#x2022;&#x1dc1;).
pattern DottedAcuteAccent :: CombiningCharacter
pattern DottedAcuteAccent = CombiningDottedAcuteAccent

-- | A pattern synonym for 'CombiningSnakeBelow', the name without the @Combining@ part, defined by @'\\x1dc2'@ (&#x2022;&#x1dc2;).
pattern SnakeBelow :: CombiningCharacter
pattern SnakeBelow = CombiningSnakeBelow

-- | A pattern synonym for 'CombiningSuspensionMark', the name without the @Combining@ part, defined by @'\\x1dc3'@ (&#x2022;&#x1dc3;).
pattern SuspensionMark :: CombiningCharacter
pattern SuspensionMark = CombiningSuspensionMark

-- | A pattern synonym for 'CombiningMacronAcute', the name without the @Combining@ part, defined by @'\\x1dc4'@ (&#x2022;&#x1dc4;).
pattern MacronAcute :: CombiningCharacter
pattern MacronAcute = CombiningMacronAcute

-- | A pattern synonym for 'CombiningGraveMacron', the name without the @Combining@ part, defined by @'\\x1dc5'@ (&#x2022;&#x1dc5;).
pattern GraveMacron :: CombiningCharacter
pattern GraveMacron = CombiningGraveMacron

-- | A pattern synonym for 'CombiningMacronGrave', the name without the @Combining@ part, defined by @'\\x1dc6'@ (&#x2022;&#x1dc6;).
pattern MacronGrave :: CombiningCharacter
pattern MacronGrave = CombiningMacronGrave

-- | A pattern synonym for 'CombiningAcuteMacron', the name without the @Combining@ part, defined by @'\\x1dc7'@ (&#x2022;&#x1dc7;).
pattern AcuteMacron :: CombiningCharacter
pattern AcuteMacron = CombiningAcuteMacron

-- | A pattern synonym for 'CombiningGraveAcuteGrave', the name without the @Combining@ part, defined by @'\\x1dc8'@ (&#x2022;&#x1dc8;).
pattern GraveAcuteGrave :: CombiningCharacter
pattern GraveAcuteGrave = CombiningGraveAcuteGrave

-- | A pattern synonym for 'CombiningAcuteGraveAcute', the name without the @Combining@ part, defined by @'\\x1dc9'@ (&#x2022;&#x1dc9;).
pattern AcuteGraveAcute :: CombiningCharacter
pattern AcuteGraveAcute = CombiningAcuteGraveAcute

-- | A pattern synonym for 'CombiningLatinSmallLetterRBelow', the name without the @Combining@ part, defined by @'\\x1dca'@ (&#x2022;&#x1dca;).
pattern LatinSmallLetterRBelow :: CombiningCharacter
pattern LatinSmallLetterRBelow = CombiningLatinSmallLetterRBelow

-- | A pattern synonym for 'CombiningBreveMacron', the name without the @Combining@ part, defined by @'\\x1dcb'@ (&#x2022;&#x1dcb;).
pattern BreveMacron :: CombiningCharacter
pattern BreveMacron = CombiningBreveMacron

-- | A pattern synonym for 'CombiningMacronBreve', the name without the @Combining@ part, defined by @'\\x1dcc'@ (&#x2022;&#x1dcc;).
pattern MacronBreve :: CombiningCharacter
pattern MacronBreve = CombiningMacronBreve

-- | A pattern synonym for 'CombiningDoubleCircumflexAbove', the name without the @Combining@ part, defined by @'\\x1dcd'@ (&#x2022;&#x1dcd;).
pattern DoubleCircumflexAbove :: CombiningCharacter
pattern DoubleCircumflexAbove = CombiningDoubleCircumflexAbove

-- | A pattern synonym for 'CombiningOgonekAbove', the name without the @Combining@ part, defined by @'\\x1dce'@ (&#x2022;&#x1dce;).
pattern OgonekAbove :: CombiningCharacter
pattern OgonekAbove = CombiningOgonekAbove

-- | A pattern synonym for 'CombiningZigzagBelow', the name without the @Combining@ part, defined by @'\\x1dcf'@ (&#x2022;&#x1dcf;).
pattern ZigzagBelow :: CombiningCharacter
pattern ZigzagBelow = CombiningZigzagBelow

-- | A pattern synonym for 'CombiningIsBelow', the name without the @Combining@ part, defined by @'\\x1dd0'@ (&#x2022;&#x1dd0;).
pattern IsBelow :: CombiningCharacter
pattern IsBelow = CombiningIsBelow

-- | A pattern synonym for 'CombiningUrAbove', the name without the @Combining@ part, defined by @'\\x1dd1'@ (&#x2022;&#x1dd1;).
pattern UrAbove :: CombiningCharacter
pattern UrAbove = CombiningUrAbove

-- | A pattern synonym for 'CombiningUsAbove', the name without the @Combining@ part, defined by @'\\x1dd2'@ (&#x2022;&#x1dd2;).
pattern UsAbove :: CombiningCharacter
pattern UsAbove = CombiningUsAbove

-- | A pattern synonym for 'CombiningLatinSmallLetterFlattenedOpenAAbove', the name without the @Combining@ part, defined by @'\\x1dd3'@ (&#x2022;&#x1dd3;).
pattern LatinSmallLetterFlattenedOpenAAbove :: CombiningCharacter
pattern LatinSmallLetterFlattenedOpenAAbove = CombiningLatinSmallLetterFlattenedOpenAAbove

-- | A pattern synonym for 'CombiningLatinSmallLetterAe', the name without the @Combining@ part, defined by @'\\x1dd4'@ (&#x2022;&#x1dd4;).
pattern LatinSmallLetterAe :: CombiningCharacter
pattern LatinSmallLetterAe = CombiningLatinSmallLetterAe

-- | A pattern synonym for 'CombiningLatinSmallLetterAo', the name without the @Combining@ part, defined by @'\\x1dd5'@ (&#x2022;&#x1dd5;).
pattern LatinSmallLetterAo :: CombiningCharacter
pattern LatinSmallLetterAo = CombiningLatinSmallLetterAo

-- | A pattern synonym for 'CombiningLatinSmallLetterAv', the name without the @Combining@ part, defined by @'\\x1dd6'@ (&#x2022;&#x1dd6;).
pattern LatinSmallLetterAv :: CombiningCharacter
pattern LatinSmallLetterAv = CombiningLatinSmallLetterAv

-- | A pattern synonym for 'CombiningLatinSmallLetterCCedilla', the name without the @Combining@ part, defined by @'\\x1dd7'@ (&#x2022;&#x1dd7;).
pattern LatinSmallLetterCCedilla :: CombiningCharacter
pattern LatinSmallLetterCCedilla = CombiningLatinSmallLetterCCedilla

-- | A pattern synonym for 'CombiningLatinSmallLetterInsularD', the name without the @Combining@ part, defined by @'\\x1dd8'@ (&#x2022;&#x1dd8;).
pattern LatinSmallLetterInsularD :: CombiningCharacter
pattern LatinSmallLetterInsularD = CombiningLatinSmallLetterInsularD

-- | A pattern synonym for 'CombiningLatinSmallLetterEth', the name without the @Combining@ part, defined by @'\\x1dd9'@ (&#x2022;&#x1dd9;).
pattern LatinSmallLetterEth :: CombiningCharacter
pattern LatinSmallLetterEth = CombiningLatinSmallLetterEth

-- | A pattern synonym for 'CombiningLatinSmallLetterG', the name without the @Combining@ part, defined by @'\\x1dda'@ (&#x2022;&#x1dda;).
pattern LatinSmallLetterG :: CombiningCharacter
pattern LatinSmallLetterG = CombiningLatinSmallLetterG

-- | A pattern synonym for 'CombiningLatinLetterSmallCapitalG', the name without the @Combining@ part, defined by @'\\x1ddb'@ (&#x2022;&#x1ddb;).
pattern LatinLetterSmallCapitalG :: CombiningCharacter
pattern LatinLetterSmallCapitalG = CombiningLatinLetterSmallCapitalG

-- | A pattern synonym for 'CombiningLatinSmallLetterK', the name without the @Combining@ part, defined by @'\\x1ddc'@ (&#x2022;&#x1ddc;).
pattern LatinSmallLetterK :: CombiningCharacter
pattern LatinSmallLetterK = CombiningLatinSmallLetterK

-- | A pattern synonym for 'CombiningLatinSmallLetterL', the name without the @Combining@ part, defined by @'\\x1ddd'@ (&#x2022;&#x1ddd;).
pattern LatinSmallLetterL :: CombiningCharacter
pattern LatinSmallLetterL = CombiningLatinSmallLetterL

-- | A pattern synonym for 'CombiningLatinLetterSmallCapitalL', the name without the @Combining@ part, defined by @'\\x1dde'@ (&#x2022;&#x1dde;).
pattern LatinLetterSmallCapitalL :: CombiningCharacter
pattern LatinLetterSmallCapitalL = CombiningLatinLetterSmallCapitalL

-- | A pattern synonym for 'CombiningLatinLetterSmallCapitalM', the name without the @Combining@ part, defined by @'\\x1ddf'@ (&#x2022;&#x1ddf;).
pattern LatinLetterSmallCapitalM :: CombiningCharacter
pattern LatinLetterSmallCapitalM = CombiningLatinLetterSmallCapitalM

-- | A pattern synonym for 'CombiningLatinSmallLetterN', the name without the @Combining@ part, defined by @'\\x1de0'@ (&#x2022;&#x1de0;).
pattern LatinSmallLetterN :: CombiningCharacter
pattern LatinSmallLetterN = CombiningLatinSmallLetterN

-- | A pattern synonym for 'CombiningLatinLetterSmallCapitalN', the name without the @Combining@ part, defined by @'\\x1de1'@ (&#x2022;&#x1de1;).
pattern LatinLetterSmallCapitalN :: CombiningCharacter
pattern LatinLetterSmallCapitalN = CombiningLatinLetterSmallCapitalN

-- | A pattern synonym for 'CombiningLatinLetterSmallCapitalR', the name without the @Combining@ part, defined by @'\\x1de2'@ (&#x2022;&#x1de2;).
pattern LatinLetterSmallCapitalR :: CombiningCharacter
pattern LatinLetterSmallCapitalR = CombiningLatinLetterSmallCapitalR

-- | A pattern synonym for 'CombiningLatinSmallLetterRRotunda', the name without the @Combining@ part, defined by @'\\x1de3'@ (&#x2022;&#x1de3;).
pattern LatinSmallLetterRRotunda :: CombiningCharacter
pattern LatinSmallLetterRRotunda = CombiningLatinSmallLetterRRotunda

-- | A pattern synonym for 'CombiningLatinSmallLetterS', the name without the @Combining@ part, defined by @'\\x1de4'@ (&#x2022;&#x1de4;).
pattern LatinSmallLetterS :: CombiningCharacter
pattern LatinSmallLetterS = CombiningLatinSmallLetterS

-- | A pattern synonym for 'CombiningLatinSmallLetterLongS', the name without the @Combining@ part, defined by @'\\x1de5'@ (&#x2022;&#x1de5;).
pattern LatinSmallLetterLongS :: CombiningCharacter
pattern LatinSmallLetterLongS = CombiningLatinSmallLetterLongS

-- | A pattern synonym for 'CombiningLatinSmallLetterZ', the name without the @Combining@ part, defined by @'\\x1de6'@ (&#x2022;&#x1de6;).
pattern LatinSmallLetterZ :: CombiningCharacter
pattern LatinSmallLetterZ = CombiningLatinSmallLetterZ

-- | A pattern synonym for 'CombiningLatinSmallLetterAlpha', the name without the @Combining@ part, defined by @'\\x1de7'@ (&#x2022;&#x1de7;).
pattern LatinSmallLetterAlpha :: CombiningCharacter
pattern LatinSmallLetterAlpha = CombiningLatinSmallLetterAlpha

-- | A pattern synonym for 'CombiningLatinSmallLetterB', the name without the @Combining@ part, defined by @'\\x1de8'@ (&#x2022;&#x1de8;).
pattern LatinSmallLetterB :: CombiningCharacter
pattern LatinSmallLetterB = CombiningLatinSmallLetterB

-- | A pattern synonym for 'CombiningLatinSmallLetterBeta', the name without the @Combining@ part, defined by @'\\x1de9'@ (&#x2022;&#x1de9;).
pattern LatinSmallLetterBeta :: CombiningCharacter
pattern LatinSmallLetterBeta = CombiningLatinSmallLetterBeta

-- | A pattern synonym for 'CombiningLatinSmallLetterSchwa', the name without the @Combining@ part, defined by @'\\x1dea'@ (&#x2022;&#x1dea;).
pattern LatinSmallLetterSchwa :: CombiningCharacter
pattern LatinSmallLetterSchwa = CombiningLatinSmallLetterSchwa

-- | A pattern synonym for 'CombiningLatinSmallLetterF', the name without the @Combining@ part, defined by @'\\x1deb'@ (&#x2022;&#x1deb;).
pattern LatinSmallLetterF :: CombiningCharacter
pattern LatinSmallLetterF = CombiningLatinSmallLetterF

-- | A pattern synonym for 'CombiningLatinSmallLetterLWithDoubleMiddleTilde', the name without the @Combining@ part, defined by @'\\x1dec'@ (&#x2022;&#x1dec;).
pattern LatinSmallLetterLWithDoubleMiddleTilde :: CombiningCharacter
pattern LatinSmallLetterLWithDoubleMiddleTilde = CombiningLatinSmallLetterLWithDoubleMiddleTilde

-- | A pattern synonym for 'CombiningLatinSmallLetterOWithLightCentralizationStroke', the name without the @Combining@ part, defined by @'\\x1ded'@ (&#x2022;&#x1ded;).
pattern LatinSmallLetterOWithLightCentralizationStroke :: CombiningCharacter
pattern LatinSmallLetterOWithLightCentralizationStroke = CombiningLatinSmallLetterOWithLightCentralizationStroke

-- | A pattern synonym for 'CombiningLatinSmallLetterP', the name without the @Combining@ part, defined by @'\\x1dee'@ (&#x2022;&#x1dee;).
pattern LatinSmallLetterP :: CombiningCharacter
pattern LatinSmallLetterP = CombiningLatinSmallLetterP

-- | A pattern synonym for 'CombiningLatinSmallLetterEsh', the name without the @Combining@ part, defined by @'\\x1def'@ (&#x2022;&#x1def;).
pattern LatinSmallLetterEsh :: CombiningCharacter
pattern LatinSmallLetterEsh = CombiningLatinSmallLetterEsh

-- | A pattern synonym for 'CombiningLatinSmallLetterUWithLightCentralizationStroke', the name without the @Combining@ part, defined by @'\\x1df0'@ (&#x2022;&#x1df0;).
pattern LatinSmallLetterUWithLightCentralizationStroke :: CombiningCharacter
pattern LatinSmallLetterUWithLightCentralizationStroke = CombiningLatinSmallLetterUWithLightCentralizationStroke

-- | A pattern synonym for 'CombiningLatinSmallLetterW', the name without the @Combining@ part, defined by @'\\x1df1'@ (&#x2022;&#x1df1;).
pattern LatinSmallLetterW :: CombiningCharacter
pattern LatinSmallLetterW = CombiningLatinSmallLetterW

-- | A pattern synonym for 'CombiningLatinSmallLetterAWithDiaeresis', the name without the @Combining@ part, defined by @'\\x1df2'@ (&#x2022;&#x1df2;).
pattern LatinSmallLetterAWithDiaeresis :: CombiningCharacter
pattern LatinSmallLetterAWithDiaeresis = CombiningLatinSmallLetterAWithDiaeresis

-- | A pattern synonym for 'CombiningLatinSmallLetterOWithDiaeresis', the name without the @Combining@ part, defined by @'\\x1df3'@ (&#x2022;&#x1df3;).
pattern LatinSmallLetterOWithDiaeresis :: CombiningCharacter
pattern LatinSmallLetterOWithDiaeresis = CombiningLatinSmallLetterOWithDiaeresis

-- | A pattern synonym for 'CombiningLatinSmallLetterUWithDiaeresis', the name without the @Combining@ part, defined by @'\\x1df4'@ (&#x2022;&#x1df4;).
pattern LatinSmallLetterUWithDiaeresis :: CombiningCharacter
pattern LatinSmallLetterUWithDiaeresis = CombiningLatinSmallLetterUWithDiaeresis

-- | A pattern synonym for 'CombiningUpTackAbove', the name without the @Combining@ part, defined by @'\\x1df5'@ (&#x2022;&#x1df5;).
pattern UpTackAbove :: CombiningCharacter
pattern UpTackAbove = CombiningUpTackAbove

-- | A pattern synonym for 'CombiningDeletionMark', the name without the @Combining@ part, defined by @'\\x1dfb'@ (&#x2022;&#x1dfb;).
pattern DeletionMark :: CombiningCharacter
pattern DeletionMark = CombiningDeletionMark

-- | A pattern synonym for 'CombiningDoubleInvertedBreveBelow', the name without the @Combining@ part, defined by @'\\x1dfc'@ (&#x2022;&#x1dfc;).
pattern DoubleInvertedBreveBelow :: CombiningCharacter
pattern DoubleInvertedBreveBelow = CombiningDoubleInvertedBreveBelow

-- | A pattern synonym for 'CombiningAlmostEqualToBelow', the name without the @Combining@ part, defined by @'\\x1dfd'@ (&#x2022;&#x1dfd;).
pattern AlmostEqualToBelow :: CombiningCharacter
pattern AlmostEqualToBelow = CombiningAlmostEqualToBelow

-- | A pattern synonym for 'CombiningLeftArrowheadAbove', the name without the @Combining@ part, defined by @'\\x1dfe'@ (&#x2022;&#x1dfe;).
pattern LeftArrowheadAbove :: CombiningCharacter
pattern LeftArrowheadAbove = CombiningLeftArrowheadAbove

-- | A pattern synonym for 'CombiningRightArrowheadAndDownArrowheadBelow', the name without the @Combining@ part, defined by @'\\x1dff'@ (&#x2022;&#x1dff;).
pattern RightArrowheadAndDownArrowheadBelow :: CombiningCharacter
pattern RightArrowheadAndDownArrowheadBelow = CombiningRightArrowheadAndDownArrowheadBelow

-- | A pattern synonym for 'CombiningLeftHarpoonAbove', the name without the @Combining@ part, defined by @'\\x20d0'@ (&#x2022;&#x20d0;).
pattern LeftHarpoonAbove :: CombiningCharacter
pattern LeftHarpoonAbove = CombiningLeftHarpoonAbove

-- | A pattern synonym for 'CombiningRightHarpoonAbove', the name without the @Combining@ part, defined by @'\\x20d1'@ (&#x2022;&#x20d1;).
pattern RightHarpoonAbove :: CombiningCharacter
pattern RightHarpoonAbove = CombiningRightHarpoonAbove

-- | A pattern synonym for 'CombiningLongVerticalLineOverlay', the name without the @Combining@ part, defined by @'\\x20d2'@ (&#x2022;&#x20d2;).
pattern LongVerticalLineOverlay :: CombiningCharacter
pattern LongVerticalLineOverlay = CombiningLongVerticalLineOverlay

-- | A pattern synonym for 'CombiningShortVerticalLineOverlay', the name without the @Combining@ part, defined by @'\\x20d3'@ (&#x2022;&#x20d3;).
pattern ShortVerticalLineOverlay :: CombiningCharacter
pattern ShortVerticalLineOverlay = CombiningShortVerticalLineOverlay

-- | A pattern synonym for 'CombiningAnticlockwiseArrowAbove', the name without the @Combining@ part, defined by @'\\x20d4'@ (&#x2022;&#x20d4;).
pattern AnticlockwiseArrowAbove :: CombiningCharacter
pattern AnticlockwiseArrowAbove = CombiningAnticlockwiseArrowAbove

-- | A pattern synonym for 'CombiningClockwiseArrowAbove', the name without the @Combining@ part, defined by @'\\x20d5'@ (&#x2022;&#x20d5;).
pattern ClockwiseArrowAbove :: CombiningCharacter
pattern ClockwiseArrowAbove = CombiningClockwiseArrowAbove

-- | A pattern synonym for 'CombiningLeftArrowAbove', the name without the @Combining@ part, defined by @'\\x20d6'@ (&#x2022;&#x20d6;).
pattern LeftArrowAbove :: CombiningCharacter
pattern LeftArrowAbove = CombiningLeftArrowAbove

-- | A pattern synonym for 'CombiningRightArrowAbove', the name without the @Combining@ part, defined by @'\\x20d7'@ (&#x2022;&#x20d7;).
pattern RightArrowAbove :: CombiningCharacter
pattern RightArrowAbove = CombiningRightArrowAbove

-- | A pattern synonym for 'CombiningRingOverlay', the name without the @Combining@ part, defined by @'\\x20d8'@ (&#x2022;&#x20d8;).
pattern RingOverlay :: CombiningCharacter
pattern RingOverlay = CombiningRingOverlay

-- | A pattern synonym for 'CombiningClockwiseRingOverlay', the name without the @Combining@ part, defined by @'\\x20d9'@ (&#x2022;&#x20d9;).
pattern ClockwiseRingOverlay :: CombiningCharacter
pattern ClockwiseRingOverlay = CombiningClockwiseRingOverlay

-- | A pattern synonym for 'CombiningAnticlockwiseRingOverlay', the name without the @Combining@ part, defined by @'\\x20da'@ (&#x2022;&#x20da;).
pattern AnticlockwiseRingOverlay :: CombiningCharacter
pattern AnticlockwiseRingOverlay = CombiningAnticlockwiseRingOverlay

-- | A pattern synonym for 'CombiningThreeDotsAbove', the name without the @Combining@ part, defined by @'\\x20db'@ (&#x2022;&#x20db;).
pattern ThreeDotsAbove :: CombiningCharacter
pattern ThreeDotsAbove = CombiningThreeDotsAbove

-- | A pattern synonym for 'CombiningFourDotsAbove', the name without the @Combining@ part, defined by @'\\x20dc'@ (&#x2022;&#x20dc;).
pattern FourDotsAbove :: CombiningCharacter
pattern FourDotsAbove = CombiningFourDotsAbove

-- | A pattern synonym for 'CombiningLeftRightArrowAbove', the name without the @Combining@ part, defined by @'\\x20e1'@ (&#x2022;&#x20e1;).
pattern LeftRightArrowAbove :: CombiningCharacter
pattern LeftRightArrowAbove = CombiningLeftRightArrowAbove

-- | A pattern synonym for 'CombiningReverseSolidusOverlay', the name without the @Combining@ part, defined by @'\\x20e5'@ (&#x2022;&#x20e5;).
pattern ReverseSolidusOverlay :: CombiningCharacter
pattern ReverseSolidusOverlay = CombiningReverseSolidusOverlay

-- | A pattern synonym for 'CombiningDoubleVerticalStrokeOverlay', the name without the @Combining@ part, defined by @'\\x20e6'@ (&#x2022;&#x20e6;).
pattern DoubleVerticalStrokeOverlay :: CombiningCharacter
pattern DoubleVerticalStrokeOverlay = CombiningDoubleVerticalStrokeOverlay

-- | A pattern synonym for 'CombiningAnnuitySymbol', the name without the @Combining@ part, defined by @'\\x20e7'@ (&#x2022;&#x20e7;).
pattern AnnuitySymbol :: CombiningCharacter
pattern AnnuitySymbol = CombiningAnnuitySymbol

-- | A pattern synonym for 'CombiningTripleUnderdot', the name without the @Combining@ part, defined by @'\\x20e8'@ (&#x2022;&#x20e8;).
pattern TripleUnderdot :: CombiningCharacter
pattern TripleUnderdot = CombiningTripleUnderdot

-- | A pattern synonym for 'CombiningWideBridgeAbove', the name without the @Combining@ part, defined by @'\\x20e9'@ (&#x2022;&#x20e9;).
pattern WideBridgeAbove :: CombiningCharacter
pattern WideBridgeAbove = CombiningWideBridgeAbove

-- | A pattern synonym for 'CombiningLeftwardsArrowOverlay', the name without the @Combining@ part, defined by @'\\x20ea'@ (&#x2022;&#x20ea;).
pattern LeftwardsArrowOverlay :: CombiningCharacter
pattern LeftwardsArrowOverlay = CombiningLeftwardsArrowOverlay

-- | A pattern synonym for 'CombiningLongDoubleSolidusOverlay', the name without the @Combining@ part, defined by @'\\x20eb'@ (&#x2022;&#x20eb;).
pattern LongDoubleSolidusOverlay :: CombiningCharacter
pattern LongDoubleSolidusOverlay = CombiningLongDoubleSolidusOverlay

-- | A pattern synonym for 'CombiningRightwardsHarpoonWithBarbDownwards', the name without the @Combining@ part, defined by @'\\x20ec'@ (&#x2022;&#x20ec;).
pattern RightwardsHarpoonWithBarbDownwards :: CombiningCharacter
pattern RightwardsHarpoonWithBarbDownwards = CombiningRightwardsHarpoonWithBarbDownwards

-- | A pattern synonym for 'CombiningLeftwardsHarpoonWithBarbDownwards', the name without the @Combining@ part, defined by @'\\x20ed'@ (&#x2022;&#x20ed;).
pattern LeftwardsHarpoonWithBarbDownwards :: CombiningCharacter
pattern LeftwardsHarpoonWithBarbDownwards = CombiningLeftwardsHarpoonWithBarbDownwards

-- | A pattern synonym for 'CombiningLeftArrowBelow', the name without the @Combining@ part, defined by @'\\x20ee'@ (&#x2022;&#x20ee;).
pattern LeftArrowBelow :: CombiningCharacter
pattern LeftArrowBelow = CombiningLeftArrowBelow

-- | A pattern synonym for 'CombiningRightArrowBelow', the name without the @Combining@ part, defined by @'\\x20ef'@ (&#x2022;&#x20ef;).
pattern RightArrowBelow :: CombiningCharacter
pattern RightArrowBelow = CombiningRightArrowBelow

-- | A pattern synonym for 'CombiningAsteriskAbove', the name without the @Combining@ part, defined by @'\\x20f0'@ (&#x2022;&#x20f0;).
pattern AsteriskAbove :: CombiningCharacter
pattern AsteriskAbove = CombiningAsteriskAbove

-- | A pattern synonym for 'CopticCombiningNiAbove', the name without the @Combining@ part, defined by @'\\x2cef'@ (&#x2022;&#x2cef;).
pattern CopticNiAbove :: CombiningCharacter
pattern CopticNiAbove = CopticCombiningNiAbove

-- | A pattern synonym for 'CopticCombiningSpiritusAsper', the name without the @Combining@ part, defined by @'\\x2cf0'@ (&#x2022;&#x2cf0;).
pattern CopticSpiritusAsper :: CombiningCharacter
pattern CopticSpiritusAsper = CopticCombiningSpiritusAsper

-- | A pattern synonym for 'CopticCombiningSpiritusLenis', the name without the @Combining@ part, defined by @'\\x2cf1'@ (&#x2022;&#x2cf1;).
pattern CopticSpiritusLenis :: CombiningCharacter
pattern CopticSpiritusLenis = CopticCombiningSpiritusLenis

-- | A pattern synonym for 'CombiningCyrillicLetterBe', the name without the @Combining@ part, defined by @'\\x2de0'@ (&#x2022;&#x2de0;).
pattern CyrillicLetterBe :: CombiningCharacter
pattern CyrillicLetterBe = CombiningCyrillicLetterBe

-- | A pattern synonym for 'CombiningCyrillicLetterVe', the name without the @Combining@ part, defined by @'\\x2de1'@ (&#x2022;&#x2de1;).
pattern CyrillicLetterVe :: CombiningCharacter
pattern CyrillicLetterVe = CombiningCyrillicLetterVe

-- | A pattern synonym for 'CombiningCyrillicLetterGhe', the name without the @Combining@ part, defined by @'\\x2de2'@ (&#x2022;&#x2de2;).
pattern CyrillicLetterGhe :: CombiningCharacter
pattern CyrillicLetterGhe = CombiningCyrillicLetterGhe

-- | A pattern synonym for 'CombiningCyrillicLetterDe', the name without the @Combining@ part, defined by @'\\x2de3'@ (&#x2022;&#x2de3;).
pattern CyrillicLetterDe :: CombiningCharacter
pattern CyrillicLetterDe = CombiningCyrillicLetterDe

-- | A pattern synonym for 'CombiningCyrillicLetterZhe', the name without the @Combining@ part, defined by @'\\x2de4'@ (&#x2022;&#x2de4;).
pattern CyrillicLetterZhe :: CombiningCharacter
pattern CyrillicLetterZhe = CombiningCyrillicLetterZhe

-- | A pattern synonym for 'CombiningCyrillicLetterZe', the name without the @Combining@ part, defined by @'\\x2de5'@ (&#x2022;&#x2de5;).
pattern CyrillicLetterZe :: CombiningCharacter
pattern CyrillicLetterZe = CombiningCyrillicLetterZe

-- | A pattern synonym for 'CombiningCyrillicLetterKa', the name without the @Combining@ part, defined by @'\\x2de6'@ (&#x2022;&#x2de6;).
pattern CyrillicLetterKa :: CombiningCharacter
pattern CyrillicLetterKa = CombiningCyrillicLetterKa

-- | A pattern synonym for 'CombiningCyrillicLetterEl', the name without the @Combining@ part, defined by @'\\x2de7'@ (&#x2022;&#x2de7;).
pattern CyrillicLetterEl :: CombiningCharacter
pattern CyrillicLetterEl = CombiningCyrillicLetterEl

-- | A pattern synonym for 'CombiningCyrillicLetterEm', the name without the @Combining@ part, defined by @'\\x2de8'@ (&#x2022;&#x2de8;).
pattern CyrillicLetterEm :: CombiningCharacter
pattern CyrillicLetterEm = CombiningCyrillicLetterEm

-- | A pattern synonym for 'CombiningCyrillicLetterEn', the name without the @Combining@ part, defined by @'\\x2de9'@ (&#x2022;&#x2de9;).
pattern CyrillicLetterEn :: CombiningCharacter
pattern CyrillicLetterEn = CombiningCyrillicLetterEn

-- | A pattern synonym for 'CombiningCyrillicLetterO', the name without the @Combining@ part, defined by @'\\x2dea'@ (&#x2022;&#x2dea;).
pattern CyrillicLetterO :: CombiningCharacter
pattern CyrillicLetterO = CombiningCyrillicLetterO

-- | A pattern synonym for 'CombiningCyrillicLetterPe', the name without the @Combining@ part, defined by @'\\x2deb'@ (&#x2022;&#x2deb;).
pattern CyrillicLetterPe :: CombiningCharacter
pattern CyrillicLetterPe = CombiningCyrillicLetterPe

-- | A pattern synonym for 'CombiningCyrillicLetterEr', the name without the @Combining@ part, defined by @'\\x2dec'@ (&#x2022;&#x2dec;).
pattern CyrillicLetterEr :: CombiningCharacter
pattern CyrillicLetterEr = CombiningCyrillicLetterEr

-- | A pattern synonym for 'CombiningCyrillicLetterEs', the name without the @Combining@ part, defined by @'\\x2ded'@ (&#x2022;&#x2ded;).
pattern CyrillicLetterEs :: CombiningCharacter
pattern CyrillicLetterEs = CombiningCyrillicLetterEs

-- | A pattern synonym for 'CombiningCyrillicLetterTe', the name without the @Combining@ part, defined by @'\\x2dee'@ (&#x2022;&#x2dee;).
pattern CyrillicLetterTe :: CombiningCharacter
pattern CyrillicLetterTe = CombiningCyrillicLetterTe

-- | A pattern synonym for 'CombiningCyrillicLetterHa', the name without the @Combining@ part, defined by @'\\x2def'@ (&#x2022;&#x2def;).
pattern CyrillicLetterHa :: CombiningCharacter
pattern CyrillicLetterHa = CombiningCyrillicLetterHa

-- | A pattern synonym for 'CombiningCyrillicLetterTse', the name without the @Combining@ part, defined by @'\\x2df0'@ (&#x2022;&#x2df0;).
pattern CyrillicLetterTse :: CombiningCharacter
pattern CyrillicLetterTse = CombiningCyrillicLetterTse

-- | A pattern synonym for 'CombiningCyrillicLetterChe', the name without the @Combining@ part, defined by @'\\x2df1'@ (&#x2022;&#x2df1;).
pattern CyrillicLetterChe :: CombiningCharacter
pattern CyrillicLetterChe = CombiningCyrillicLetterChe

-- | A pattern synonym for 'CombiningCyrillicLetterSha', the name without the @Combining@ part, defined by @'\\x2df2'@ (&#x2022;&#x2df2;).
pattern CyrillicLetterSha :: CombiningCharacter
pattern CyrillicLetterSha = CombiningCyrillicLetterSha

-- | A pattern synonym for 'CombiningCyrillicLetterShcha', the name without the @Combining@ part, defined by @'\\x2df3'@ (&#x2022;&#x2df3;).
pattern CyrillicLetterShcha :: CombiningCharacter
pattern CyrillicLetterShcha = CombiningCyrillicLetterShcha

-- | A pattern synonym for 'CombiningCyrillicLetterFita', the name without the @Combining@ part, defined by @'\\x2df4'@ (&#x2022;&#x2df4;).
pattern CyrillicLetterFita :: CombiningCharacter
pattern CyrillicLetterFita = CombiningCyrillicLetterFita

-- | A pattern synonym for 'CombiningCyrillicLetterEsTe', the name without the @Combining@ part, defined by @'\\x2df5'@ (&#x2022;&#x2df5;).
pattern CyrillicLetterEsTe :: CombiningCharacter
pattern CyrillicLetterEsTe = CombiningCyrillicLetterEsTe

-- | A pattern synonym for 'CombiningCyrillicLetterA', the name without the @Combining@ part, defined by @'\\x2df6'@ (&#x2022;&#x2df6;).
pattern CyrillicLetterA :: CombiningCharacter
pattern CyrillicLetterA = CombiningCyrillicLetterA

-- | A pattern synonym for 'CombiningCyrillicLetterIe', the name without the @Combining@ part, defined by @'\\x2df7'@ (&#x2022;&#x2df7;).
pattern CyrillicLetterIe :: CombiningCharacter
pattern CyrillicLetterIe = CombiningCyrillicLetterIe

-- | A pattern synonym for 'CombiningCyrillicLetterDjerv', the name without the @Combining@ part, defined by @'\\x2df8'@ (&#x2022;&#x2df8;).
pattern CyrillicLetterDjerv :: CombiningCharacter
pattern CyrillicLetterDjerv = CombiningCyrillicLetterDjerv

-- | A pattern synonym for 'CombiningCyrillicLetterMonographUk', the name without the @Combining@ part, defined by @'\\x2df9'@ (&#x2022;&#x2df9;).
pattern CyrillicLetterMonographUk :: CombiningCharacter
pattern CyrillicLetterMonographUk = CombiningCyrillicLetterMonographUk

-- | A pattern synonym for 'CombiningCyrillicLetterYat', the name without the @Combining@ part, defined by @'\\x2dfa'@ (&#x2022;&#x2dfa;).
pattern CyrillicLetterYat :: CombiningCharacter
pattern CyrillicLetterYat = CombiningCyrillicLetterYat

-- | A pattern synonym for 'CombiningCyrillicLetterYu', the name without the @Combining@ part, defined by @'\\x2dfb'@ (&#x2022;&#x2dfb;).
pattern CyrillicLetterYu :: CombiningCharacter
pattern CyrillicLetterYu = CombiningCyrillicLetterYu

-- | A pattern synonym for 'CombiningCyrillicLetterIotifiedA', the name without the @Combining@ part, defined by @'\\x2dfc'@ (&#x2022;&#x2dfc;).
pattern CyrillicLetterIotifiedA :: CombiningCharacter
pattern CyrillicLetterIotifiedA = CombiningCyrillicLetterIotifiedA

-- | A pattern synonym for 'CombiningCyrillicLetterLittleYus', the name without the @Combining@ part, defined by @'\\x2dfd'@ (&#x2022;&#x2dfd;).
pattern CyrillicLetterLittleYus :: CombiningCharacter
pattern CyrillicLetterLittleYus = CombiningCyrillicLetterLittleYus

-- | A pattern synonym for 'CombiningCyrillicLetterBigYus', the name without the @Combining@ part, defined by @'\\x2dfe'@ (&#x2022;&#x2dfe;).
pattern CyrillicLetterBigYus :: CombiningCharacter
pattern CyrillicLetterBigYus = CombiningCyrillicLetterBigYus

-- | A pattern synonym for 'CombiningCyrillicLetterIotifiedBigYus', the name without the @Combining@ part, defined by @'\\x2dff'@ (&#x2022;&#x2dff;).
pattern CyrillicLetterIotifiedBigYus :: CombiningCharacter
pattern CyrillicLetterIotifiedBigYus = CombiningCyrillicLetterIotifiedBigYus

-- | A pattern synonym for 'CombiningKatakanaHiraganaVoicedSoundMark', the name without the @Combining@ part, defined by @'\\x3099'@ (&#x2022;&#x3099;).
pattern KatakanaHiraganaVoicedSoundMark :: CombiningCharacter
pattern KatakanaHiraganaVoicedSoundMark = CombiningKatakanaHiraganaVoicedSoundMark

-- | A pattern synonym for 'CombiningKatakanaHiraganaSemiVoicedSoundMark', the name without the @Combining@ part, defined by @'\\x309a'@ (&#x2022;&#x309a;).
pattern KatakanaHiraganaSemiVoicedSoundMark :: CombiningCharacter
pattern KatakanaHiraganaSemiVoicedSoundMark = CombiningKatakanaHiraganaSemiVoicedSoundMark

-- | A pattern synonym for 'CombiningCyrillicVzmet', the name without the @Combining@ part, defined by @'\\xa66f'@ (&#x2022;&#xa66f;).
pattern CyrillicVzmet :: CombiningCharacter
pattern CyrillicVzmet = CombiningCyrillicVzmet

-- | A pattern synonym for 'CombiningCyrillicLetterUkrainianIe', the name without the @Combining@ part, defined by @'\\xa674'@ (&#x2022;&#xa674;).
pattern CyrillicLetterUkrainianIe :: CombiningCharacter
pattern CyrillicLetterUkrainianIe = CombiningCyrillicLetterUkrainianIe

-- | A pattern synonym for 'CombiningCyrillicLetterI', the name without the @Combining@ part, defined by @'\\xa675'@ (&#x2022;&#xa675;).
pattern CyrillicLetterI :: CombiningCharacter
pattern CyrillicLetterI = CombiningCyrillicLetterI

-- | A pattern synonym for 'CombiningCyrillicLetterYi', the name without the @Combining@ part, defined by @'\\xa676'@ (&#x2022;&#xa676;).
pattern CyrillicLetterYi :: CombiningCharacter
pattern CyrillicLetterYi = CombiningCyrillicLetterYi

-- | A pattern synonym for 'CombiningCyrillicLetterU', the name without the @Combining@ part, defined by @'\\xa677'@ (&#x2022;&#xa677;).
pattern CyrillicLetterU :: CombiningCharacter
pattern CyrillicLetterU = CombiningCyrillicLetterU

-- | A pattern synonym for 'CombiningCyrillicLetterHardSign', the name without the @Combining@ part, defined by @'\\xa678'@ (&#x2022;&#xa678;).
pattern CyrillicLetterHardSign :: CombiningCharacter
pattern CyrillicLetterHardSign = CombiningCyrillicLetterHardSign

-- | A pattern synonym for 'CombiningCyrillicLetterYeru', the name without the @Combining@ part, defined by @'\\xa679'@ (&#x2022;&#xa679;).
pattern CyrillicLetterYeru :: CombiningCharacter
pattern CyrillicLetterYeru = CombiningCyrillicLetterYeru

-- | A pattern synonym for 'CombiningCyrillicLetterSoftSign', the name without the @Combining@ part, defined by @'\\xa67a'@ (&#x2022;&#xa67a;).
pattern CyrillicLetterSoftSign :: CombiningCharacter
pattern CyrillicLetterSoftSign = CombiningCyrillicLetterSoftSign

-- | A pattern synonym for 'CombiningCyrillicLetterOmega', the name without the @Combining@ part, defined by @'\\xa67b'@ (&#x2022;&#xa67b;).
pattern CyrillicLetterOmega :: CombiningCharacter
pattern CyrillicLetterOmega = CombiningCyrillicLetterOmega

-- | A pattern synonym for 'CombiningCyrillicKavyka', the name without the @Combining@ part, defined by @'\\xa67c'@ (&#x2022;&#xa67c;).
pattern CyrillicKavyka :: CombiningCharacter
pattern CyrillicKavyka = CombiningCyrillicKavyka

-- | A pattern synonym for 'CombiningCyrillicPayerok', the name without the @Combining@ part, defined by @'\\xa67d'@ (&#x2022;&#xa67d;).
pattern CyrillicPayerok :: CombiningCharacter
pattern CyrillicPayerok = CombiningCyrillicPayerok

-- | A pattern synonym for 'CombiningCyrillicLetterEf', the name without the @Combining@ part, defined by @'\\xa69e'@ (&#x2022;&#xa69e;).
pattern CyrillicLetterEf :: CombiningCharacter
pattern CyrillicLetterEf = CombiningCyrillicLetterEf

-- | A pattern synonym for 'CombiningCyrillicLetterIotifiedE', the name without the @Combining@ part, defined by @'\\xa69f'@ (&#x2022;&#xa69f;).
pattern CyrillicLetterIotifiedE :: CombiningCharacter
pattern CyrillicLetterIotifiedE = CombiningCyrillicLetterIotifiedE

-- | A pattern synonym for 'BamumCombiningMarkKoqndon', the name without the @Combining@ part, defined by @'\\xa6f0'@ (&#x2022;&#xa6f0;).
pattern BamumMarkKoqndon :: CombiningCharacter
pattern BamumMarkKoqndon = BamumCombiningMarkKoqndon

-- | A pattern synonym for 'BamumCombiningMarkTukwentis', the name without the @Combining@ part, defined by @'\\xa6f1'@ (&#x2022;&#xa6f1;).
pattern BamumMarkTukwentis :: CombiningCharacter
pattern BamumMarkTukwentis = BamumCombiningMarkTukwentis

-- | A pattern synonym for 'CombiningDevanagariDigitZero', the name without the @Combining@ part, defined by @'\\xa8e0'@ (&#x2022;&#xa8e0;).
pattern DevanagariDigitZero :: CombiningCharacter
pattern DevanagariDigitZero = CombiningDevanagariDigitZero

-- | A pattern synonym for 'CombiningDevanagariDigitOne', the name without the @Combining@ part, defined by @'\\xa8e1'@ (&#x2022;&#xa8e1;).
pattern DevanagariDigitOne :: CombiningCharacter
pattern DevanagariDigitOne = CombiningDevanagariDigitOne

-- | A pattern synonym for 'CombiningDevanagariDigitTwo', the name without the @Combining@ part, defined by @'\\xa8e2'@ (&#x2022;&#xa8e2;).
pattern DevanagariDigitTwo :: CombiningCharacter
pattern DevanagariDigitTwo = CombiningDevanagariDigitTwo

-- | A pattern synonym for 'CombiningDevanagariDigitThree', the name without the @Combining@ part, defined by @'\\xa8e3'@ (&#x2022;&#xa8e3;).
pattern DevanagariDigitThree :: CombiningCharacter
pattern DevanagariDigitThree = CombiningDevanagariDigitThree

-- | A pattern synonym for 'CombiningDevanagariDigitFour', the name without the @Combining@ part, defined by @'\\xa8e4'@ (&#x2022;&#xa8e4;).
pattern DevanagariDigitFour :: CombiningCharacter
pattern DevanagariDigitFour = CombiningDevanagariDigitFour

-- | A pattern synonym for 'CombiningDevanagariDigitFive', the name without the @Combining@ part, defined by @'\\xa8e5'@ (&#x2022;&#xa8e5;).
pattern DevanagariDigitFive :: CombiningCharacter
pattern DevanagariDigitFive = CombiningDevanagariDigitFive

-- | A pattern synonym for 'CombiningDevanagariDigitSix', the name without the @Combining@ part, defined by @'\\xa8e6'@ (&#x2022;&#xa8e6;).
pattern DevanagariDigitSix :: CombiningCharacter
pattern DevanagariDigitSix = CombiningDevanagariDigitSix

-- | A pattern synonym for 'CombiningDevanagariDigitSeven', the name without the @Combining@ part, defined by @'\\xa8e7'@ (&#x2022;&#xa8e7;).
pattern DevanagariDigitSeven :: CombiningCharacter
pattern DevanagariDigitSeven = CombiningDevanagariDigitSeven

-- | A pattern synonym for 'CombiningDevanagariDigitEight', the name without the @Combining@ part, defined by @'\\xa8e8'@ (&#x2022;&#xa8e8;).
pattern DevanagariDigitEight :: CombiningCharacter
pattern DevanagariDigitEight = CombiningDevanagariDigitEight

-- | A pattern synonym for 'CombiningDevanagariDigitNine', the name without the @Combining@ part, defined by @'\\xa8e9'@ (&#x2022;&#xa8e9;).
pattern DevanagariDigitNine :: CombiningCharacter
pattern DevanagariDigitNine = CombiningDevanagariDigitNine

-- | A pattern synonym for 'CombiningDevanagariLetterA', the name without the @Combining@ part, defined by @'\\xa8ea'@ (&#x2022;&#xa8ea;).
pattern DevanagariLetterA :: CombiningCharacter
pattern DevanagariLetterA = CombiningDevanagariLetterA

-- | A pattern synonym for 'CombiningDevanagariLetterU', the name without the @Combining@ part, defined by @'\\xa8eb'@ (&#x2022;&#xa8eb;).
pattern DevanagariLetterU :: CombiningCharacter
pattern DevanagariLetterU = CombiningDevanagariLetterU

-- | A pattern synonym for 'CombiningDevanagariLetterKa', the name without the @Combining@ part, defined by @'\\xa8ec'@ (&#x2022;&#xa8ec;).
pattern DevanagariLetterKa :: CombiningCharacter
pattern DevanagariLetterKa = CombiningDevanagariLetterKa

-- | A pattern synonym for 'CombiningDevanagariLetterNa', the name without the @Combining@ part, defined by @'\\xa8ed'@ (&#x2022;&#xa8ed;).
pattern DevanagariLetterNa :: CombiningCharacter
pattern DevanagariLetterNa = CombiningDevanagariLetterNa

-- | A pattern synonym for 'CombiningDevanagariLetterPa', the name without the @Combining@ part, defined by @'\\xa8ee'@ (&#x2022;&#xa8ee;).
pattern DevanagariLetterPa :: CombiningCharacter
pattern DevanagariLetterPa = CombiningDevanagariLetterPa

-- | A pattern synonym for 'CombiningDevanagariLetterRa', the name without the @Combining@ part, defined by @'\\xa8ef'@ (&#x2022;&#xa8ef;).
pattern DevanagariLetterRa :: CombiningCharacter
pattern DevanagariLetterRa = CombiningDevanagariLetterRa

-- | A pattern synonym for 'CombiningDevanagariLetterVi', the name without the @Combining@ part, defined by @'\\xa8f0'@ (&#x2022;&#xa8f0;).
pattern DevanagariLetterVi :: CombiningCharacter
pattern DevanagariLetterVi = CombiningDevanagariLetterVi

-- | A pattern synonym for 'CombiningDevanagariSignAvagraha', the name without the @Combining@ part, defined by @'\\xa8f1'@ (&#x2022;&#xa8f1;).
pattern DevanagariSignAvagraha :: CombiningCharacter
pattern DevanagariSignAvagraha = CombiningDevanagariSignAvagraha

-- | A pattern synonym for 'CombiningLigatureLeftHalf', the name without the @Combining@ part, defined by @'\\xfe20'@ (&#x2022;&#xfe20;).
pattern LigatureLeftHalf :: CombiningCharacter
pattern LigatureLeftHalf = CombiningLigatureLeftHalf

-- | A pattern synonym for 'CombiningLigatureRightHalf', the name without the @Combining@ part, defined by @'\\xfe21'@ (&#x2022;&#xfe21;).
pattern LigatureRightHalf :: CombiningCharacter
pattern LigatureRightHalf = CombiningLigatureRightHalf

-- | A pattern synonym for 'CombiningDoubleTildeLeftHalf', the name without the @Combining@ part, defined by @'\\xfe22'@ (&#x2022;&#xfe22;).
pattern DoubleTildeLeftHalf :: CombiningCharacter
pattern DoubleTildeLeftHalf = CombiningDoubleTildeLeftHalf

-- | A pattern synonym for 'CombiningDoubleTildeRightHalf', the name without the @Combining@ part, defined by @'\\xfe23'@ (&#x2022;&#xfe23;).
pattern DoubleTildeRightHalf :: CombiningCharacter
pattern DoubleTildeRightHalf = CombiningDoubleTildeRightHalf

-- | A pattern synonym for 'CombiningMacronLeftHalf', the name without the @Combining@ part, defined by @'\\xfe24'@ (&#x2022;&#xfe24;).
pattern MacronLeftHalf :: CombiningCharacter
pattern MacronLeftHalf = CombiningMacronLeftHalf

-- | A pattern synonym for 'CombiningMacronRightHalf', the name without the @Combining@ part, defined by @'\\xfe25'@ (&#x2022;&#xfe25;).
pattern MacronRightHalf :: CombiningCharacter
pattern MacronRightHalf = CombiningMacronRightHalf

-- | A pattern synonym for 'CombiningConjoiningMacron', the name without the @Combining@ part, defined by @'\\xfe26'@ (&#x2022;&#xfe26;).
pattern ConjoiningMacron :: CombiningCharacter
pattern ConjoiningMacron = CombiningConjoiningMacron

-- | A pattern synonym for 'CombiningLigatureLeftHalfBelow', the name without the @Combining@ part, defined by @'\\xfe27'@ (&#x2022;&#xfe27;).
pattern LigatureLeftHalfBelow :: CombiningCharacter
pattern LigatureLeftHalfBelow = CombiningLigatureLeftHalfBelow

-- | A pattern synonym for 'CombiningLigatureRightHalfBelow', the name without the @Combining@ part, defined by @'\\xfe28'@ (&#x2022;&#xfe28;).
pattern LigatureRightHalfBelow :: CombiningCharacter
pattern LigatureRightHalfBelow = CombiningLigatureRightHalfBelow

-- | A pattern synonym for 'CombiningTildeLeftHalfBelow', the name without the @Combining@ part, defined by @'\\xfe29'@ (&#x2022;&#xfe29;).
pattern TildeLeftHalfBelow :: CombiningCharacter
pattern TildeLeftHalfBelow = CombiningTildeLeftHalfBelow

-- | A pattern synonym for 'CombiningTildeRightHalfBelow', the name without the @Combining@ part, defined by @'\\xfe2a'@ (&#x2022;&#xfe2a;).
pattern TildeRightHalfBelow :: CombiningCharacter
pattern TildeRightHalfBelow = CombiningTildeRightHalfBelow

-- | A pattern synonym for 'CombiningMacronLeftHalfBelow', the name without the @Combining@ part, defined by @'\\xfe2b'@ (&#x2022;&#xfe2b;).
pattern MacronLeftHalfBelow :: CombiningCharacter
pattern MacronLeftHalfBelow = CombiningMacronLeftHalfBelow

-- | A pattern synonym for 'CombiningMacronRightHalfBelow', the name without the @Combining@ part, defined by @'\\xfe2c'@ (&#x2022;&#xfe2c;).
pattern MacronRightHalfBelow :: CombiningCharacter
pattern MacronRightHalfBelow = CombiningMacronRightHalfBelow

-- | A pattern synonym for 'CombiningConjoiningMacronBelow', the name without the @Combining@ part, defined by @'\\xfe2d'@ (&#x2022;&#xfe2d;).
pattern ConjoiningMacronBelow :: CombiningCharacter
pattern ConjoiningMacronBelow = CombiningConjoiningMacronBelow

-- | A pattern synonym for 'CombiningCyrillicTitloLeftHalf', the name without the @Combining@ part, defined by @'\\xfe2e'@ (&#x2022;&#xfe2e;).
pattern CyrillicTitloLeftHalf :: CombiningCharacter
pattern CyrillicTitloLeftHalf = CombiningCyrillicTitloLeftHalf

-- | A pattern synonym for 'CombiningCyrillicTitloRightHalf', the name without the @Combining@ part, defined by @'\\xfe2f'@ (&#x2022;&#xfe2f;).
pattern CyrillicTitloRightHalf :: CombiningCharacter
pattern CyrillicTitloRightHalf = CombiningCyrillicTitloRightHalf

-- | A pattern synonym for 'PhaistosDiscSignCombiningObliqueStroke', the name without the @Combining@ part, defined by @'\\x101fd'@ (&#x2022;&#x101fd;).
pattern PhaistosDiscSignObliqueStroke :: CombiningCharacter
pattern PhaistosDiscSignObliqueStroke = PhaistosDiscSignCombiningObliqueStroke

-- | A pattern synonym for 'CombiningOldPermicLetterAn', the name without the @Combining@ part, defined by @'\\x10376'@ (&#x2022;&#x10376;).
pattern OldPermicLetterAn :: CombiningCharacter
pattern OldPermicLetterAn = CombiningOldPermicLetterAn

-- | A pattern synonym for 'CombiningOldPermicLetterDoi', the name without the @Combining@ part, defined by @'\\x10377'@ (&#x2022;&#x10377;).
pattern OldPermicLetterDoi :: CombiningCharacter
pattern OldPermicLetterDoi = CombiningOldPermicLetterDoi

-- | A pattern synonym for 'CombiningOldPermicLetterZata', the name without the @Combining@ part, defined by @'\\x10378'@ (&#x2022;&#x10378;).
pattern OldPermicLetterZata :: CombiningCharacter
pattern OldPermicLetterZata = CombiningOldPermicLetterZata

-- | A pattern synonym for 'CombiningOldPermicLetterNenoe', the name without the @Combining@ part, defined by @'\\x10379'@ (&#x2022;&#x10379;).
pattern OldPermicLetterNenoe :: CombiningCharacter
pattern OldPermicLetterNenoe = CombiningOldPermicLetterNenoe

-- | A pattern synonym for 'CombiningOldPermicLetterSii', the name without the @Combining@ part, defined by @'\\x1037a'@ (&#x2022;&#x1037a;).
pattern OldPermicLetterSii :: CombiningCharacter
pattern OldPermicLetterSii = CombiningOldPermicLetterSii

-- | A pattern synonym for 'CombiningGranthaDigitZero', the name without the @Combining@ part, defined by @'\\x11366'@ (&#x2022;&#x11366;).
pattern GranthaDigitZero :: CombiningCharacter
pattern GranthaDigitZero = CombiningGranthaDigitZero

-- | A pattern synonym for 'CombiningGranthaDigitOne', the name without the @Combining@ part, defined by @'\\x11367'@ (&#x2022;&#x11367;).
pattern GranthaDigitOne :: CombiningCharacter
pattern GranthaDigitOne = CombiningGranthaDigitOne

-- | A pattern synonym for 'CombiningGranthaDigitTwo', the name without the @Combining@ part, defined by @'\\x11368'@ (&#x2022;&#x11368;).
pattern GranthaDigitTwo :: CombiningCharacter
pattern GranthaDigitTwo = CombiningGranthaDigitTwo

-- | A pattern synonym for 'CombiningGranthaDigitThree', the name without the @Combining@ part, defined by @'\\x11369'@ (&#x2022;&#x11369;).
pattern GranthaDigitThree :: CombiningCharacter
pattern GranthaDigitThree = CombiningGranthaDigitThree

-- | A pattern synonym for 'CombiningGranthaDigitFour', the name without the @Combining@ part, defined by @'\\x1136a'@ (&#x2022;&#x1136a;).
pattern GranthaDigitFour :: CombiningCharacter
pattern GranthaDigitFour = CombiningGranthaDigitFour

-- | A pattern synonym for 'CombiningGranthaDigitFive', the name without the @Combining@ part, defined by @'\\x1136b'@ (&#x2022;&#x1136b;).
pattern GranthaDigitFive :: CombiningCharacter
pattern GranthaDigitFive = CombiningGranthaDigitFive

-- | A pattern synonym for 'CombiningGranthaDigitSix', the name without the @Combining@ part, defined by @'\\x1136c'@ (&#x2022;&#x1136c;).
pattern GranthaDigitSix :: CombiningCharacter
pattern GranthaDigitSix = CombiningGranthaDigitSix

-- | A pattern synonym for 'CombiningGranthaLetterA', the name without the @Combining@ part, defined by @'\\x11370'@ (&#x2022;&#x11370;).
pattern GranthaLetterA :: CombiningCharacter
pattern GranthaLetterA = CombiningGranthaLetterA

-- | A pattern synonym for 'CombiningGranthaLetterKa', the name without the @Combining@ part, defined by @'\\x11371'@ (&#x2022;&#x11371;).
pattern GranthaLetterKa :: CombiningCharacter
pattern GranthaLetterKa = CombiningGranthaLetterKa

-- | A pattern synonym for 'CombiningGranthaLetterNa', the name without the @Combining@ part, defined by @'\\x11372'@ (&#x2022;&#x11372;).
pattern GranthaLetterNa :: CombiningCharacter
pattern GranthaLetterNa = CombiningGranthaLetterNa

-- | A pattern synonym for 'CombiningGranthaLetterVi', the name without the @Combining@ part, defined by @'\\x11373'@ (&#x2022;&#x11373;).
pattern GranthaLetterVi :: CombiningCharacter
pattern GranthaLetterVi = CombiningGranthaLetterVi

-- | A pattern synonym for 'CombiningGranthaLetterPa', the name without the @Combining@ part, defined by @'\\x11374'@ (&#x2022;&#x11374;).
pattern GranthaLetterPa :: CombiningCharacter
pattern GranthaLetterPa = CombiningGranthaLetterPa

-- | A pattern synonym for 'BassaVahCombiningHighTone', the name without the @Combining@ part, defined by @'\\x16af0'@ (&#x2022;&#x16af0;).
pattern BassaVahHighTone :: CombiningCharacter
pattern BassaVahHighTone = BassaVahCombiningHighTone

-- | A pattern synonym for 'BassaVahCombiningLowTone', the name without the @Combining@ part, defined by @'\\x16af1'@ (&#x2022;&#x16af1;).
pattern BassaVahLowTone :: CombiningCharacter
pattern BassaVahLowTone = BassaVahCombiningLowTone

-- | A pattern synonym for 'BassaVahCombiningMidTone', the name without the @Combining@ part, defined by @'\\x16af2'@ (&#x2022;&#x16af2;).
pattern BassaVahMidTone :: CombiningCharacter
pattern BassaVahMidTone = BassaVahCombiningMidTone

-- | A pattern synonym for 'BassaVahCombiningLowMidTone', the name without the @Combining@ part, defined by @'\\x16af3'@ (&#x2022;&#x16af3;).
pattern BassaVahLowMidTone :: CombiningCharacter
pattern BassaVahLowMidTone = BassaVahCombiningLowMidTone

-- | A pattern synonym for 'BassaVahCombiningHighLowTone', the name without the @Combining@ part, defined by @'\\x16af4'@ (&#x2022;&#x16af4;).
pattern BassaVahHighLowTone :: CombiningCharacter
pattern BassaVahHighLowTone = BassaVahCombiningHighLowTone

-- | A pattern synonym for 'MusicalSymbolCombiningStem', the name without the @Combining@ part, defined by @'\\x1d165'@ (&#x2022;&#x1d165;).
pattern MusicalSymbolStem :: CombiningCharacter
pattern MusicalSymbolStem = MusicalSymbolCombiningStem

-- | A pattern synonym for 'MusicalSymbolCombiningSprechgesangStem', the name without the @Combining@ part, defined by @'\\x1d166'@ (&#x2022;&#x1d166;).
pattern MusicalSymbolSprechgesangStem :: CombiningCharacter
pattern MusicalSymbolSprechgesangStem = MusicalSymbolCombiningSprechgesangStem

-- | A pattern synonym for 'MusicalSymbolCombiningTremolo1', the name without the @Combining@ part, defined by @'\\x1d167'@ (&#x2022;&#x1d167;).
pattern MusicalSymbolTremolo1 :: CombiningCharacter
pattern MusicalSymbolTremolo1 = MusicalSymbolCombiningTremolo1

-- | A pattern synonym for 'MusicalSymbolCombiningTremolo2', the name without the @Combining@ part, defined by @'\\x1d168'@ (&#x2022;&#x1d168;).
pattern MusicalSymbolTremolo2 :: CombiningCharacter
pattern MusicalSymbolTremolo2 = MusicalSymbolCombiningTremolo2

-- | A pattern synonym for 'MusicalSymbolCombiningTremolo3', the name without the @Combining@ part, defined by @'\\x1d169'@ (&#x2022;&#x1d169;).
pattern MusicalSymbolTremolo3 :: CombiningCharacter
pattern MusicalSymbolTremolo3 = MusicalSymbolCombiningTremolo3

-- | A pattern synonym for 'MusicalSymbolCombiningAugmentationDot', the name without the @Combining@ part, defined by @'\\x1d16d'@ (&#x2022;&#x1d16d;).
pattern MusicalSymbolAugmentationDot :: CombiningCharacter
pattern MusicalSymbolAugmentationDot = MusicalSymbolCombiningAugmentationDot

-- | A pattern synonym for 'MusicalSymbolCombiningFlag1', the name without the @Combining@ part, defined by @'\\x1d16e'@ (&#x2022;&#x1d16e;).
pattern MusicalSymbolFlag1 :: CombiningCharacter
pattern MusicalSymbolFlag1 = MusicalSymbolCombiningFlag1

-- | A pattern synonym for 'MusicalSymbolCombiningFlag2', the name without the @Combining@ part, defined by @'\\x1d16f'@ (&#x2022;&#x1d16f;).
pattern MusicalSymbolFlag2 :: CombiningCharacter
pattern MusicalSymbolFlag2 = MusicalSymbolCombiningFlag2

-- | A pattern synonym for 'MusicalSymbolCombiningFlag3', the name without the @Combining@ part, defined by @'\\x1d170'@ (&#x2022;&#x1d170;).
pattern MusicalSymbolFlag3 :: CombiningCharacter
pattern MusicalSymbolFlag3 = MusicalSymbolCombiningFlag3

-- | A pattern synonym for 'MusicalSymbolCombiningFlag4', the name without the @Combining@ part, defined by @'\\x1d171'@ (&#x2022;&#x1d171;).
pattern MusicalSymbolFlag4 :: CombiningCharacter
pattern MusicalSymbolFlag4 = MusicalSymbolCombiningFlag4

-- | A pattern synonym for 'MusicalSymbolCombiningFlag5', the name without the @Combining@ part, defined by @'\\x1d172'@ (&#x2022;&#x1d172;).
pattern MusicalSymbolFlag5 :: CombiningCharacter
pattern MusicalSymbolFlag5 = MusicalSymbolCombiningFlag5

-- | A pattern synonym for 'MusicalSymbolCombiningAccent', the name without the @Combining@ part, defined by @'\\x1d17b'@ (&#x2022;&#x1d17b;).
pattern MusicalSymbolAccent :: CombiningCharacter
pattern MusicalSymbolAccent = MusicalSymbolCombiningAccent

-- | A pattern synonym for 'MusicalSymbolCombiningStaccato', the name without the @Combining@ part, defined by @'\\x1d17c'@ (&#x2022;&#x1d17c;).
pattern MusicalSymbolStaccato :: CombiningCharacter
pattern MusicalSymbolStaccato = MusicalSymbolCombiningStaccato

-- | A pattern synonym for 'MusicalSymbolCombiningTenuto', the name without the @Combining@ part, defined by @'\\x1d17d'@ (&#x2022;&#x1d17d;).
pattern MusicalSymbolTenuto :: CombiningCharacter
pattern MusicalSymbolTenuto = MusicalSymbolCombiningTenuto

-- | A pattern synonym for 'MusicalSymbolCombiningStaccatissimo', the name without the @Combining@ part, defined by @'\\x1d17e'@ (&#x2022;&#x1d17e;).
pattern MusicalSymbolStaccatissimo :: CombiningCharacter
pattern MusicalSymbolStaccatissimo = MusicalSymbolCombiningStaccatissimo

-- | A pattern synonym for 'MusicalSymbolCombiningMarcato', the name without the @Combining@ part, defined by @'\\x1d17f'@ (&#x2022;&#x1d17f;).
pattern MusicalSymbolMarcato :: CombiningCharacter
pattern MusicalSymbolMarcato = MusicalSymbolCombiningMarcato

-- | A pattern synonym for 'MusicalSymbolCombiningMarcatoStaccato', the name without the @Combining@ part, defined by @'\\x1d180'@ (&#x2022;&#x1d180;).
pattern MusicalSymbolMarcatoStaccato :: CombiningCharacter
pattern MusicalSymbolMarcatoStaccato = MusicalSymbolCombiningMarcatoStaccato

-- | A pattern synonym for 'MusicalSymbolCombiningAccentStaccato', the name without the @Combining@ part, defined by @'\\x1d181'@ (&#x2022;&#x1d181;).
pattern MusicalSymbolAccentStaccato :: CombiningCharacter
pattern MusicalSymbolAccentStaccato = MusicalSymbolCombiningAccentStaccato

-- | A pattern synonym for 'MusicalSymbolCombiningLoure', the name without the @Combining@ part, defined by @'\\x1d182'@ (&#x2022;&#x1d182;).
pattern MusicalSymbolLoure :: CombiningCharacter
pattern MusicalSymbolLoure = MusicalSymbolCombiningLoure

-- | A pattern synonym for 'MusicalSymbolCombiningDoit', the name without the @Combining@ part, defined by @'\\x1d185'@ (&#x2022;&#x1d185;).
pattern MusicalSymbolDoit :: CombiningCharacter
pattern MusicalSymbolDoit = MusicalSymbolCombiningDoit

-- | A pattern synonym for 'MusicalSymbolCombiningRip', the name without the @Combining@ part, defined by @'\\x1d186'@ (&#x2022;&#x1d186;).
pattern MusicalSymbolRip :: CombiningCharacter
pattern MusicalSymbolRip = MusicalSymbolCombiningRip

-- | A pattern synonym for 'MusicalSymbolCombiningFlip', the name without the @Combining@ part, defined by @'\\x1d187'@ (&#x2022;&#x1d187;).
pattern MusicalSymbolFlip :: CombiningCharacter
pattern MusicalSymbolFlip = MusicalSymbolCombiningFlip

-- | A pattern synonym for 'MusicalSymbolCombiningSmear', the name without the @Combining@ part, defined by @'\\x1d188'@ (&#x2022;&#x1d188;).
pattern MusicalSymbolSmear :: CombiningCharacter
pattern MusicalSymbolSmear = MusicalSymbolCombiningSmear

-- | A pattern synonym for 'MusicalSymbolCombiningBend', the name without the @Combining@ part, defined by @'\\x1d189'@ (&#x2022;&#x1d189;).
pattern MusicalSymbolBend :: CombiningCharacter
pattern MusicalSymbolBend = MusicalSymbolCombiningBend

-- | A pattern synonym for 'MusicalSymbolCombiningDoubleTongue', the name without the @Combining@ part, defined by @'\\x1d18a'@ (&#x2022;&#x1d18a;).
pattern MusicalSymbolDoubleTongue :: CombiningCharacter
pattern MusicalSymbolDoubleTongue = MusicalSymbolCombiningDoubleTongue

-- | A pattern synonym for 'MusicalSymbolCombiningTripleTongue', the name without the @Combining@ part, defined by @'\\x1d18b'@ (&#x2022;&#x1d18b;).
pattern MusicalSymbolTripleTongue :: CombiningCharacter
pattern MusicalSymbolTripleTongue = MusicalSymbolCombiningTripleTongue

-- | A pattern synonym for 'MusicalSymbolCombiningDownBow', the name without the @Combining@ part, defined by @'\\x1d1aa'@ (&#x2022;&#x1d1aa;).
pattern MusicalSymbolDownBow :: CombiningCharacter
pattern MusicalSymbolDownBow = MusicalSymbolCombiningDownBow

-- | A pattern synonym for 'MusicalSymbolCombiningUpBow', the name without the @Combining@ part, defined by @'\\x1d1ab'@ (&#x2022;&#x1d1ab;).
pattern MusicalSymbolUpBow :: CombiningCharacter
pattern MusicalSymbolUpBow = MusicalSymbolCombiningUpBow

-- | A pattern synonym for 'MusicalSymbolCombiningHarmonic', the name without the @Combining@ part, defined by @'\\x1d1ac'@ (&#x2022;&#x1d1ac;).
pattern MusicalSymbolHarmonic :: CombiningCharacter
pattern MusicalSymbolHarmonic = MusicalSymbolCombiningHarmonic

-- | A pattern synonym for 'MusicalSymbolCombiningSnapPizzicato', the name without the @Combining@ part, defined by @'\\x1d1ad'@ (&#x2022;&#x1d1ad;).
pattern MusicalSymbolSnapPizzicato :: CombiningCharacter
pattern MusicalSymbolSnapPizzicato = MusicalSymbolCombiningSnapPizzicato

-- | A pattern synonym for 'CombiningGreekMusicalTriseme', the name without the @Combining@ part, defined by @'\\x1d242'@ (&#x2022;&#x1d242;).
pattern GreekMusicalTriseme :: CombiningCharacter
pattern GreekMusicalTriseme = CombiningGreekMusicalTriseme

-- | A pattern synonym for 'CombiningGreekMusicalTetraseme', the name without the @Combining@ part, defined by @'\\x1d243'@ (&#x2022;&#x1d243;).
pattern GreekMusicalTetraseme :: CombiningCharacter
pattern GreekMusicalTetraseme = CombiningGreekMusicalTetraseme

-- | A pattern synonym for 'CombiningGreekMusicalPentaseme', the name without the @Combining@ part, defined by @'\\x1d244'@ (&#x2022;&#x1d244;).
pattern GreekMusicalPentaseme :: CombiningCharacter
pattern GreekMusicalPentaseme = CombiningGreekMusicalPentaseme

-- | A pattern synonym for 'CombiningGlagoliticLetterAzu', the name without the @Combining@ part, defined by @'\\x1e000'@ (&#x2022;&#x1e000;).
pattern GlagoliticLetterAzu :: CombiningCharacter
pattern GlagoliticLetterAzu = CombiningGlagoliticLetterAzu

-- | A pattern synonym for 'CombiningGlagoliticLetterBuky', the name without the @Combining@ part, defined by @'\\x1e001'@ (&#x2022;&#x1e001;).
pattern GlagoliticLetterBuky :: CombiningCharacter
pattern GlagoliticLetterBuky = CombiningGlagoliticLetterBuky

-- | A pattern synonym for 'CombiningGlagoliticLetterVede', the name without the @Combining@ part, defined by @'\\x1e002'@ (&#x2022;&#x1e002;).
pattern GlagoliticLetterVede :: CombiningCharacter
pattern GlagoliticLetterVede = CombiningGlagoliticLetterVede

-- | A pattern synonym for 'CombiningGlagoliticLetterGlagoli', the name without the @Combining@ part, defined by @'\\x1e003'@ (&#x2022;&#x1e003;).
pattern GlagoliticLetterGlagoli :: CombiningCharacter
pattern GlagoliticLetterGlagoli = CombiningGlagoliticLetterGlagoli

-- | A pattern synonym for 'CombiningGlagoliticLetterDobro', the name without the @Combining@ part, defined by @'\\x1e004'@ (&#x2022;&#x1e004;).
pattern GlagoliticLetterDobro :: CombiningCharacter
pattern GlagoliticLetterDobro = CombiningGlagoliticLetterDobro

-- | A pattern synonym for 'CombiningGlagoliticLetterYestu', the name without the @Combining@ part, defined by @'\\x1e005'@ (&#x2022;&#x1e005;).
pattern GlagoliticLetterYestu :: CombiningCharacter
pattern GlagoliticLetterYestu = CombiningGlagoliticLetterYestu

-- | A pattern synonym for 'CombiningGlagoliticLetterZhivete', the name without the @Combining@ part, defined by @'\\x1e006'@ (&#x2022;&#x1e006;).
pattern GlagoliticLetterZhivete :: CombiningCharacter
pattern GlagoliticLetterZhivete = CombiningGlagoliticLetterZhivete

-- | A pattern synonym for 'CombiningGlagoliticLetterZemlja', the name without the @Combining@ part, defined by @'\\x1e008'@ (&#x2022;&#x1e008;).
pattern GlagoliticLetterZemlja :: CombiningCharacter
pattern GlagoliticLetterZemlja = CombiningGlagoliticLetterZemlja

-- | A pattern synonym for 'CombiningGlagoliticLetterIzhe', the name without the @Combining@ part, defined by @'\\x1e009'@ (&#x2022;&#x1e009;).
pattern GlagoliticLetterIzhe :: CombiningCharacter
pattern GlagoliticLetterIzhe = CombiningGlagoliticLetterIzhe

-- | A pattern synonym for 'CombiningGlagoliticLetterInitialIzhe', the name without the @Combining@ part, defined by @'\\x1e00a'@ (&#x2022;&#x1e00a;).
pattern GlagoliticLetterInitialIzhe :: CombiningCharacter
pattern GlagoliticLetterInitialIzhe = CombiningGlagoliticLetterInitialIzhe

-- | A pattern synonym for 'CombiningGlagoliticLetterI', the name without the @Combining@ part, defined by @'\\x1e00b'@ (&#x2022;&#x1e00b;).
pattern GlagoliticLetterI :: CombiningCharacter
pattern GlagoliticLetterI = CombiningGlagoliticLetterI

-- | A pattern synonym for 'CombiningGlagoliticLetterDjervi', the name without the @Combining@ part, defined by @'\\x1e00c'@ (&#x2022;&#x1e00c;).
pattern GlagoliticLetterDjervi :: CombiningCharacter
pattern GlagoliticLetterDjervi = CombiningGlagoliticLetterDjervi

-- | A pattern synonym for 'CombiningGlagoliticLetterKako', the name without the @Combining@ part, defined by @'\\x1e00d'@ (&#x2022;&#x1e00d;).
pattern GlagoliticLetterKako :: CombiningCharacter
pattern GlagoliticLetterKako = CombiningGlagoliticLetterKako

-- | A pattern synonym for 'CombiningGlagoliticLetterLjudije', the name without the @Combining@ part, defined by @'\\x1e00e'@ (&#x2022;&#x1e00e;).
pattern GlagoliticLetterLjudije :: CombiningCharacter
pattern GlagoliticLetterLjudije = CombiningGlagoliticLetterLjudije

-- | A pattern synonym for 'CombiningGlagoliticLetterMyslite', the name without the @Combining@ part, defined by @'\\x1e00f'@ (&#x2022;&#x1e00f;).
pattern GlagoliticLetterMyslite :: CombiningCharacter
pattern GlagoliticLetterMyslite = CombiningGlagoliticLetterMyslite

-- | A pattern synonym for 'CombiningGlagoliticLetterNashi', the name without the @Combining@ part, defined by @'\\x1e010'@ (&#x2022;&#x1e010;).
pattern GlagoliticLetterNashi :: CombiningCharacter
pattern GlagoliticLetterNashi = CombiningGlagoliticLetterNashi

-- | A pattern synonym for 'CombiningGlagoliticLetterOnu', the name without the @Combining@ part, defined by @'\\x1e011'@ (&#x2022;&#x1e011;).
pattern GlagoliticLetterOnu :: CombiningCharacter
pattern GlagoliticLetterOnu = CombiningGlagoliticLetterOnu

-- | A pattern synonym for 'CombiningGlagoliticLetterPokoji', the name without the @Combining@ part, defined by @'\\x1e012'@ (&#x2022;&#x1e012;).
pattern GlagoliticLetterPokoji :: CombiningCharacter
pattern GlagoliticLetterPokoji = CombiningGlagoliticLetterPokoji

-- | A pattern synonym for 'CombiningGlagoliticLetterRitsi', the name without the @Combining@ part, defined by @'\\x1e013'@ (&#x2022;&#x1e013;).
pattern GlagoliticLetterRitsi :: CombiningCharacter
pattern GlagoliticLetterRitsi = CombiningGlagoliticLetterRitsi

-- | A pattern synonym for 'CombiningGlagoliticLetterSlovo', the name without the @Combining@ part, defined by @'\\x1e014'@ (&#x2022;&#x1e014;).
pattern GlagoliticLetterSlovo :: CombiningCharacter
pattern GlagoliticLetterSlovo = CombiningGlagoliticLetterSlovo

-- | A pattern synonym for 'CombiningGlagoliticLetterTvrido', the name without the @Combining@ part, defined by @'\\x1e015'@ (&#x2022;&#x1e015;).
pattern GlagoliticLetterTvrido :: CombiningCharacter
pattern GlagoliticLetterTvrido = CombiningGlagoliticLetterTvrido

-- | A pattern synonym for 'CombiningGlagoliticLetterUku', the name without the @Combining@ part, defined by @'\\x1e016'@ (&#x2022;&#x1e016;).
pattern GlagoliticLetterUku :: CombiningCharacter
pattern GlagoliticLetterUku = CombiningGlagoliticLetterUku

-- | A pattern synonym for 'CombiningGlagoliticLetterFritu', the name without the @Combining@ part, defined by @'\\x1e017'@ (&#x2022;&#x1e017;).
pattern GlagoliticLetterFritu :: CombiningCharacter
pattern GlagoliticLetterFritu = CombiningGlagoliticLetterFritu

-- | A pattern synonym for 'CombiningGlagoliticLetterHeru', the name without the @Combining@ part, defined by @'\\x1e018'@ (&#x2022;&#x1e018;).
pattern GlagoliticLetterHeru :: CombiningCharacter
pattern GlagoliticLetterHeru = CombiningGlagoliticLetterHeru

-- | A pattern synonym for 'CombiningGlagoliticLetterShta', the name without the @Combining@ part, defined by @'\\x1e01b'@ (&#x2022;&#x1e01b;).
pattern GlagoliticLetterShta :: CombiningCharacter
pattern GlagoliticLetterShta = CombiningGlagoliticLetterShta

-- | A pattern synonym for 'CombiningGlagoliticLetterTsi', the name without the @Combining@ part, defined by @'\\x1e01c'@ (&#x2022;&#x1e01c;).
pattern GlagoliticLetterTsi :: CombiningCharacter
pattern GlagoliticLetterTsi = CombiningGlagoliticLetterTsi

-- | A pattern synonym for 'CombiningGlagoliticLetterChrivi', the name without the @Combining@ part, defined by @'\\x1e01d'@ (&#x2022;&#x1e01d;).
pattern GlagoliticLetterChrivi :: CombiningCharacter
pattern GlagoliticLetterChrivi = CombiningGlagoliticLetterChrivi

-- | A pattern synonym for 'CombiningGlagoliticLetterSha', the name without the @Combining@ part, defined by @'\\x1e01e'@ (&#x2022;&#x1e01e;).
pattern GlagoliticLetterSha :: CombiningCharacter
pattern GlagoliticLetterSha = CombiningGlagoliticLetterSha

-- | A pattern synonym for 'CombiningGlagoliticLetterYeru', the name without the @Combining@ part, defined by @'\\x1e01f'@ (&#x2022;&#x1e01f;).
pattern GlagoliticLetterYeru :: CombiningCharacter
pattern GlagoliticLetterYeru = CombiningGlagoliticLetterYeru

-- | A pattern synonym for 'CombiningGlagoliticLetterYeri', the name without the @Combining@ part, defined by @'\\x1e020'@ (&#x2022;&#x1e020;).
pattern GlagoliticLetterYeri :: CombiningCharacter
pattern GlagoliticLetterYeri = CombiningGlagoliticLetterYeri

-- | A pattern synonym for 'CombiningGlagoliticLetterYati', the name without the @Combining@ part, defined by @'\\x1e021'@ (&#x2022;&#x1e021;).
pattern GlagoliticLetterYati :: CombiningCharacter
pattern GlagoliticLetterYati = CombiningGlagoliticLetterYati

-- | A pattern synonym for 'CombiningGlagoliticLetterYu', the name without the @Combining@ part, defined by @'\\x1e023'@ (&#x2022;&#x1e023;).
pattern GlagoliticLetterYu :: CombiningCharacter
pattern GlagoliticLetterYu = CombiningGlagoliticLetterYu

-- | A pattern synonym for 'CombiningGlagoliticLetterSmallYus', the name without the @Combining@ part, defined by @'\\x1e024'@ (&#x2022;&#x1e024;).
pattern GlagoliticLetterSmallYus :: CombiningCharacter
pattern GlagoliticLetterSmallYus = CombiningGlagoliticLetterSmallYus

-- | A pattern synonym for 'CombiningGlagoliticLetterYo', the name without the @Combining@ part, defined by @'\\x1e026'@ (&#x2022;&#x1e026;).
pattern GlagoliticLetterYo :: CombiningCharacter
pattern GlagoliticLetterYo = CombiningGlagoliticLetterYo

-- | A pattern synonym for 'CombiningGlagoliticLetterIotatedSmallYus', the name without the @Combining@ part, defined by @'\\x1e027'@ (&#x2022;&#x1e027;).
pattern GlagoliticLetterIotatedSmallYus :: CombiningCharacter
pattern GlagoliticLetterIotatedSmallYus = CombiningGlagoliticLetterIotatedSmallYus

-- | A pattern synonym for 'CombiningGlagoliticLetterBigYus', the name without the @Combining@ part, defined by @'\\x1e028'@ (&#x2022;&#x1e028;).
pattern GlagoliticLetterBigYus :: CombiningCharacter
pattern GlagoliticLetterBigYus = CombiningGlagoliticLetterBigYus

-- | A pattern synonym for 'CombiningGlagoliticLetterIotatedBigYus', the name without the @Combining@ part, defined by @'\\x1e029'@ (&#x2022;&#x1e029;).
pattern GlagoliticLetterIotatedBigYus :: CombiningCharacter
pattern GlagoliticLetterIotatedBigYus = CombiningGlagoliticLetterIotatedBigYus

-- | A pattern synonym for 'CombiningGlagoliticLetterFita', the name without the @Combining@ part, defined by @'\\x1e02a'@ (&#x2022;&#x1e02a;).
pattern GlagoliticLetterFita :: CombiningCharacter
pattern GlagoliticLetterFita = CombiningGlagoliticLetterFita

-- | A pattern synonym for 'MendeKikakuiCombiningNumberTeens', the name without the @Combining@ part, defined by @'\\x1e8d0'@ (&#x2022;&#x1e8d0;).
pattern MendeKikakuiNumberTeens :: CombiningCharacter
pattern MendeKikakuiNumberTeens = MendeKikakuiCombiningNumberTeens

-- | A pattern synonym for 'MendeKikakuiCombiningNumberTens', the name without the @Combining@ part, defined by @'\\x1e8d1'@ (&#x2022;&#x1e8d1;).
pattern MendeKikakuiNumberTens :: CombiningCharacter
pattern MendeKikakuiNumberTens = MendeKikakuiCombiningNumberTens

-- | A pattern synonym for 'MendeKikakuiCombiningNumberHundreds', the name without the @Combining@ part, defined by @'\\x1e8d2'@ (&#x2022;&#x1e8d2;).
pattern MendeKikakuiNumberHundreds :: CombiningCharacter
pattern MendeKikakuiNumberHundreds = MendeKikakuiCombiningNumberHundreds

-- | A pattern synonym for 'MendeKikakuiCombiningNumberThousands', the name without the @Combining@ part, defined by @'\\x1e8d3'@ (&#x2022;&#x1e8d3;).
pattern MendeKikakuiNumberThousands :: CombiningCharacter
pattern MendeKikakuiNumberThousands = MendeKikakuiCombiningNumberThousands

-- | A pattern synonym for 'MendeKikakuiCombiningNumberTenThousands', the name without the @Combining@ part, defined by @'\\x1e8d4'@ (&#x2022;&#x1e8d4;).
pattern MendeKikakuiNumberTenThousands :: CombiningCharacter
pattern MendeKikakuiNumberTenThousands = MendeKikakuiCombiningNumberTenThousands

-- | A pattern synonym for 'MendeKikakuiCombiningNumberHundredThousands', the name without the @Combining@ part, defined by @'\\x1e8d5'@ (&#x2022;&#x1e8d5;).
pattern MendeKikakuiNumberHundredThousands :: CombiningCharacter
pattern MendeKikakuiNumberHundredThousands = MendeKikakuiCombiningNumberHundredThousands

-- | A pattern synonym for 'MendeKikakuiCombiningNumberMillions', the name without the @Combining@ part, defined by @'\\x1e8d6'@ (&#x2022;&#x1e8d6;).
pattern MendeKikakuiNumberMillions :: CombiningCharacter
pattern MendeKikakuiNumberMillions = MendeKikakuiCombiningNumberMillions

instance Arbitrary CombiningCharacter where
    arbitrary = arbitraryBoundedEnum
    
instance Arbitrary CombiningSequence where
    arbitrary = CombiningSequence <$> ((:|) <$> arbitrary <*> arbitrary)
