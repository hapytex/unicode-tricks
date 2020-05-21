{-# LANGUAGE Safe #-}

module Data.Char.Combining (
    -- * Combining characters
    CombiningCharacter(..)
    -- * Conversions from and to 'CombiningCharacter'
  , combiningToUnicode, combiningCharacter, combiningCharacter', isCombiningCharacter
  ) where

import Data.Maybe(isJust)
import Data.String(IsString(fromString))

newtype CombiningSequence = CombiningSequence [CombiningCharacter] deriving (Eq, Ord, Read, Show)

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
  | BengaliSignVirama  -- ^ The combining character @BENGALI SIGN VIRAMA@ from the Unicode standard, defined by @'\\x09cd'@ (&#x2022;&#x09cd;).
  | GurmukhiSignNukta  -- ^ The combining character @GURMUKHI SIGN NUKTA@ from the Unicode standard, defined by @'\\x0a3c'@ (&#x2022;&#x0a3c;).
  | GurmukhiSignVirama  -- ^ The combining character @GURMUKHI SIGN VIRAMA@ from the Unicode standard, defined by @'\\x0a4d'@ (&#x2022;&#x0a4d;).
  | GujaratiSignNukta  -- ^ The combining character @GUJARATI SIGN NUKTA@ from the Unicode standard, defined by @'\\x0abc'@ (&#x2022;&#x0abc;).
  | GujaratiSignVirama  -- ^ The combining character @GUJARATI SIGN VIRAMA@ from the Unicode standard, defined by @'\\x0acd'@ (&#x2022;&#x0acd;).
  | OriyaSignNukta  -- ^ The combining character @ORIYA SIGN NUKTA@ from the Unicode standard, defined by @'\\x0b3c'@ (&#x2022;&#x0b3c;).
  | OriyaSignVirama  -- ^ The combining character @ORIYA SIGN VIRAMA@ from the Unicode standard, defined by @'\\x0b4d'@ (&#x2022;&#x0b4d;).
  | TamilSignVirama  -- ^ The combining character @TAMIL SIGN VIRAMA@ from the Unicode standard, defined by @'\\x0bcd'@ (&#x2022;&#x0bcd;).
  | TeluguSignVirama  -- ^ The combining character @TELUGU SIGN VIRAMA@ from the Unicode standard, defined by @'\\x0c4d'@ (&#x2022;&#x0c4d;).
  | TeluguLengthMark  -- ^ The combining character @TELUGU LENGTH MARK@ from the Unicode standard, defined by @'\\x0c55'@ (&#x2022;&#x0c55;).
  | TeluguAiLengthMark  -- ^ The combining character @TELUGU AI LENGTH MARK@ from the Unicode standard, defined by @'\\x0c56'@ (&#x2022;&#x0c56;).
  | KannadaSignNukta  -- ^ The combining character @KANNADA SIGN NUKTA@ from the Unicode standard, defined by @'\\x0cbc'@ (&#x2022;&#x0cbc;).
  | KannadaSignVirama  -- ^ The combining character @KANNADA SIGN VIRAMA@ from the Unicode standard, defined by @'\\x0ccd'@ (&#x2022;&#x0ccd;).
  | MalayalamSignVirama  -- ^ The combining character @MALAYALAM SIGN VIRAMA@ from the Unicode standard, defined by @'\\x0d4d'@ (&#x2022;&#x0d4d;).
  | SinhalaSignAlLakuna  -- ^ The combining character @SINHALA SIGN AL-LAKUNA@ from the Unicode standard, defined by @'\\x0dca'@ (&#x2022;&#x0dca;).
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
  | TibetanSymbolPadmaGdan  -- ^ The combining character @TIBETAN SYMBOL PADMA GDAN@ from the Unicode standard, defined by @'\\x0fc6'@ (&#x2022;&#x0fc6;).
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
  | TaiThamSignTone1  -- ^ The combining character @TAI THAM SIGN TONE1@ from the Unicode standard, defined by @'\\x1a75'@ (&#x2022;&#x1a75;).
  | TaiThamSignTone2  -- ^ The combining character @TAI THAM SIGN TONE2@ from the Unicode standard, defined by @'\\x1a76'@ (&#x2022;&#x1a76;).
  | TaiThamSignKhuenTone3  -- ^ The combining character @TAI THAM SIGN KHUEN TONE3@ from the Unicode standard, defined by @'\\x1a77'@ (&#x2022;&#x1a77;).
  | TaiThamSignKhuenTone4  -- ^ The combining character @TAI THAM SIGN KHUEN TONE4@ from the Unicode standard, defined by @'\\x1a78'@ (&#x2022;&#x1a78;).
  | TaiThamSignKhuenTone5  -- ^ The combining character @TAI THAM SIGN KHUEN TONE5@ from the Unicode standard, defined by @'\\x1a79'@ (&#x2022;&#x1a79;).
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
  | GranthaSignVirama  -- ^ The combining character @GRANTHA SIGN VIRAMA@ from the Unicode standard, defined by @'\\x1134d'@ (&#x2022;&#x1134d;).
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
  | TirhutaSignVirama  -- ^ The combining character @TIRHUTA SIGN VIRAMA@ from the Unicode standard, defined by @'\\x114c2'@ (&#x2022;&#x114c2;).
  | TirhutaSignNukta  -- ^ The combining character @TIRHUTA SIGN NUKTA@ from the Unicode standard, defined by @'\\x114c3'@ (&#x2022;&#x114c3;).
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
  | MusicalSymbolCombiningTremolo1  -- ^ The combining character @MUSICAL SYMBOL COMBINING TREMOLO1@ from the Unicode standard, defined by @'\\x1d167'@ (&#x2022;&#x1d167;).
  | MusicalSymbolCombiningTremolo2  -- ^ The combining character @MUSICAL SYMBOL COMBINING TREMOLO2@ from the Unicode standard, defined by @'\\x1d168'@ (&#x2022;&#x1d168;).
  | MusicalSymbolCombiningTremolo3  -- ^ The combining character @MUSICAL SYMBOL COMBINING TREMOLO3@ from the Unicode standard, defined by @'\\x1d169'@ (&#x2022;&#x1d169;).
  | MusicalSymbolCombiningAugmentationDot  -- ^ The combining character @MUSICAL SYMBOL COMBINING AUGMENTATION DOT@ from the Unicode standard, defined by @'\\x1d16d'@ (&#x2022;&#x1d16d;).
  | MusicalSymbolCombiningFlag1  -- ^ The combining character @MUSICAL SYMBOL COMBINING FLAG1@ from the Unicode standard, defined by @'\\x1d16e'@ (&#x2022;&#x1d16e;).
  | MusicalSymbolCombiningFlag2  -- ^ The combining character @MUSICAL SYMBOL COMBINING FLAG2@ from the Unicode standard, defined by @'\\x1d16f'@ (&#x2022;&#x1d16f;).
  | MusicalSymbolCombiningFlag3  -- ^ The combining character @MUSICAL SYMBOL COMBINING FLAG3@ from the Unicode standard, defined by @'\\x1d170'@ (&#x2022;&#x1d170;).
  | MusicalSymbolCombiningFlag4  -- ^ The combining character @MUSICAL SYMBOL COMBINING FLAG4@ from the Unicode standard, defined by @'\\x1d171'@ (&#x2022;&#x1d171;).
  | MusicalSymbolCombiningFlag5  -- ^ The combining character @MUSICAL SYMBOL COMBINING FLAG5@ from the Unicode standard, defined by @'\\x1d172'@ (&#x2022;&#x1d172;).
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

instance IsString CombiningCharacter where
    fromString [x] = combiningCharacter' x
    fromString _ = error "The given string should contain exactly one codepoint"

combiningToUnicode :: CombiningCharacter -> Char
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
combiningToUnicode BengaliSignVirama = '\x09cd'
combiningToUnicode GurmukhiSignNukta = '\x0a3c'
combiningToUnicode GurmukhiSignVirama = '\x0a4d'
combiningToUnicode GujaratiSignNukta = '\x0abc'
combiningToUnicode GujaratiSignVirama = '\x0acd'
combiningToUnicode OriyaSignNukta = '\x0b3c'
combiningToUnicode OriyaSignVirama = '\x0b4d'
combiningToUnicode TamilSignVirama = '\x0bcd'
combiningToUnicode TeluguSignVirama = '\x0c4d'
combiningToUnicode TeluguLengthMark = '\x0c55'
combiningToUnicode TeluguAiLengthMark = '\x0c56'
combiningToUnicode KannadaSignNukta = '\x0cbc'
combiningToUnicode KannadaSignVirama = '\x0ccd'
combiningToUnicode MalayalamSignVirama = '\x0d4d'
combiningToUnicode SinhalaSignAlLakuna = '\x0dca'
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
combiningToUnicode TibetanSymbolPadmaGdan = '\x0fc6'
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
combiningToUnicode GranthaSignVirama = '\x1134d'
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
combiningToUnicode TirhutaSignVirama = '\x114c2'
combiningToUnicode TirhutaSignNukta = '\x114c3'
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

isCombiningCharacter :: Char -> Bool
isCombiningCharacter = isJust . combiningCharacter

combiningCharacter' :: Char -> CombiningCharacter
combiningCharacter' c
    | Just y <- combiningCharacter c = y
    | otherwise = error "The given character is a not a CombiningCharacter"

combiningCharacter :: Char -> Maybe CombiningCharacter
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
combiningCharacter '\x09cd' = Just BengaliSignVirama
combiningCharacter '\x0a3c' = Just GurmukhiSignNukta
combiningCharacter '\x0a4d' = Just GurmukhiSignVirama
combiningCharacter '\x0abc' = Just GujaratiSignNukta
combiningCharacter '\x0acd' = Just GujaratiSignVirama
combiningCharacter '\x0b3c' = Just OriyaSignNukta
combiningCharacter '\x0b4d' = Just OriyaSignVirama
combiningCharacter '\x0bcd' = Just TamilSignVirama
combiningCharacter '\x0c4d' = Just TeluguSignVirama
combiningCharacter '\x0c55' = Just TeluguLengthMark
combiningCharacter '\x0c56' = Just TeluguAiLengthMark
combiningCharacter '\x0cbc' = Just KannadaSignNukta
combiningCharacter '\x0ccd' = Just KannadaSignVirama
combiningCharacter '\x0d4d' = Just MalayalamSignVirama
combiningCharacter '\x0dca' = Just SinhalaSignAlLakuna
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
combiningCharacter '\x0fc6' = Just TibetanSymbolPadmaGdan
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
combiningCharacter '\x1134d' = Just GranthaSignVirama
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
combiningCharacter '\x114c2' = Just TirhutaSignVirama
combiningCharacter '\x114c3' = Just TirhutaSignNukta
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
