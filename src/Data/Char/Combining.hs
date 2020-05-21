{-# LANGUAGE Safe #-}

module Data.Char.Combining (
    -- * Combining characters
    CombiningCharacter(..)
    -- * Conversions from and to 'CombiningCharacter'
  , combiningToUnicode, combiningCharacter, combiningCharacter', isCombiningCharacter
  ) where

data CombiningSequence = CombiningSequence [CombiningCharacter] deriving (Eq, Ord, Read, Show)

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
  | CombiningGraphemeJoiner  -- ^ The combining character @COMBINING GRAPHEME JOINER@ from the Unicode standard, defined by @'\\x034f'@ (&#x2022;&#x034f;).
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
  | CombiningCyrillicHundredThousandsSign  -- ^ The combining character @COMBINING CYRILLIC HUNDRED THOUSANDS SIGN@ from the Unicode standard, defined by @'\\x0488'@ (&#x2022;&#x0488;).
  | CombiningCyrillicMillionsSign  -- ^ The combining character @COMBINING CYRILLIC MILLIONS SIGN@ from the Unicode standard, defined by @'\\x0489'@ (&#x2022;&#x0489;).
  | NkoCombiningShortHighTone  -- ^ The combining character @NKO COMBINING SHORT HIGH TONE@ from the Unicode standard, defined by @'\\x07eb'@ (&#x2022;&#x07eb;).
  | NkoCombiningShortLowTone  -- ^ The combining character @NKO COMBINING SHORT LOW TONE@ from the Unicode standard, defined by @'\\x07ec'@ (&#x2022;&#x07ec;).
  | NkoCombiningShortRisingTone  -- ^ The combining character @NKO COMBINING SHORT RISING TONE@ from the Unicode standard, defined by @'\\x07ed'@ (&#x2022;&#x07ed;).
  | NkoCombiningLongDescendingTone  -- ^ The combining character @NKO COMBINING LONG DESCENDING TONE@ from the Unicode standard, defined by @'\\x07ee'@ (&#x2022;&#x07ee;).
  | NkoCombiningLongHighTone  -- ^ The combining character @NKO COMBINING LONG HIGH TONE@ from the Unicode standard, defined by @'\\x07ef'@ (&#x2022;&#x07ef;).
  | NkoCombiningLongLowTone  -- ^ The combining character @NKO COMBINING LONG LOW TONE@ from the Unicode standard, defined by @'\\x07f0'@ (&#x2022;&#x07f0;).
  | NkoCombiningLongRisingTone  -- ^ The combining character @NKO COMBINING LONG RISING TONE@ from the Unicode standard, defined by @'\\x07f1'@ (&#x2022;&#x07f1;).
  | NkoCombiningNasalizationMark  -- ^ The combining character @NKO COMBINING NASALIZATION MARK@ from the Unicode standard, defined by @'\\x07f2'@ (&#x2022;&#x07f2;).
  | NkoCombiningDoubleDotAbove  -- ^ The combining character @NKO COMBINING DOUBLE DOT ABOVE@ from the Unicode standard, defined by @'\\x07f3'@ (&#x2022;&#x07f3;).
  | TeluguSignCombiningCandrabinduAbove  -- ^ The combining character @TELUGU SIGN COMBINING CANDRABINDU ABOVE@ from the Unicode standard, defined by @'\\x0c00'@ (&#x2022;&#x0c00;).
  | TeluguSignCombiningAnusvaraAbove  -- ^ The combining character @TELUGU SIGN COMBINING ANUSVARA ABOVE@ from the Unicode standard, defined by @'\\x0c04'@ (&#x2022;&#x0c04;).
  | MalayalamSignCombiningAnusvaraAbove  -- ^ The combining character @MALAYALAM SIGN COMBINING ANUSVARA ABOVE@ from the Unicode standard, defined by @'\\x0d00'@ (&#x2022;&#x0d00;).
  | EthiopicCombiningGeminationAndVowelLengthMark  -- ^ The combining character @ETHIOPIC COMBINING GEMINATION AND VOWEL LENGTH MARK@ from the Unicode standard, defined by @'\\x135d'@ (&#x2022;&#x135d;).
  | EthiopicCombiningVowelLengthMark  -- ^ The combining character @ETHIOPIC COMBINING VOWEL LENGTH MARK@ from the Unicode standard, defined by @'\\x135e'@ (&#x2022;&#x135e;).
  | EthiopicCombiningGeminationMark  -- ^ The combining character @ETHIOPIC COMBINING GEMINATION MARK@ from the Unicode standard, defined by @'\\x135f'@ (&#x2022;&#x135f;).
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
  | CombiningParenthesesOverlay  -- ^ The combining character @COMBINING PARENTHESES OVERLAY@ from the Unicode standard, defined by @'\\x1abe'@ (&#x2022;&#x1abe;).
  | CombiningLatinSmallLetterWBelow  -- ^ The combining character @COMBINING LATIN SMALL LETTER W BELOW@ from the Unicode standard, defined by @'\\x1abf'@ (&#x2022;&#x1abf;).
  | CombiningLatinSmallLetterTurnedWBelow  -- ^ The combining character @COMBINING LATIN SMALL LETTER TURNED W BELOW@ from the Unicode standard, defined by @'\\x1ac0'@ (&#x2022;&#x1ac0;).
  | BalineseMusicalSymbolCombiningTegeh  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING TEGEH@ from the Unicode standard, defined by @'\\x1b6b'@ (&#x2022;&#x1b6b;).
  | BalineseMusicalSymbolCombiningEndep  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING ENDEP@ from the Unicode standard, defined by @'\\x1b6c'@ (&#x2022;&#x1b6c;).
  | BalineseMusicalSymbolCombiningKempul  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING KEMPUL@ from the Unicode standard, defined by @'\\x1b6d'@ (&#x2022;&#x1b6d;).
  | BalineseMusicalSymbolCombiningKempli  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING KEMPLI@ from the Unicode standard, defined by @'\\x1b6e'@ (&#x2022;&#x1b6e;).
  | BalineseMusicalSymbolCombiningJegogan  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING JEGOGAN@ from the Unicode standard, defined by @'\\x1b6f'@ (&#x2022;&#x1b6f;).
  | BalineseMusicalSymbolCombiningKempulWithJegogan  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING KEMPUL WITH JEGOGAN@ from the Unicode standard, defined by @'\\x1b70'@ (&#x2022;&#x1b70;).
  | BalineseMusicalSymbolCombiningKempliWithJegogan  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING KEMPLI WITH JEGOGAN@ from the Unicode standard, defined by @'\\x1b71'@ (&#x2022;&#x1b71;).
  | BalineseMusicalSymbolCombiningBende  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING BENDE@ from the Unicode standard, defined by @'\\x1b72'@ (&#x2022;&#x1b72;).
  | BalineseMusicalSymbolCombiningGong  -- ^ The combining character @BALINESE MUSICAL SYMBOL COMBINING GONG@ from the Unicode standard, defined by @'\\x1b73'@ (&#x2022;&#x1b73;).
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
  | CombiningKavykaAboveRight  -- ^ The combining character @COMBINING KAVYKA ABOVE RIGHT@ from the Unicode standard, defined by @'\\x1df6'@ (&#x2022;&#x1df6;).
  | CombiningKavykaAboveLeft  -- ^ The combining character @COMBINING KAVYKA ABOVE LEFT@ from the Unicode standard, defined by @'\\x1df7'@ (&#x2022;&#x1df7;).
  | CombiningDotAboveLeft  -- ^ The combining character @COMBINING DOT ABOVE LEFT@ from the Unicode standard, defined by @'\\x1df8'@ (&#x2022;&#x1df8;).
  | CombiningWideInvertedBridgeBelow  -- ^ The combining character @COMBINING WIDE INVERTED BRIDGE BELOW@ from the Unicode standard, defined by @'\\x1df9'@ (&#x2022;&#x1df9;).
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
  | CombiningEnclosingCircle  -- ^ The combining character @COMBINING ENCLOSING CIRCLE@ from the Unicode standard, defined by @'\\x20dd'@ (&#x2022;&#x20dd;).
  | CombiningEnclosingSquare  -- ^ The combining character @COMBINING ENCLOSING SQUARE@ from the Unicode standard, defined by @'\\x20de'@ (&#x2022;&#x20de;).
  | CombiningEnclosingDiamond  -- ^ The combining character @COMBINING ENCLOSING DIAMOND@ from the Unicode standard, defined by @'\\x20df'@ (&#x2022;&#x20df;).
  | CombiningEnclosingCircleBackslash  -- ^ The combining character @COMBINING ENCLOSING CIRCLE BACKSLASH@ from the Unicode standard, defined by @'\\x20e0'@ (&#x2022;&#x20e0;).
  | CombiningLeftRightArrowAbove  -- ^ The combining character @COMBINING LEFT RIGHT ARROW ABOVE@ from the Unicode standard, defined by @'\\x20e1'@ (&#x2022;&#x20e1;).
  | CombiningEnclosingScreen  -- ^ The combining character @COMBINING ENCLOSING SCREEN@ from the Unicode standard, defined by @'\\x20e2'@ (&#x2022;&#x20e2;).
  | CombiningEnclosingKeycap  -- ^ The combining character @COMBINING ENCLOSING KEYCAP@ from the Unicode standard, defined by @'\\x20e3'@ (&#x2022;&#x20e3;).
  | CombiningEnclosingUpwardPointingTriangle  -- ^ The combining character @COMBINING ENCLOSING UPWARD POINTING TRIANGLE@ from the Unicode standard, defined by @'\\x20e4'@ (&#x2022;&#x20e4;).
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
  | CombiningKatakanaHiraganaVoicedSoundMark  -- ^ The combining character @COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK@ from the Unicode standard, defined by @'\\x3099'@ (&#x2022;&#x3099;).
  | CombiningKatakanaHiraganaSemiVoicedSoundMark  -- ^ The combining character @COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK@ from the Unicode standard, defined by @'\\x309a'@ (&#x2022;&#x309a;).
  | CombiningCyrillicVzmet  -- ^ The combining character @COMBINING CYRILLIC VZMET@ from the Unicode standard, defined by @'\\xa66f'@ (&#x2022;&#xa66f;).
  | CombiningCyrillicTenMillionsSign  -- ^ The combining character @COMBINING CYRILLIC TEN MILLIONS SIGN@ from the Unicode standard, defined by @'\\xa670'@ (&#x2022;&#xa670;).
  | CombiningCyrillicHundredMillionsSign  -- ^ The combining character @COMBINING CYRILLIC HUNDRED MILLIONS SIGN@ from the Unicode standard, defined by @'\\xa671'@ (&#x2022;&#xa671;).
  | CombiningCyrillicThousandMillionsSign  -- ^ The combining character @COMBINING CYRILLIC THOUSAND MILLIONS SIGN@ from the Unicode standard, defined by @'\\xa672'@ (&#x2022;&#xa672;).
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
  | CombiningOldPermicLetterAn  -- ^ The combining character @COMBINING OLD PERMIC LETTER AN@ from the Unicode standard, defined by @'\\x10376'@ (&#x2022;&#x10376;).
  | CombiningOldPermicLetterDoi  -- ^ The combining character @COMBINING OLD PERMIC LETTER DOI@ from the Unicode standard, defined by @'\\x10377'@ (&#x2022;&#x10377;).
  | CombiningOldPermicLetterZata  -- ^ The combining character @COMBINING OLD PERMIC LETTER ZATA@ from the Unicode standard, defined by @'\\x10378'@ (&#x2022;&#x10378;).
  | CombiningOldPermicLetterNenoe  -- ^ The combining character @COMBINING OLD PERMIC LETTER NENOE@ from the Unicode standard, defined by @'\\x10379'@ (&#x2022;&#x10379;).
  | CombiningOldPermicLetterSii  -- ^ The combining character @COMBINING OLD PERMIC LETTER SII@ from the Unicode standard, defined by @'\\x1037a'@ (&#x2022;&#x1037a;).
  | YezidiCombiningHamzaMark  -- ^ The combining character @YEZIDI COMBINING HAMZA MARK@ from the Unicode standard, defined by @'\\x10eab'@ (&#x2022;&#x10eab;).
  | YezidiCombiningMaddaMark  -- ^ The combining character @YEZIDI COMBINING MADDA MARK@ from the Unicode standard, defined by @'\\x10eac'@ (&#x2022;&#x10eac;).
  | SogdianCombiningDotBelow  -- ^ The combining character @SOGDIAN COMBINING DOT BELOW@ from the Unicode standard, defined by @'\\x10f46'@ (&#x2022;&#x10f46;).
  | SogdianCombiningTwoDotsBelow  -- ^ The combining character @SOGDIAN COMBINING TWO DOTS BELOW@ from the Unicode standard, defined by @'\\x10f47'@ (&#x2022;&#x10f47;).
  | SogdianCombiningDotAbove  -- ^ The combining character @SOGDIAN COMBINING DOT ABOVE@ from the Unicode standard, defined by @'\\x10f48'@ (&#x2022;&#x10f48;).
  | SogdianCombiningTwoDotsAbove  -- ^ The combining character @SOGDIAN COMBINING TWO DOTS ABOVE@ from the Unicode standard, defined by @'\\x10f49'@ (&#x2022;&#x10f49;).
  | SogdianCombiningCurveAbove  -- ^ The combining character @SOGDIAN COMBINING CURVE ABOVE@ from the Unicode standard, defined by @'\\x10f4a'@ (&#x2022;&#x10f4a;).
  | SogdianCombiningCurveBelow  -- ^ The combining character @SOGDIAN COMBINING CURVE BELOW@ from the Unicode standard, defined by @'\\x10f4b'@ (&#x2022;&#x10f4b;).
  | SogdianCombiningHookAbove  -- ^ The combining character @SOGDIAN COMBINING HOOK ABOVE@ from the Unicode standard, defined by @'\\x10f4c'@ (&#x2022;&#x10f4c;).
  | SogdianCombiningHookBelow  -- ^ The combining character @SOGDIAN COMBINING HOOK BELOW@ from the Unicode standard, defined by @'\\x10f4d'@ (&#x2022;&#x10f4d;).
  | SogdianCombiningLongHookBelow  -- ^ The combining character @SOGDIAN COMBINING LONG HOOK BELOW@ from the Unicode standard, defined by @'\\x10f4e'@ (&#x2022;&#x10f4e;).
  | SogdianCombiningReshBelow  -- ^ The combining character @SOGDIAN COMBINING RESH BELOW@ from the Unicode standard, defined by @'\\x10f4f'@ (&#x2022;&#x10f4f;).
  | SogdianCombiningStrokeBelow  -- ^ The combining character @SOGDIAN COMBINING STROKE BELOW@ from the Unicode standard, defined by @'\\x10f50'@ (&#x2022;&#x10f50;).
  | GranthaSignCombiningAnusvaraAbove  -- ^ The combining character @GRANTHA SIGN COMBINING ANUSVARA ABOVE@ from the Unicode standard, defined by @'\\x11300'@ (&#x2022;&#x11300;).
  | CombiningBinduBelow  -- ^ The combining character @COMBINING BINDU BELOW@ from the Unicode standard, defined by @'\\x1133b'@ (&#x2022;&#x1133b;).
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
  | BassaVahCombiningHighTone  -- ^ The combining character @BASSA VAH COMBINING HIGH TONE@ from the Unicode standard, defined by @'\\x16af0'@ (&#x2022;&#x16af0;).
  | BassaVahCombiningLowTone  -- ^ The combining character @BASSA VAH COMBINING LOW TONE@ from the Unicode standard, defined by @'\\x16af1'@ (&#x2022;&#x16af1;).
  | BassaVahCombiningMidTone  -- ^ The combining character @BASSA VAH COMBINING MID TONE@ from the Unicode standard, defined by @'\\x16af2'@ (&#x2022;&#x16af2;).
  | BassaVahCombiningLowMidTone  -- ^ The combining character @BASSA VAH COMBINING LOW-MID TONE@ from the Unicode standard, defined by @'\\x16af3'@ (&#x2022;&#x16af3;).
  | BassaVahCombiningHighLowTone  -- ^ The combining character @BASSA VAH COMBINING HIGH-LOW TONE@ from the Unicode standard, defined by @'\\x16af4'@ (&#x2022;&#x16af4;).
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
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Obtain the Unicode codepoint for the given CombiningCharacter.
combiningToUnicode
  :: CombiningCharacter -- ^ The given 'CombiningCharacter' to obtain the Unicode codepoint 'Char' from.
  -> Char -- ^ The equivalent Unicode codepoint 'Char'.
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
combiningToUnicode CombiningGraphemeJoiner = '\x034f'
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
combiningToUnicode CombiningCyrillicHundredThousandsSign = '\x0488'
combiningToUnicode CombiningCyrillicMillionsSign = '\x0489'
combiningToUnicode NkoCombiningShortHighTone = '\x07eb'
combiningToUnicode NkoCombiningShortLowTone = '\x07ec'
combiningToUnicode NkoCombiningShortRisingTone = '\x07ed'
combiningToUnicode NkoCombiningLongDescendingTone = '\x07ee'
combiningToUnicode NkoCombiningLongHighTone = '\x07ef'
combiningToUnicode NkoCombiningLongLowTone = '\x07f0'
combiningToUnicode NkoCombiningLongRisingTone = '\x07f1'
combiningToUnicode NkoCombiningNasalizationMark = '\x07f2'
combiningToUnicode NkoCombiningDoubleDotAbove = '\x07f3'
combiningToUnicode TeluguSignCombiningCandrabinduAbove = '\x0c00'
combiningToUnicode TeluguSignCombiningAnusvaraAbove = '\x0c04'
combiningToUnicode MalayalamSignCombiningAnusvaraAbove = '\x0d00'
combiningToUnicode EthiopicCombiningGeminationAndVowelLengthMark = '\x135d'
combiningToUnicode EthiopicCombiningVowelLengthMark = '\x135e'
combiningToUnicode EthiopicCombiningGeminationMark = '\x135f'
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
combiningToUnicode CombiningParenthesesOverlay = '\x1abe'
combiningToUnicode CombiningLatinSmallLetterWBelow = '\x1abf'
combiningToUnicode CombiningLatinSmallLetterTurnedWBelow = '\x1ac0'
combiningToUnicode BalineseMusicalSymbolCombiningTegeh = '\x1b6b'
combiningToUnicode BalineseMusicalSymbolCombiningEndep = '\x1b6c'
combiningToUnicode BalineseMusicalSymbolCombiningKempul = '\x1b6d'
combiningToUnicode BalineseMusicalSymbolCombiningKempli = '\x1b6e'
combiningToUnicode BalineseMusicalSymbolCombiningJegogan = '\x1b6f'
combiningToUnicode BalineseMusicalSymbolCombiningKempulWithJegogan = '\x1b70'
combiningToUnicode BalineseMusicalSymbolCombiningKempliWithJegogan = '\x1b71'
combiningToUnicode BalineseMusicalSymbolCombiningBende = '\x1b72'
combiningToUnicode BalineseMusicalSymbolCombiningGong = '\x1b73'
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
combiningToUnicode CombiningKavykaAboveRight = '\x1df6'
combiningToUnicode CombiningKavykaAboveLeft = '\x1df7'
combiningToUnicode CombiningDotAboveLeft = '\x1df8'
combiningToUnicode CombiningWideInvertedBridgeBelow = '\x1df9'
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
combiningToUnicode CombiningEnclosingCircle = '\x20dd'
combiningToUnicode CombiningEnclosingSquare = '\x20de'
combiningToUnicode CombiningEnclosingDiamond = '\x20df'
combiningToUnicode CombiningEnclosingCircleBackslash = '\x20e0'
combiningToUnicode CombiningLeftRightArrowAbove = '\x20e1'
combiningToUnicode CombiningEnclosingScreen = '\x20e2'
combiningToUnicode CombiningEnclosingKeycap = '\x20e3'
combiningToUnicode CombiningEnclosingUpwardPointingTriangle = '\x20e4'
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
combiningToUnicode CombiningKatakanaHiraganaVoicedSoundMark = '\x3099'
combiningToUnicode CombiningKatakanaHiraganaSemiVoicedSoundMark = '\x309a'
combiningToUnicode CombiningCyrillicVzmet = '\xa66f'
combiningToUnicode CombiningCyrillicTenMillionsSign = '\xa670'
combiningToUnicode CombiningCyrillicHundredMillionsSign = '\xa671'
combiningToUnicode CombiningCyrillicThousandMillionsSign = '\xa672'
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
combiningToUnicode CombiningOldPermicLetterAn = '\x10376'
combiningToUnicode CombiningOldPermicLetterDoi = '\x10377'
combiningToUnicode CombiningOldPermicLetterZata = '\x10378'
combiningToUnicode CombiningOldPermicLetterNenoe = '\x10379'
combiningToUnicode CombiningOldPermicLetterSii = '\x1037a'
combiningToUnicode YezidiCombiningHamzaMark = '\x10eab'
combiningToUnicode YezidiCombiningMaddaMark = '\x10eac'
combiningToUnicode SogdianCombiningDotBelow = '\x10f46'
combiningToUnicode SogdianCombiningTwoDotsBelow = '\x10f47'
combiningToUnicode SogdianCombiningDotAbove = '\x10f48'
combiningToUnicode SogdianCombiningTwoDotsAbove = '\x10f49'
combiningToUnicode SogdianCombiningCurveAbove = '\x10f4a'
combiningToUnicode SogdianCombiningCurveBelow = '\x10f4b'
combiningToUnicode SogdianCombiningHookAbove = '\x10f4c'
combiningToUnicode SogdianCombiningHookBelow = '\x10f4d'
combiningToUnicode SogdianCombiningLongHookBelow = '\x10f4e'
combiningToUnicode SogdianCombiningReshBelow = '\x10f4f'
combiningToUnicode SogdianCombiningStrokeBelow = '\x10f50'
combiningToUnicode GranthaSignCombiningAnusvaraAbove = '\x11300'
combiningToUnicode CombiningBinduBelow = '\x1133b'
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
combiningToUnicode BassaVahCombiningHighTone = '\x16af0'
combiningToUnicode BassaVahCombiningLowTone = '\x16af1'
combiningToUnicode BassaVahCombiningMidTone = '\x16af2'
combiningToUnicode BassaVahCombiningLowMidTone = '\x16af3'
combiningToUnicode BassaVahCombiningHighLowTone = '\x16af4'
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

combiningCharacter' :: Char -> CombiningCharacter
combiningCharacter' c
    | Just y <- combiningCharacter c = y
    | otherwise = error "The given character is not a CombiningCharacter."

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
combiningCharacter '\x034f' = Just CombiningGraphemeJoiner
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
combiningCharacter '\x0488' = Just CombiningCyrillicHundredThousandsSign
combiningCharacter '\x0489' = Just CombiningCyrillicMillionsSign
combiningCharacter '\x07eb' = Just NkoCombiningShortHighTone
combiningCharacter '\x07ec' = Just NkoCombiningShortLowTone
combiningCharacter '\x07ed' = Just NkoCombiningShortRisingTone
combiningCharacter '\x07ee' = Just NkoCombiningLongDescendingTone
combiningCharacter '\x07ef' = Just NkoCombiningLongHighTone
combiningCharacter '\x07f0' = Just NkoCombiningLongLowTone
combiningCharacter '\x07f1' = Just NkoCombiningLongRisingTone
combiningCharacter '\x07f2' = Just NkoCombiningNasalizationMark
combiningCharacter '\x07f3' = Just NkoCombiningDoubleDotAbove
combiningCharacter '\x0c00' = Just TeluguSignCombiningCandrabinduAbove
combiningCharacter '\x0c04' = Just TeluguSignCombiningAnusvaraAbove
combiningCharacter '\x0d00' = Just MalayalamSignCombiningAnusvaraAbove
combiningCharacter '\x135d' = Just EthiopicCombiningGeminationAndVowelLengthMark
combiningCharacter '\x135e' = Just EthiopicCombiningVowelLengthMark
combiningCharacter '\x135f' = Just EthiopicCombiningGeminationMark
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
combiningCharacter '\x1abe' = Just CombiningParenthesesOverlay
combiningCharacter '\x1abf' = Just CombiningLatinSmallLetterWBelow
combiningCharacter '\x1ac0' = Just CombiningLatinSmallLetterTurnedWBelow
combiningCharacter '\x1b6b' = Just BalineseMusicalSymbolCombiningTegeh
combiningCharacter '\x1b6c' = Just BalineseMusicalSymbolCombiningEndep
combiningCharacter '\x1b6d' = Just BalineseMusicalSymbolCombiningKempul
combiningCharacter '\x1b6e' = Just BalineseMusicalSymbolCombiningKempli
combiningCharacter '\x1b6f' = Just BalineseMusicalSymbolCombiningJegogan
combiningCharacter '\x1b70' = Just BalineseMusicalSymbolCombiningKempulWithJegogan
combiningCharacter '\x1b71' = Just BalineseMusicalSymbolCombiningKempliWithJegogan
combiningCharacter '\x1b72' = Just BalineseMusicalSymbolCombiningBende
combiningCharacter '\x1b73' = Just BalineseMusicalSymbolCombiningGong
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
combiningCharacter '\x1df6' = Just CombiningKavykaAboveRight
combiningCharacter '\x1df7' = Just CombiningKavykaAboveLeft
combiningCharacter '\x1df8' = Just CombiningDotAboveLeft
combiningCharacter '\x1df9' = Just CombiningWideInvertedBridgeBelow
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
combiningCharacter '\x20dd' = Just CombiningEnclosingCircle
combiningCharacter '\x20de' = Just CombiningEnclosingSquare
combiningCharacter '\x20df' = Just CombiningEnclosingDiamond
combiningCharacter '\x20e0' = Just CombiningEnclosingCircleBackslash
combiningCharacter '\x20e1' = Just CombiningLeftRightArrowAbove
combiningCharacter '\x20e2' = Just CombiningEnclosingScreen
combiningCharacter '\x20e3' = Just CombiningEnclosingKeycap
combiningCharacter '\x20e4' = Just CombiningEnclosingUpwardPointingTriangle
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
combiningCharacter '\x3099' = Just CombiningKatakanaHiraganaVoicedSoundMark
combiningCharacter '\x309a' = Just CombiningKatakanaHiraganaSemiVoicedSoundMark
combiningCharacter '\xa66f' = Just CombiningCyrillicVzmet
combiningCharacter '\xa670' = Just CombiningCyrillicTenMillionsSign
combiningCharacter '\xa671' = Just CombiningCyrillicHundredMillionsSign
combiningCharacter '\xa672' = Just CombiningCyrillicThousandMillionsSign
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
combiningCharacter '\x10376' = Just CombiningOldPermicLetterAn
combiningCharacter '\x10377' = Just CombiningOldPermicLetterDoi
combiningCharacter '\x10378' = Just CombiningOldPermicLetterZata
combiningCharacter '\x10379' = Just CombiningOldPermicLetterNenoe
combiningCharacter '\x1037a' = Just CombiningOldPermicLetterSii
combiningCharacter '\x10eab' = Just YezidiCombiningHamzaMark
combiningCharacter '\x10eac' = Just YezidiCombiningMaddaMark
combiningCharacter '\x10f46' = Just SogdianCombiningDotBelow
combiningCharacter '\x10f47' = Just SogdianCombiningTwoDotsBelow
combiningCharacter '\x10f48' = Just SogdianCombiningDotAbove
combiningCharacter '\x10f49' = Just SogdianCombiningTwoDotsAbove
combiningCharacter '\x10f4a' = Just SogdianCombiningCurveAbove
combiningCharacter '\x10f4b' = Just SogdianCombiningCurveBelow
combiningCharacter '\x10f4c' = Just SogdianCombiningHookAbove
combiningCharacter '\x10f4d' = Just SogdianCombiningHookBelow
combiningCharacter '\x10f4e' = Just SogdianCombiningLongHookBelow
combiningCharacter '\x10f4f' = Just SogdianCombiningReshBelow
combiningCharacter '\x10f50' = Just SogdianCombiningStrokeBelow
combiningCharacter '\x11300' = Just GranthaSignCombiningAnusvaraAbove
combiningCharacter '\x1133b' = Just CombiningBinduBelow
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
combiningCharacter '\x16af0' = Just BassaVahCombiningHighTone
combiningCharacter '\x16af1' = Just BassaVahCombiningLowTone
combiningCharacter '\x16af2' = Just BassaVahCombiningMidTone
combiningCharacter '\x16af3' = Just BassaVahCombiningLowMidTone
combiningCharacter '\x16af4' = Just BassaVahCombiningHighLowTone
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
combiningCharacter _ = Nothing
