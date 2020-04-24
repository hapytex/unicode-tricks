module Data.Char.MathSpec
  ( spec
  ) where

import Test.Hspec
import Data.Char.Core
import Data.Char.Math

spec :: Spec
spec = do
  describe "mathAlpha" do
    describe "Serif" do
      describe "NoItalic" do
        describe "NoBold" do
          let transform = mathAlpha Serif NoItalic NoBold
          it "A-Za-z" do
            transform <$> alpha `shouldBe` Just <$> alpha
          it "0-9 (char)" do
            transform <$> digits `shouldBe` (Nothing <$ digits)
        describe "Bold" do
          let transform = mathAlpha Serif NoItalic Bold
          it "A-Za-z" do
            transform <$> alpha `shouldBe` Just <$> "𝐀𝐁𝐂𝐃𝐄𝐅𝐆𝐇𝐈𝐉𝐊𝐋𝐌𝐍𝐎𝐏𝐐𝐑𝐒𝐓𝐔𝐕𝐖𝐗𝐘𝐙𝐚𝐛𝐜𝐝𝐞𝐟𝐠𝐡𝐢𝐣𝐤𝐥𝐦𝐧𝐨𝐩𝐪𝐫𝐬𝐭𝐮𝐯𝐰𝐱𝐲𝐳"
          it "0-9 (char)" do
            transform <$> digits `shouldBe` (Nothing <$ digits)
      describe "Italic" do
        describe "NoBold" do
          let transform = mathAlpha Serif Italic NoBold
          it "A-Za-z" do
            transform <$> alpha `shouldBe` Just <$> "𝐴𝐵𝐶𝐷𝐸𝐹𝐺𝐻𝐼𝐽𝐾𝐿𝑀𝑁𝑂𝑃𝑄𝑅𝑆𝑇𝑈𝑉𝑊𝑋𝑌𝑍𝑎𝑏𝑐𝑑𝑒𝑓𝑔ℎ𝑖𝑗𝑘𝑙𝑚𝑛𝑜𝑝𝑞𝑟𝑠𝑡𝑢𝑣𝑤𝑥𝑦𝑧"
          it "0-9 (char)" do
            transform <$> digits `shouldBe` (Nothing <$ digits)
        describe "Bold" do
          let transform = mathAlpha Serif Italic Bold
          it "A-Za-z" do
            transform <$> alpha `shouldBe` Just <$> "𝑨𝑩𝑪𝑫𝑬𝑭𝑮𝑯𝑰𝑱𝑲𝑳𝑴𝑵𝑶𝑷𝑸𝑹𝑺𝑻𝑼𝑽𝑾𝑿𝒀𝒁𝒂𝒃𝒄𝒅𝒆𝒇𝒈𝒉𝒊𝒋𝒌𝒍𝒎𝒏𝒐𝒑𝒒𝒓𝒔𝒕𝒖𝒗𝒘𝒙𝒚𝒛"
          it "0-9 (char)" do
            transform <$> digits `shouldBe` (Nothing <$ digits)
    describe "SansSerif" do
      describe "NoItalic" do
        describe "NoBold" do
          let transform = mathAlpha SansSerif NoItalic NoBold
          it "A-Za-z" do
            transform <$> alpha `shouldBe` Just <$> "𝖠𝖡𝖢𝖣𝖤𝖥𝖦𝖧𝖨𝖩𝖪𝖫𝖬𝖭𝖮𝖯𝖰𝖱𝖲𝖳𝖴𝖵𝖶𝖷𝖸𝖹𝖺𝖻𝖼𝖽𝖾𝖿𝗀𝗁𝗂𝗃𝗄𝗅𝗆𝗇𝗈𝗉𝗊𝗋𝗌𝗍𝗎𝗏𝗐𝗑𝗒𝗓"
          it "0-9 (char)" do
            transform <$> digits `shouldBe` (Nothing <$ digits)
        describe "Bold" do
          let transform = mathAlpha SansSerif NoItalic Bold
          it "A-Za-z" do
            transform <$> alpha `shouldBe` Just <$> "𝗔𝗕𝗖𝗗𝗘𝗙𝗚𝗛𝗜𝗝𝗞𝗟𝗠𝗡𝗢𝗣𝗤𝗥𝗦𝗧𝗨𝗩𝗪𝗫𝗬𝗭𝗮𝗯𝗰𝗱𝗲𝗳𝗴𝗵𝗶𝗷𝗸𝗹𝗺𝗻𝗼𝗽𝗾𝗿𝘀𝘁𝘂𝘃𝘄𝘅𝘆𝘇"
          it "0-9 (char)" do
            transform <$> digits `shouldBe` (Nothing <$ digits)
      describe "Italic" do
        describe "NoBold" do
          let transform = mathAlpha SansSerif Italic NoBold
          it "A-Za-z" do
            transform <$> alpha `shouldBe` Just <$> "𝘈𝘉𝘊𝘋𝘌𝘍𝘎𝘏𝘐𝘑𝘒𝘓𝘔𝘕𝘖𝘗𝘘𝘙𝘚𝘛𝘜𝘝𝘞𝘟𝘠𝘡𝘢𝘣𝘤𝘥𝘦𝘧𝘨𝘩𝘪𝘫𝘬𝘭𝘮𝘯𝘰𝘱𝘲𝘳𝘴𝘵𝘶𝘷𝘸𝘹𝘺𝘻"
          it "0-9 (char)" do
            transform <$> digits `shouldBe` (Nothing <$ digits)
        describe "Bold" do
          let transform = mathAlpha SansSerif Italic Bold
          it "A-Za-z" do
            transform <$> alpha `shouldBe` Just <$> "𝘼𝘽𝘾𝘿𝙀𝙁𝙂𝙃𝙄𝙅𝙆𝙇𝙈𝙉𝙊𝙋𝙌𝙍𝙎𝙏𝙐𝙑𝙒𝙓𝙔𝙕𝙖𝙗𝙘𝙙𝙚𝙛𝙜𝙝𝙞𝙟𝙠𝙡𝙢𝙣𝙤𝙥𝙦𝙧𝙨𝙩𝙪𝙫𝙬𝙭𝙮𝙯"
          it "0-9 (char)" do
            transform <$> digits `shouldBe` (Nothing <$ digits)
  describe "digit" do
    describe "Serif" do
      describe "NoBold" do
        let transform = digit Serif NoBold
        it "A-Za-z" do
          transform <$> alpha `shouldBe` (Nothing <$ alpha)
        it "0-9 (char)" do
          transform <$> digits `shouldBe` Just <$> digits
        it "0-9 (int)" do
          intToDigitChar Serif NoBold <$> [0..9] `shouldBe` Just <$> digits
      describe "Bold" do
        let transform = digit Serif Bold
        it "A-Za-z" do
          transform <$> alpha `shouldBe` (Nothing <$ alpha)
        it "0-9 (char)" do
          transform <$> digits `shouldBe` Just <$> "𝟎𝟏𝟐𝟑𝟒𝟓𝟔𝟕𝟖𝟗"
        it "0-9 (int)" do
          intToDigitChar Serif Bold <$> [0..9] `shouldBe` Just <$> "𝟎𝟏𝟐𝟑𝟒𝟓𝟔𝟕𝟖𝟗"
    describe "SansSerif" do
      describe "NoBold" do
        let transform = digit SansSerif NoBold
        it "A-Za-z" do
          transform <$> alpha `shouldBe` (Nothing <$ alpha)
        it "0-9 (char)" do
          transform <$> digits `shouldBe` Just <$> "𝟢𝟣𝟤𝟥𝟦𝟧𝟨𝟩𝟪𝟫"
        it "0-9 (int)" do
          intToDigitChar SansSerif NoBold <$> [0..9] `shouldBe` Just <$> "𝟢𝟣𝟤𝟥𝟦𝟧𝟨𝟩𝟪𝟫"
      describe "Bold" do
        let transform = digit SansSerif Bold
        it "A-Za-z" do
          transform <$> alpha `shouldBe` (Nothing <$ alpha)
        it "0-9 (char)" do
          transform <$> digits `shouldBe` Just <$> "𝟬𝟭𝟮𝟯𝟰𝟱𝟲𝟳𝟴𝟵"
        it "0-9 (int)" do
          intToDigitChar SansSerif Bold <$> [0..9] `shouldBe` Just <$> "𝟬𝟭𝟮𝟯𝟰𝟱𝟲𝟳𝟴𝟵"
  describe "doubleStruck" do
    it "A-Za-z" do
      doubleStruck <$> alpha `shouldBe` Just <$> "𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℙℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ𝕒𝕓𝕔𝕕𝕖𝕗𝕘𝕙𝕚𝕛𝕜𝕝𝕞𝕟𝕠𝕡𝕢𝕣𝕤𝕥𝕦𝕧𝕨𝕩𝕪𝕫"
    it "0-9 (char)" do
      doubleStruck <$> digits `shouldBe` Just <$> "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡"
    it "0-9 (int)" do
      intToDigitDoubleStruck <$> [0..9] `shouldBe` Just <$> "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡"
  describe "script" do
    describe "NoBold" do
      let transform = script NoBold
      it "A-Za-z" do
        transform <$> alpha `shouldBe` Just <$> "𝒜ℬ𝒞𝒟ℰℱ𝒢ℋℐ𝒥𝒦ℒℳ𝒩𝒪𝒫𝒬ℛ𝒮𝒯𝒰𝒱𝒲𝒳𝒴𝒵𝒶𝒷𝒸𝒹ℯ𝒻ℊ𝒽𝒾𝒿𝓀𝓁𝓂𝓃ℴ𝓅𝓆𝓇𝓈𝓉𝓊𝓋𝓌𝓍𝓎𝓏"
      it "0-9 (char)" do
        transform <$> digits `shouldBe` (Nothing <$ digits)
    describe "Bold" do
      let transform = script Bold
      it "A-Za-z" do
        transform <$> alpha `shouldBe` Just <$> "𝓐𝓑𝓒𝓓𝓔𝓕𝓖𝓗𝓘𝓙𝓚𝓛𝓜𝓝𝓞𝓟𝓠𝓡𝓢𝓣𝓤𝓥𝓦𝓧𝓨𝓩𝓪𝓫𝓬𝓭𝓮𝓯𝓰𝓱𝓲𝓳𝓴𝓵𝓶𝓷𝓸𝓹𝓺𝓻𝓼𝓽𝓾𝓿𝔀𝔁𝔂𝔃"
      it "0-9 (char)" do
        transform <$> digits `shouldBe` (Nothing <$ digits)
  describe "fraktur" do
    describe "NoBold" do
      let transform = fraktur NoBold
      it "A-Za-z" do
        transform <$> alpha `shouldBe` Just <$> "𝔄𝔅ℭ𝔇𝔈𝔉𝔊ℌℑ𝔍𝔎𝔏𝔐𝔑𝔒𝔓𝔔ℜ𝔖𝔗𝔘𝔙𝔚𝔛𝔜ℨ𝔞𝔟𝔠𝔡𝔢𝔣𝔤𝔥𝔦𝔧𝔨𝔩𝔪𝔫𝔬𝔭𝔮𝔯𝔰𝔱𝔲𝔳𝔴𝔵𝔶𝔷"
      it "0-9 (char)" do
        transform <$> digits `shouldBe` (Nothing <$ digits)
    describe "Bold" do
      let transform = fraktur Bold
      it "A-Za-z" do
        transform <$> alpha `shouldBe` Just <$> "𝕬𝕭𝕮𝕯𝕰𝕱𝕲𝕳𝕴𝕵𝕶𝕷𝕸𝕹𝕺𝕻𝕼𝕽𝕾𝕿𝖀𝖁𝖂𝖃𝖄𝖅𝖆𝖇𝖈𝖉𝖊𝖋𝖌𝖍𝖎𝖏𝖐𝖑𝖒𝖓𝖔𝖕𝖖𝖗𝖘𝖙𝖚𝖛𝖜𝖝𝖞𝖟"
      it "0-9 (char)" do
        transform <$> digits `shouldBe` (Nothing <$ digits)
  describe "monospace" do
    it "A-Za-z" do
      monospace <$> alpha `shouldBe` Just <$> "𝙰𝙱𝙲𝙳𝙴𝙵𝙶𝙷𝙸𝙹𝙺𝙻𝙼𝙽𝙾𝙿𝚀𝚁𝚂𝚃𝚄𝚅𝚆𝚇𝚈𝚉𝚊𝚋𝚌𝚍𝚎𝚏𝚐𝚑𝚒𝚓𝚔𝚕𝚖𝚗𝚘𝚙𝚚𝚛𝚜𝚝𝚞𝚟𝚠𝚡𝚢𝚣"
    it "0-9 (char)" do
      monospace <$> digits `shouldBe` Just <$> "𝟶𝟷𝟸𝟹𝟺𝟻𝟼𝟽𝟾𝟿"
    it "0-9 (int)" do
      intToDigitMonospace <$> [0..9] `shouldBe` Just <$> "𝟶𝟷𝟸𝟹𝟺𝟻𝟼𝟽𝟾𝟿"
  where
    alpha :: String
    alpha = ['A'..'Z'] <> ['a'..'z']
    digits :: String
    digits = ['0'..'9']
