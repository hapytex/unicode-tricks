{-# LANGUAGE Safe #-}

module Data.Char.Number.Egyptian where

import Data.Char.Core(Ligate, PlusStyle(WithPlus, WithoutPlus))

singleStroke :: Char
singleStroke = '\x133fa'

cattleHobble :: Char
cattleHobble = '\x13386'

coilOfRope :: Char
coilOfRope = '\x13362'

waterLily :: Char
waterLily = '\x131bc'

bentFinger :: Char
bentFinger = '\x130ad'

tadpole :: Char
tadpole = '\x13190'

heh :: Char
heh = '\x13068'

plus :: Char
plus = '\x130bd'

minus :: Char
minus = '\x130bb'

-- egyptianNumber :: Ligate -> PlusStyle -> Int -> Text
-- egyptianNumber Ligate
-- egyptianNumber NoLigate 
