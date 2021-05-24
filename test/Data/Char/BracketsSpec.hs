{-# LANGUAGE TypeApplications #-}

module Data.Char.BracketsSpec where

import Data.Char.CoreTest
import Data.Char.Brackets
import Data.List(nub)

import Test.Hspec
import Test.QuickCheck(property)

spec :: Spec
spec = describe "Brackets" $ do
    it "double opposite is id" (property mappingOpposite)
    it "a bracket can not be both have as type open and close" (property notBothTypes)
    it "if it is a bracket, it is open or closed" (property ifABracketOpenOrClose)
    it "if it is a bracket, it is a member of brackets" (property checkIfBracketWithMembership)
    it "if it is an opening bracket, it is a member of brackets" (property checkIfOpenBracketWithMembership)
    it "if it is a closing bracket, it is a member of brackets" (property checkIfCloseBracketWithMembership)
    it "check if all brackets are unique" (length brackets == length (nub brackets))
    testHashable @ BracketType


mappingOpposite :: Char -> Bool
mappingOpposite c = c == getOppositeChar' (getOppositeChar' c)

notBothTypes :: Char -> Bool
notBothTypes c = not (isOpenBracket c) || not (isCloseBracket c)

ifABracketOpenOrClose :: Char -> Bool
ifABracketOpenOrClose c = not (isBracket c) || isOpenBracket c || isCloseBracket c

checkIfBracketWithMembership :: Char -> Bool
checkIfBracketWithMembership c = (c `elem` brackets) == isBracket c

checkIfOpenBracketWithMembership :: Char -> Bool
checkIfOpenBracketWithMembership c = (c `elem` openBrackets) == isOpenBracket c

checkIfCloseBracketWithMembership :: Char -> Bool
checkIfCloseBracketWithMembership c = (c `elem` closeBrackets) == isCloseBracket c
