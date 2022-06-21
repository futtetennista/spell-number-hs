{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Coerce (coerce)
import Data.Either (fromRight, isLeft, isRight)
import qualified Data.Text as Tx
import SpellNumber (spellNumber)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = T.defaultMain tests

newtype Valid = Valid Int
  deriving (Eq, Ord, Show, Num)

instance QC.Arbitrary Valid where
  arbitrary = Valid <$> QC.choose (1, 1000)

newtype Invalid = Invalid Int
  deriving (Eq, Ord, Show, Num)

instance QC.Arbitrary Invalid where
  arbitrary = Invalid <$> QC.oneof [QC.choose (1001, 10000), QC.choose (-1000, 0)]

newtype Hundreds = Hundreds Int
  deriving (Eq, Ord, Show, Num)

instance QC.Arbitrary Hundreds where
  arbitrary = Hundreds <$> QC.choose (100, 999)

newtype LessThanHundred = LessThanHundred Int
  deriving (Eq, Ord, Show, Num)

instance QC.Arbitrary LessThanHundred where
  arbitrary = LessThanHundred <$> QC.choose (1, 99)

tests :: T.TestTree
tests =
  T.testGroup
    "Tests"
    [ H.testCase "Test it returns a spelled number" do
        spellNumber 100 H.@?= Right "one hundred",
      QC.testProperty
        "Valid number is always spelled"
        (isRight . spellNumber . coerce @Valid @Int),
      QC.testProperty
        "Invalid number is never spelled"
        (isLeft . spellNumber . coerce @Invalid @Int),
      QC.testProperty "'and' appears at most once" \n ->
        let spelledNumber = fromRight "unexpected" $ spellNumber (coerce @Valid n)
            ands = filter (== "and") $ Tx.words spelledNumber
         in null ands || length ands == 1,
      QC.testProperty "The word 'hundred' appears when it should" \n ->
        let spelledNumber = fromRight "unexpected" $ spellNumber $ coerce @Hundreds n
         in filter (== "hundred") (Tx.words spelledNumber) == ["hundred"],
      QC.testProperty "The word 'hundred' never appears when it should not" \n ->
        let spelledNumber = fromRight "unexpected" $ spellNumber $ coerce @LessThanHundred n
         in "hundred" `notElem` Tx.words spelledNumber
    ]
