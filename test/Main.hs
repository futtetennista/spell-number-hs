{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as H
import SignalAI.Translate (numberToLetters)
import Data.Either (isRight, isLeft, fromRight)
import Data.Coerce (coerce)
import qualified Data.Text as Tx

main :: IO ()
main = T.defaultMain tests

newtype Valid = Valid Int
  deriving (Eq, Ord, Show, Num)

instance QC.Arbitrary Valid where
  arbitrary = Valid <$> QC.choose(1, 1000)

newtype Invalid = Invalid Int
  deriving (Eq, Ord, Show, Num)

instance QC.Arbitrary Invalid where
  arbitrary = Invalid <$> QC.oneof [QC.choose(1001, 10000), QC.choose(-1000, 0)]

newtype Hundreds = Hundreds Int
  deriving (Eq, Ord, Show, Num)

instance QC.Arbitrary Hundreds where
  arbitrary = Hundreds <$> QC.choose(100, 999)

newtype LessThanHundred = LessThanHundred Int
  deriving (Eq, Ord, Show, Num)

instance QC.Arbitrary LessThanHundred where
  arbitrary = LessThanHundred <$> QC.choose(1, 99)

tests :: T.TestTree
tests =
  T.testGroup "Tests" [
    H.testCase "Test it returns a translation" do
      numberToLetters 100 H.@?= Right "one hundred",

    QC.testProperty "Valid number is always translated"
      (isRight . numberToLetters . coerce @Valid @Int),

    QC.testProperty "Invalid number is never translated"
      (isLeft . numberToLetters . coerce @Invalid @Int),

    QC.testProperty "'and' appears at most once" \n ->
      let translation = fromRight "unexpected" $ numberToLetters (coerce @Valid n)
          ands = filter (== "and") $ Tx.words translation
       in null ands || length ands == 1,

    QC.testProperty "The word 'hundred' appears when it should" \n ->
      let translation = fromRight "unexpected" $ numberToLetters $ coerce @Hundreds n
       in filter (== "hundred") (Tx.words translation) == ["hundred"],

    QC.testProperty "The word 'hundred' never appears when it should not" \n ->
      let translation = fromRight "unexpected" $ numberToLetters $ coerce @LessThanHundred n
       in "hundred" `notElem` Tx.words translation
  ]
