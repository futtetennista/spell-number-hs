{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MyLib (translate) where

import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Tx

validateInput :: (Int, Int) -> Int -> Either Text Text
validateInput (low, high) input
  | input >= low && input <= high = Right $ translateInput input
  | otherwise =
    Left $
      Tx.unwords ["Invalid input. Number must be between", Tx.pack $ show low, "and", Tx.pack $ show high]

-- TYPES
data Units = Unit | Tens | Hundreds | Thousands
  deriving (Show, Eq, Ord, Bounded, Enum)

-- Types representing units. Useful to create type class instances.
data UnitT = UnitT

data TensT = TensT

data HundredsT = HundredsT

data ThousandsT = ThousandsT

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Show, Eq, Ord, Bounded, Enum)

class Translate a where
  type Input a
  translate :: Input a -> Text

instance Translate ThousandsT where
  type Input ThousandsT = Digit
  translate = \case
    Zero -> ""
    d -> translate @UnitT (d, False) <> " thousand"

instance Translate HundredsT where
  type Input HundredsT = Digit
  translate = \case
    Zero -> ""
    d -> translate @UnitT (d, False) <> " hundred"

instance Translate TensT where
  type Input TensT = (Digit, Digit)
  translate (tens, unit) =
    case tens of
      Zero -> "" -- translate @Unit unit
      One -> toTens unit
      Two -> "twenty"
      Three -> "thirty"
      Four -> "fourty"
      Five -> "fifty"
      Six -> "sixty"
      Seven -> "seventy"
      Eight -> "eigthty"
      Nine -> "ninety"
    where
      toTens = \case
        Zero -> "ten"
        One -> "eleven"
        Two -> "twelve"
        Three -> "thirteen"
        Four -> "fourteen"
        Five -> "fifteen"
        Six -> "sixten"
        Seven -> "seventeen"
        Eight -> "eigthteen"
        Nine -> "nineteen"

instance Translate UnitT where
  type Input UnitT = (Digit, Bool)
  translate (d, showZero) = case d of
    Zero -> if showZero then "zero" else ""
    One -> "one"
    Two -> "two"
    Three -> "three"
    Four -> "four"
    Five -> "five"
    Six -> "six"
    Seven -> "seven"
    Eight -> "eigth"
    Nine -> "nine"

-- TRANSLATOR
translateInput :: Int -> Text
translateInput input =
  let inputParsed = parseNumber input

      (unitDigit, _) = last inputParsed

      translate' :: (Maybe Units, Bool, Text) -> (Digit, Units) -> (Maybe Units, Bool, Text)
      translate' (munit, translateUnit, n) (d, u) = case u of
        Thousands ->
          ( if d /= Zero then Just Thousands else Nothing,
            translateUnit,
            translate @ThousandsT d
          )
        Hundreds ->
          ( if d /= Zero
              then maximum [munit, Just Hundreds]
              else munit,
            translateUnit,
            n <> (if n /= "" && d /= Zero then " " else "") <> translate @HundredsT d
          )
        Tens
          | d == Zero -> (munit, translateUnit, n)
          | otherwise ->
            ( Just Tens,
              d /= One,
              case munit of
                Nothing -> translate @TensT (d, unitDigit)
                Just _ -> n <> " and " <> translate @TensT (d, unitDigit)
            )
        Unit ->
          if translateUnit
            then
              ( munit,
                translateUnit,
                n <> case munit of
                  Nothing -> translate @UnitT (d, True)
                  Just Tens -> letteriseIfNotZero "-" d
                  Just Hundreds -> letteriseIfNotZero " and " d
                  Just Thousands -> letteriseIfNotZero " and " d
                  -- It cannot happen but has to be included for the pattern-match
                  -- to be exhaustive
                  Just Unit -> ""
              )
            else (munit, translateUnit, n)

      letteriseIfNotZero delim = \case
        Zero -> ""
        d -> delim <> translate @UnitT (d, False)

      (_, _, inputTranslated) = foldl translate' (Nothing, True, "") inputParsed
   in inputTranslated

-- PARSER
class AsPowerOfTen a where
  asPowerOfTen :: a -> Int

instance AsPowerOfTen Units where
  asPowerOfTen = \case
    Unit -> 1
    Tens -> 10
    Hundreds -> 100
    Thousands -> 1000

parseNumber :: Int -> [(Digit, Units)]
parseNumber input = parseNumber' [] input Thousands
  where
    parseNumber' :: [(Digit, Units)] -> Int -> Units -> [(Digit, Units)]
    parseNumber' res x unit =
      let (quot, rem) = x `divMod` asPowerOfTen unit
          res' = res <> [(toEnum @Digit quot, unit)]
       in if minBound == unit
            then res'
            else parseNumber' res' rem (pred unit)
