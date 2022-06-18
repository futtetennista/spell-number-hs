{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Translator (numberToLetters) where

import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Tx

numberToLetters :: Int -> Either Text Text
numberToLetters input = do
  _ <- validateInput (1, 1_000) input
  pure $ translateInput input

validateInput :: (Int, Int) -> Int -> Either Text ()
validateInput (low, high) input
  | input >= low && input <= high = Right ()
  | otherwise =
    Left $
      Tx.unwords
        ["Invalid input. Number must be between",
         Tx.pack $ show low,
         "and",
         Tx.pack $ show high
        ]

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
  type Input a :: Type

  translate :: Input a -> Text

instance Translate ThousandsT where
  type Input ThousandsT = Digit

  translate :: Digit -> Text
  translate = \case
    Zero -> ""
    d -> translate @UnitT (d, False) <> " thousand"

instance Translate HundredsT where
  type Input HundredsT = Digit

  translate :: Digit -> Text
  translate = \case
    Zero -> ""
    d -> translate @UnitT (d, False) <> " hundred"

instance Translate TensT where
  type Input TensT = (Digit, Digit)

  translate :: (Digit, Digit) -> Text
  translate (d, u) =
    case d of
      Zero -> ""
      One -> toTens u
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

  translate :: (Digit, Bool) -> Text
  translate (d, translateZero) = case d of
    Zero -> if translateZero then "zero" else ""
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

      -- This pattern match is safe because the input will always be non-empty
      (unitDigit, _) = last inputParsed

      translate' :: (Maybe Units, Bool, Text) -> (Digit, Units) -> (Maybe Units, Bool, Text)
      translate' (previousUnits, skipSingleDigit, n) (d, u) =
        case u of
          Thousands ->
            ( if d /= Zero then Just Thousands else Nothing,
              skipSingleDigit,
              translate @ThousandsT d
            )
          Hundreds ->
            ( if d /= Zero
                then maximum [previousUnits, Just Hundreds]
                else previousUnits,
              skipSingleDigit,
              case previousUnits of
                Nothing -> n <> translate @HundredsT d
                Just _ -> if d /= Zero then n <> " " <> translate @HundredsT d else n
            )
          Tens
            | d == Zero -> (previousUnits, skipSingleDigit, n)
            | otherwise ->
              ( Just Tens,
                d == One,
                case previousUnits of
                  Nothing -> translate @TensT (d, unitDigit)
                  Just _ -> n <> " and " <> translate @TensT (d, unitDigit)
              )
          Unit ->
            if skipSingleDigit
              then (previousUnits, skipSingleDigit, n)
              else
                ( previousUnits,
                  skipSingleDigit,
                  n <> case previousUnits of
                    Nothing -> translate @UnitT (d, True)
                    Just Tens -> letteriseIfNotZero "-" d
                    Just Hundreds -> letteriseIfNotZero " and " d
                    Just Thousands -> letteriseIfNotZero " and " d
                    -- It cannot happen but has to be included for the pattern-match
                    -- to be exhaustive
                    Just Unit -> ""
                )

      letteriseIfNotZero delim = \case
        Zero -> ""
        d -> delim <> translate @UnitT (d, False)

      (_, _, inputTranslated) = foldl translate' (Nothing, False, "") inputParsed
   in inputTranslated

-- PARSER
class AsPowerOfTen a where
  asPowerOfTen :: a -> Int

instance AsPowerOfTen Units where
  asPowerOfTen = \case
    Unit -> 1
    Tens -> 10
    Hundreds -> 100
    Thousands -> 1_000

-- | Parse a number into a list of digits and units
-- e.g. parseNumber 1000 = [(One, Thousands), (Zero, Hundreds), (Zero, Tens), (Zero Unit)]
-- e.g. parseNumber 1 = [(Zero, Thousands), (Zero, Hundreds), (Zero, Tens), (One Unit)]
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
