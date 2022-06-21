{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module SpellNumber (spellNumber) where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Tx

spellNumber :: Int -> Either Text Text
spellNumber input =
  spellNumber' input <$ validateInput (1, 1_000) input

validateInput :: (Int, Int) -> Int -> Either Text ()
validateInput (low, high) input
  | input >= low && input <= high = Right ()
  | otherwise =
    Left $
      Tx.unwords
        [ "Invalid input. Number must be between",
          Tx.pack $ show low,
          "and",
          Tx.pack $ show high
        ]

-- TYPES
data Units = Unit | Tens | Hundreds | Thousands
  deriving (Show, Eq, Ord, Bounded, Enum)

-- Unhabited types representing units. Useful to create type class instances.
data UnitT

data TensT

data HundredsT

data ThousandsT

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Show, Eq, Ord, Bounded, Enum)

class SpellUnit a where
  type Input a :: Type

  spellUnit :: Input a -> Text

instance SpellUnit ThousandsT where
  type Input ThousandsT = Digit

  spellUnit :: Digit -> Text
  spellUnit = \case
    Zero -> ""
    d -> spellUnit @UnitT (d, False) <> " thousand"

instance SpellUnit HundredsT where
  type Input HundredsT = Digit

  spellUnit :: Digit -> Text
  spellUnit = \case
    Zero -> ""
    d -> spellUnit @UnitT (d, False) <> " hundred"

instance SpellUnit TensT where
  type Input TensT = (Digit, Digit)

  spellUnit :: (Digit, Digit) -> Text
  spellUnit (d, u) =
    case d of
      Zero -> ""
      One -> toTens u
      Two -> appendUnitIfNotZero "twenty"
      Three -> appendUnitIfNotZero "thirty"
      Four -> appendUnitIfNotZero "fourty"
      Five -> appendUnitIfNotZero "fifty"
      Six -> appendUnitIfNotZero "sixty"
      Seven -> appendUnitIfNotZero "seventy"
      Eight -> appendUnitIfNotZero "eigthty"
      Nine -> appendUnitIfNotZero "ninety"
    where
      appendUnitIfNotZero t =
        if u == Zero
          then t
          else t <> "-" <> spellUnit @UnitT (u, False)

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

instance SpellUnit UnitT where
  type Input UnitT = (Digit, Bool)

  spellUnit :: (Digit, Bool) -> Text
  spellUnit (d, spellZero) = case d of
    Zero -> if spellZero then "zero" else ""
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
spellNumber' :: Int -> Text
spellNumber' =
  (\x -> if x == "" then "zero" else x) . snd . foldr spellNumberR ([], Tx.empty) . parseNumber

spellNumberR :: (Digit, Units) -> ([Digit], Text) -> ([Digit], Text)
spellNumberR (d, u) (prevDigit, xs) =
  case u of
    Unit -> ([d], spellUnit @UnitT (d, False))
    Tens -> ([d], if d == Zero then xs else spellUnit @TensT (d, head prevDigit))
    Hundreds -> ([d], let x = spellUnit @HundredsT d in join False x xs)
    Thousands -> ([d], let x = spellUnit @ThousandsT d in join (prevDigit /= [Zero]) x xs)
  where
    join usingSpace x y
      | x == "" = y
      | y == "" = x
      | usingSpace = x <> " " <> y
      | otherwise = x <> " and " <> y

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
      let (quotient, remainder) = x `divMod` asPowerOfTen unit
          res' = res <> [(toEnum @Digit quotient, unit)]
       in if minBound == unit
            then res'
            else parseNumber' res' remainder (pred unit)
