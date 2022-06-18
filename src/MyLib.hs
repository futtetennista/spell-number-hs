{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module MyLib (translate)
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Kind (Type)
import Data.Functor ((<&>))

data Units = Unit | Tens | Hundreds | Thousands
  deriving (Show, Eq, Ord, Bounded, Enum)

data Unit = Unit_
data Tens = Tens_
data Hundreds = Hundreds_
data Thousands = Thousands_

class ToLetters a where
  type I a
  toLetters :: I a -> Text

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Show, Eq, Ord, Bounded, Enum)

instance ToLetters Thousands where
  type I Thousands = Digit
  toLetters = \case
    Zero -> ""
    d -> toLetters @Unit (d, False) <> " thousand"

instance ToLetters Hundreds where
  type I Hundreds = Digit
  toLetters = \case
    Zero -> ""
    d -> toLetters @Unit (d, False ) <> " hundred"

instance ToLetters Tens where
  type I Tens = (Digit, Digit)

  toLetters (tens, unit) =
    case tens of
      Zero -> "" -- toLetters @Unit unit
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

instance ToLetters Unit where
  type I Unit = (Digit, Bool)
  toLetters (d, showZero) = case d of
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

translate :: Int -> Text
translate input =
  let inputParsed = toUnits input

      (unitDigit, _) = last inputParsed

      translate' :: (Maybe Units, Bool, Text) -> (Digit, Units) -> (Maybe Units, Bool, Text)
      translate' (isBig, translateUnit, n) (d, u) = case u of
            Thousands -> (if d /= Zero then Just Thousands else Nothing,
                          translateUnit,
                          toLetters @Thousands d
                         )
            Hundreds -> (if d /= Zero
                         then maximum [isBig, Just Hundreds]
                         else isBig,
                         translateUnit,
                         n <> (if n /= "" && d /= Zero then " " else "") <> toLetters @Hundreds d
                        )
            Tens -> case toLetters @Tens (d, unitDigit) of
              "" -> (isBig, translateUnit, n)
              x -> (Just Tens,
                    d /= One,
                    case isBig of
                      Nothing -> x
                      Just _ -> n <> " and " <> x
                   )
            Unit ->
              if translateUnit
                then
                  (isBig,
                   translateUnit,
                   n <> case isBig of
                     Nothing -> toLetters @Unit (d, True)
                     Just Tens -> letteriseIfNotZero "-" d
                     Just Hundreds -> letteriseIfNotZero " and " d
                     Just Thousands -> letteriseIfNotZero " and " d
                  )
                else (isBig, translateUnit, n)

      letteriseIfNotZero delim = \case
        Zero -> ""
        d -> delim <> toLetters @Unit (d, False)

      (_, _, n) = foldl translate' (Nothing, True, T.empty) inputParsed
  in n

class ToPowerOfTen a where
  toPowerOfTen :: a -> Int

instance ToPowerOfTen Units where
  toPowerOfTen = \case
    Unit -> 1
    Tens -> 10
    Hundreds -> 100
    Thousands -> 1000

toUnits :: Int -> [(Digit, Units)]
toUnits input = toUnits' [] input Thousands
  where
    toUnits' :: [(Digit, Units)] -> Int -> Units -> [(Digit, Units)]
    toUnits' res x unit =
      let (quot, rem) = x `divMod` toPowerOfTen unit
          res' = res <> [(toEnum @Digit quot, unit)]

      in if minBound == unit
        then res'
        else toUnits' res' rem (pred unit)
