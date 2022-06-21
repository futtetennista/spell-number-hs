{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import SpellNumber (spellNumber)
import Prelude
import Text.Read (readMaybe)
import qualified Data.Text as Tx

main :: IO ()
main = loop
  where
    loop = do
      putStrLn "Enter a number:"
      maybeNumber <- readMaybe @Int <$> getLine
      case maybeNumber of
        Nothing -> putStrLn "Please enter a number between [1, 1000]"
        Just n -> either putText putText (spellNumber n)
      loop

    putText = putStrLn . Tx.unpack
