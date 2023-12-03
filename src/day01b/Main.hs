module Main where

import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text.Lazy (anyChar, endOfInput, endOfLine, manyTill, parseTest, satisfy, string)
import Data.Char (digitToInt, isDigit)
import Data.Text.Lazy.IO (getContents)

main :: IO ()
main = parseTest p =<< getContents
  where
    p = sum <$> manyTill l endOfInput
    l =
      uncurry (+)
        . first (* 10)
        . (head &&& last)
        . fromMaybe (pure 0)
        . nonEmpty
        . catMaybes
        <$> manyTill n endOfLine
    n =
      Just
        <$> (digitWord <|> numericDigit)
        <|> Nothing <$ anyChar
    digitWord =
      word 0 "zero"
        <|> word 1 "one"
        <|> word 2 "two"
        <|> word 3 "three"
        <|> word 4 "four"
        <|> word 5 "five"
        <|> word 6 "six"
        <|> word 7 "seven"
        <|> word 8 "eight"
        <|> word 9 "nine"
    word i s = i <$ (lookAhead (string s) *> anyChar)
    numericDigit = digitToInt <$> satisfy isDigit
