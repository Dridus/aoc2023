module Main where

import Control.Lens (each, sumOf, to)
import Data.IntSet qualified as IntSet
import Text.Trifecta (Parser, char, decimal, eof, newline, parseFromFile, sepEndBy1, skipSome, symbol, (<?>))
import qualified Data.IntMap.Strict as IntMap

data Card = Card {cardNo :: Int, cardWinning :: IntSet, cardDraw :: IntSet}
  deriving stock (Show)

cardMatches :: Card -> Int
cardMatches Card{..}= IntSet.size $ cardWinning `IntSet.intersection` cardDraw

sp :: Parser ()
sp = skipSome (char ' ') <?> "spaces"

parseCard :: Parser Card
parseCard =
  Card
    <$> (symbol "Card" *> (fromIntegral <$> decimal) <* char ':')
    <*> (sp *> (IntSet.fromList <$> ((fromIntegral <$> decimal) `sepEndBy1` sp)))
    <*> (char '|' *> sp *> (IntSet.fromList <$> ((fromIntegral <$> decimal) `sepEndBy1` sp)))

parseCards :: Parser [Card]
parseCards = (parseCard `sepEndBy1` newline) <* eof

scoreA :: Card -> Int
scoreA = truncate @Double . (2 **) . fromIntegral . pred . cardMatches

scoreB :: [Card] -> IntMap Int
scoreB cards0 = go (IntMap.fromList ((,1 :: Int) . cardNo <$> cards0)) mempty cards0
 where
  go accum res cards@(card@Card{..}:rest)
    | IntMap.member cardNo accum =
      let
        accum' = foldl' (\ m k -> IntMap.insertWith (+) k 1 m) accum [cardNo+1..cardNo+cardMatches card]
        accum'' = IntMap.update (mfilter (> 0) . Just . pred) cardNo accum'
      in
        go accum'' (IntMap.insertWith (+) cardNo 1 res) cards
    | otherwise = go accum res rest
  go _ res [] = res

main :: IO ()
main = do
  [inputPath] <- getArgs
  cards <- fromMaybe (error "whoops, eh") <$> parseFromFile parseCards inputPath
  print . sumOf (each . to scoreA) $ cards
  let scoredB = scoreB cards
  print scoredB
  print $ sum scoredB

