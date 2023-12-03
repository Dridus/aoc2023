module Main where

import Control.Lens (each, filtered, folded, sumOf, view, _1, _2, _3, to)
import Control.Lens.TH (makeClassy)
import Data.Attoparsec.Combinator (sepBy)
import Data.Attoparsec.Text.Lazy
  ( Parser,
    char,
    decimal,
    endOfInput,
    endOfLine,
    parseOnly,
    skipSpace,
    string,
  )
import Data.Text.Lazy.IO (getContents)
import Data.Typeable (typeRep)
import Text.Show qualified as TS

data Observation = Observation {_red :: Int, _green :: Int, _blue :: Int}
  deriving stock (Eq)

pattern Red, Green, Blue :: Int -> Observation
pattern Red r <- Observation r _ _ where Red r = Observation r 0 0
pattern Green g <- Observation _ g _ where Green r = Observation 0 r 0
pattern Blue b <- Observation _ _ b where Blue r = Observation 0 0 r

instance Ord Observation where
  Observation ra ga ba <= Observation rb gb bb = ra <= rb && ga <= gb && ba <= bb

instance Show Observation where
  show (Observation r g b) = intercalate ", " . catMaybes $ [f "red" r, f "green" g, f "blue" b]
    where
      f _ 0 = Nothing
      f s n = Just $ show n <> " " <> s

instance Semigroup Observation where
  Observation ra ga ba <> Observation rb gb bb =
    Observation (ra `max` rb) (ga `max` gb) (ba `max` bb)

instance Monoid Observation where
  mempty = Observation 0 0 0

observationPower :: Observation -> Int
observationPower (Observation r g b) = r * g * b

data Game = Game
  { _gameNo :: Int,
    _observations :: NonEmpty Observation
  }
  deriving stock (Eq)

instance Show Game where
  show (Game n obs) = "Game " <> show n <> ": " <> (intercalate "; " . fmap show . toList $ obs)

makeClassy ''Game

colorP :: Parser (Int -> Observation)
colorP = Red <$ string "red" <|> Green <$ string "green" <|> Blue <$ string "blue"

qtyColorP :: Parser Observation
qtyColorP = flip ($) <$> decimal <*> (skipSpace *> colorP)

nonEmptyP :: forall a. Typeable a => Parser [a] -> Parser (NonEmpty a)
nonEmptyP = (=<<) $ maybe (fail $ "empty list of " <> show (typeRep (Proxy @a))) pure . nonEmpty

observationP :: Parser Observation
observationP = fmap sconcat . nonEmptyP $ (skipSpace *> qtyColorP) `sepBy` char ','

observationsP :: Parser (NonEmpty Observation)
observationsP = nonEmptyP $ (skipSpace *> observationP) `sepBy` char ';'

gameP :: Parser Game
gameP = Game <$> (string "Game" *> skipSpace *> decimal) <*> (char ':' *> observationsP)

main :: IO ()
main = do
  let limit = Observation 12 13 14
  games <-
    either (fail . show) (pure . catMaybes)
      . parseOnly ((optional gameP `sepBy` endOfLine) <* endOfInput)
      =<< getContents
  let summedGames =
        games <&> \g ->
          let obsum = view (observations . folded) g
           in (g, obsum, obsum <= limit)
  for_ summedGames $ \(g, obsum, possible) ->
    putStrLn $
      bool "" "Possible " possible
        <> show g
        <> " => "
        <> show obsum
        <> ", "
        <> show (observationPower obsum)
        <> " power"
  print $ sumOf (each . filtered (view _3) . _1 . gameNo) summedGames
  print $ sumOf (each . _2 . to observationPower) summedGames
