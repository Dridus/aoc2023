{-# LANGUAGE StrictData #-}

module Main where

import Control.Lens (each, sumOf, to, _2)
import Data.ByteString.Char8 (hGetContents, index, lines, readInt, strip)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit)
import Data.IntMap.Strict qualified as IntMap
import Data.List (partition)
import Data.List.NonEmpty qualified as NEL
import System.IO (openFile)
import Text.Show qualified as TS
import Prelude hiding (lines)

type Coord = (Int, Int)

data Span = Span {spanStart :: Int, spanEnd :: Int, spanY :: Int}
  deriving stock (Eq)

instance TS.Show Span where
  show Span {..} = "(" <> show spanStart <> ".." <> show spanEnd <> "," <> show spanY <> ")"

spanLeft :: Span -> Coord
spanLeft (Span x _ y) = (x, y)

main :: IO ()
main = do
  [filePath] <- getArgs
  ((width, height), input) <- load filePath

  let offs (x, y) = y * width + x
  let at = index input . offs
  let neighborhood Span {..} =
        filter (\(x, y) -> x >= 0 && x < width && y >= 0 && y < height) $
          [(x, spanY - 1) | x <- [spanStart - 1 .. spanEnd + 1]]
            <> [(x, spanY + 1) | x <- [spanStart - 1 .. spanEnd + 1]]
            <> [(spanStart - 1, spanY), (spanEnd + 1, spanY)]

  let numberSpans :: [Span]
      numberSpans =
        [0 .. height - 1] >>= \y ->
          fmap (\xs -> Span (fst $ head xs) (fst $ last xs) y)
            . filter (snd . head)
            . NEL.groupBy ((==) `on` snd)
            . fmap (id &&& (isDigit . at . (,y)))
            $ (0 :| [1 .. (width - 1)])

  let partNumbers :: [(Span, Int, [Coord])]
      partNumbers =
        numberSpans >>= \s ->
          let (gearSymbols, otherSymbols) =
                bimap (fmap fst) (fmap fst)
                  . partition ((== '*') . snd)
                  . filter (\(_, c) -> c /= '.' && not (isDigit c))
                  . fmap (id &&& at)
                  . neighborhood
                  $ s
              n =
                fst $
                  fromMaybe
                    (error $ "part number which isn't numeric at " <> show s)
                    (readInt . flip BS.drop input . offs . spanLeft $ s)
           in [(s, n, gearSymbols) | not (null gearSymbols && null otherSymbols)]

  let validGears =
        IntMap.mapMaybe
          ( \case
              [a, b] -> Just (a, b)
              _ -> Nothing
          )
          . IntMap.fromListWith (<>)
          . concatMap (\(_, n, gs) -> (,pure n) . offs <$> gs)
          $ partNumbers

  print numberSpans
  print partNumbers
  print validGears

  putStrLn "---"

  putStrLn $ "sum of part numbers: " <> show (sumOf (each . _2) partNumbers)
  putStrLn $ "sum of gear 'ratios': " <> show (sumOf (each . to (uncurry (*))) validGears)

load :: FilePath -> IO ((Int, Int), ByteString)
load path = do
  f <- openFile path ReadMode
  ls <- maybe (fail "empty input") pure . nonEmpty . filter ((/= "") . strip) . lines =<< hGetContents f
  let width = BS.length (head ls)
  let height = length ls
  pure ((width, height), sconcat ls)

indexes :: NonEmpty Int
indexes = 0 :| [1 ..]
