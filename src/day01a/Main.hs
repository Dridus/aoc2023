module Main where

import Control.Exception (try, IOException)
import Control.Lens (preview, _head, _last)
import Data.Char (isDigit, digitToInt)
import Data.Text (unpack)

main :: IO ()
main = print <=< flip fix 0 $ \continue accum ->
  either (const @_ @IOException $ pure accum) (continue . (+) accum . parse) =<< try getLine
  where
    viewOr0 = fmap (maybe 0 digitToInt) . preview
    parse (filter isDigit . unpack -> digits) = 10 * viewOr0 _head digits + viewOr0 _last digits
