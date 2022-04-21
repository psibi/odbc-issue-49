module Main where

import Lib
import GHC.IO.Encoding

main :: IO ()
main = do
  setLocaleEncoding utf8
  query2
