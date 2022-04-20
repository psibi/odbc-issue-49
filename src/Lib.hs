{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( query,
  )
where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.ODBC.Internal (Binary, Connection, ODBCException (..), Step (..), Value (..))
import qualified Database.ODBC.Internal as Internal
import System.Environment (lookupEnv)

connectWithString :: IO Connection
connectWithString = do
  mconnStr <- lookupEnv "ODBC_TEST_CONNECTION_STRING"
  case mconnStr of
    Just connStr
      | not (null connStr) -> Internal.connect (T.pack connStr)
    _ ->
      error
        "Need ODBC_TEST_CONNECTION_STRING environment variable.\n\
        \Example:\n\
        \ODBC_TEST_CONNECTION_STRING='DRIVER={ODBC Driver 17 for SQL Server};SERVER=127.0.0.1;Uid=SA;Pwd=Passw0rd;Encrypt=no'"

setupTable :: IO Connection
setupTable = do
  conn <- connectWithString
  Internal.exec conn "DROP TABLE IF EXISTS wibble"
  Internal.exec conn "CREATE TABLE wibble (tt1 nvarchar, tt2 varchar(50))"
  Internal.exec conn "insert into wibble (tt1, tt2) values ('£', '£')"
  pure conn

dropTable :: Connection -> IO ()
dropTable = Internal.close

issue49 :: Connection -> IO ()
issue49 conn = do
  vals <- Internal.query conn "SELECT tt1, tt2 from wibble"
  print vals
  mapM_ printValues (getValues vals)

getValues :: [[(Internal.Column, Internal.Value)]] -> [Internal.Value]
getValues xs = concat $ map (map snd) xs

printValues :: Internal.Value -> IO ()
printValues (Internal.ByteStringValue bs) = print bs
printValues (Internal.TextValue txt) = T.putStrLn txt

query :: IO ()
query = bracket setupTable dropTable issue49
