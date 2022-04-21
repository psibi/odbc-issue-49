{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( query2,
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
import Database.ODBC.SQLServer
import Data.Text.Encoding (encodeUtf8)

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
  pure conn

dropTable :: Connection -> IO ()
dropTable = Internal.close

renderVarcharText :: Text -> Text
renderVarcharText = renderValue . TextValue


selectRows :: Connection -> IO ()
selectRows conn = do
  vals <- Internal.query conn "SELECT tt1, tt2 from wibble"
  print vals
  mapM_ printValues (getValues vals)

issue49_2 :: Connection -> IO ()
issue49_2 conn = do
  let que = "INSERT into wibble (tt1, tt2) values (" <> toSql pound <> "," <> toSql pound <> ")"
      pound :: Text
      pound = "\632"
  T.putStrLn $ renderQuery que
  Internal.query conn $ renderQuery que
  selectRows conn
  selectWhere conn

selectWhere :: Connection -> IO ()
selectWhere conn = do
  putStrLn "selectWhere"
  let where_query_tt1 = "SELECT tt1, tt2 FROM WIBBLE where tt1 = " <> toSql pound
      where_query_tt2 = "SELECT tt1, tt2 FROM WIBBLE where tt2 = " <> toSql pound
      pound :: Text
      pound = "\632"
  T.putStrLn $ renderQuery where_query_tt1
  vals <- Internal.query conn $ renderQuery where_query_tt1
  print vals
  T.putStrLn $ renderQuery where_query_tt2
  vals <- Internal.query conn $ renderQuery where_query_tt2
  print vals

getValues :: [[(Internal.Column, Internal.Value)]] -> [Internal.Value]
getValues xs = concat $ map (map snd) xs

printValues :: Internal.Value -> IO ()
printValues (Internal.ByteStringValue bs) = print bs
printValues (Internal.TextValue txt) = T.putStrLn txt

query2 :: IO ()
query2 = bracket setupTable dropTable issue49_2
