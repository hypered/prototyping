-- This script offers a few sub-commands that are used by the Makefile to build
-- the `site/` directory. Some of the sub-commands are used to populate the
-- SQLite database `prototype.db`, some other are used to read the database and
-- generate HTML files.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (foldM)
import Control.Monad.Extra (partitionM)
import Control.Applicative
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs, getEnv)
import System.FilePath (takeExtension, (</>), FilePath)
import System.Posix (fileSize, getFileStatus)


--------------------------------------------------------------------------------
main :: IO ()
main = do
  databasePath <- getEnv "PROTOTYPE_DB"
  args <- getArgs
  case args of
    ["tables"] -> do
      -- List the table names found in the `PROTOTYPE_DB` SQLite database. This
      -- is used to generate the targets in the Makefile.
      tables <- selectTables' databasePath
      mapM_ (T.putStrLn . tableName) tables

    ["begin-html"] ->
      -- Generate the "top" of an HTML file.
      putStrLn beginHtml

    ["generate-html-index"] -> do
      -- Generate an HTML page listing all the tables.
      tables <- selectTables' databasePath
      putStrLn beginHtml
      putStrLn "<ul>"
      mapM_ (putStrLn. tableNameToLink) tables
      putStrLn "</ul>"
      -- Dont close the HTML, so it is easy to append additional content.
      -- putStrLn
      --   "</pre></code></body></html>"

    ["generate-html", table] -> do
      -- Generate an HTML page for a given table name.
      putStrLn beginHtml
      putStr "<a href=\"/\">home</a> | <a href=\"/tables/\">tables</a> | "
      putStrLn table
      -- Dont close the HTML, so it is easy to append additional content.
      -- putStrLn
      --   "</pre></code></body></html>"

    ["list-md-sources"] -> do
      -- List all Markdown files in this directory.
      files <- findMarkdownFiles
      mapM_ print files

    ["import-md-sources"] -> do
      -- Insert all Markdown files found in this directory into the `sources`
      -- table. Use "list-md-sources" to see the filenames that would be
      -- imported.
      files <- findMarkdownFiles
      let files' = map (\(fn, size) -> Source (T.pack fn) size) files
      conn <- open databasePath
      executeMany conn
        "INSERT INTO sources (path, size) VALUES (?,?)"
        files'
      close conn

    _ -> inProgress databasePath


--------------------------------------------------------------------------------
beginHtml =
  "<!DOCTYPE html>\n\
  \<html>\n\
  \<head></head><body><code><pre>"

tableNameToLink SqliteTable{..} =
  "<li><a href=\"/tables/" ++ name ++ ".html\">" ++ name ++ "</a></li>"
  where name = T.unpack tableName


--------------------------------------------------------------------------------
-- Some not-yet used code.
inProgress fn = do
  conn <- open fn
  ts <- selectTables conn
  mapM_ (\t@SqliteTable{..} -> do
    print t
    cs <- selectColumns conn tableName
    mapM_ print cs) ts
  close conn


--------------------------------------------------------------------------------
selectTables' fn = do
  conn <- open fn
  tables <- selectTables conn
  close conn
  return (sortBy (comparing tableName) tables)

selectTables :: Connection -> IO [SqliteTable]
selectTables conn =
  query_ conn
    "SELECT type, name, tbl_name, rootpage, sql \
    \FROM sqlite_master \
    \WHERE type='table'" :: IO [SqliteTable]

selectColumns :: Connection -> Text -> IO [SqliteColumn]
selectColumns conn table =
  -- Crafting manually the query. Otherwise it prepares a statement
  -- which SQLite doesn't like.
  query_ conn (Query (T.concat ["PRAGMA table_info(", table, ")"]))


--------------------------------------------------------------------------------
data SqliteTable =
  SqliteTable
  { tableType :: Text
  , tableName :: Text
  , tableTblName :: Text
  , tableRootPage :: Int
  , tableSql :: Text
  }
  deriving (Show)

instance FromRow SqliteTable
  where
  fromRow = SqliteTable <$> field <*> field <*> field <*> field <*> field

instance ToRow SqliteTable
  where
  toRow (SqliteTable{..}) =
    toRow (tableType, tableName, tableTblName, tableRootPage, tableSql)


--------------------------------------------------------------------------------
data SqliteColumn =
  SqliteColumn
  { columnCid :: Int
  , columnName :: Text
  , columnType :: Text
  , columnNotNull :: Int
  , columnDfltValue :: Maybe Text
  , columnPk :: Int
  }
  deriving (Show)

instance FromRow SqliteColumn
  where
  fromRow = SqliteColumn <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow SqliteColumn
  where
  toRow (SqliteColumn{..}) =
    toRow (columnCid, columnName, columnType, columnNotNull, columnDfltValue, columnPk)


--------------------------------------------------------------------------------
-- Describe a source file (e.g. Markdown).
data Source =
  Source
  { sourceName :: Text
  , sourceSize :: Int
  }
  deriving (Show)

instance FromRow Source
  where
  fromRow = Source <$> field <*> field

instance ToRow Source
  where
  toRow (Source{..}) =
    toRow (sourceName, sourceSize)

findMarkdownFiles = do
  traverseDir (const True)
    (\fs f ->
      if takeExtension f == ".md"
      then do
        size <- getFileSize f
        return ((drop 2 f, size) : fs)
      else
        return fs)
    [] "."


--------------------------------------------------------------------------------
-- From https://stackoverflow.com/questions/51712083/recursively-search-directories-for-all-files-matching-name-criteria-in-haskell
traverseDir :: (FilePath -> Bool) -> (b -> FilePath -> IO b) -> b -> FilePath -> IO b
traverseDir validDir transition =
    let go state dirPath =
            do names <- listDirectory dirPath
               let paths = map (dirPath </>) names
               (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
               state' <- foldM transition state filePaths -- process current dir
               foldM go state' (filter validDir dirPaths) -- process subdirs
     in go

getFileSize :: String -> IO Int
getFileSize path = do
    stat <- getFileStatus path
    return (fromIntegral (fileSize stat))
