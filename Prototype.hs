{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Prototype where

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
import System.FilePath (takeExtension, (</>), FilePath)
import System.Posix (fileSize, getFileStatus)


--------------------------------------------------------------------------------
beginHtml =
  "<!DOCTYPE html>\n\
  \<html>\n\
  \<head>\n\
  \  <title>Prototyping</title>\n\
  \  <meta name=\"viewport\" content=\"width=device-width\">\n\
  \  <link rel=\"stylesheet\" href=\"/static/css/style.css\">\n\
  \</head><body><main><pre><code>\n\
  \<a href=\"/\">home</a>   \
  \<a href=\"/screens/\">screens</a>   \
  \<a href=\"/tables/\">tables</a>\n"

endHtml =
  "</code></pre></main></body></html>"

screenNameToLink Screen{..} =
  "<li><a href=\"/screens/" ++ name ++ ".html\">" ++ name ++ "</a></li>"
  where name = T.unpack screenName

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
selectScreens' fn = do
  conn <- open fn
  tables <- selectScreens conn
  close conn
  return (sortBy (comparing screenName) tables)

selectScreens :: Connection -> IO [Screen]
selectScreens conn =
  query_ conn
    "SELECT name, route, type, query, ids \
    \FROM prototype_screens"

selectScreen :: Connection -> Text -> IO [Screen]
selectScreen conn name =
  query conn
    "SELECT name, route, type, query, ids \
    \FROM prototype_screens \
    \WHERE name=?"
    (Only name)


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
    \WHERE type='table'"

selectColumns :: Connection -> Text -> IO [SqliteColumn]
selectColumns conn table =
  -- Crafting manually the query. Otherwise it prepares a statement
  -- which SQLite doesn't like.
  query_ conn (Query (T.concat ["PRAGMA table_info(", table, ")"]))


--------------------------------------------------------------------------------
-- | This data type is mapped to the prototype_screens table. See
-- https://prototyping.hypered.design/tables/prototype_screens.html.
data Screen =
  Screen
  { screenName :: Text
  , screenRoute :: Text
  , screenType :: Text
  , screenQuery :: Text
  , screenIds :: Text
  }
  deriving (Show)

instance FromRow Screen
  where
  fromRow = Screen <$> field <*> field <*> field <*> field <*> field

instance ToRow Screen
  where
  toRow (Screen{..}) =
    toRow (screenName, screenRoute, screenType, screenQuery, screenIds)

-- | This data type is maaped to the sqlite_master table.
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
-- | This data type is mapped to the result of PRAGMA table_info().
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
