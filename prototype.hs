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

import Prototype


--------------------------------------------------------------------------------
main :: IO ()
main = do
  databasePath <- getEnv "PROTOTYPE_DB"
  args <- getArgs
  case args of
    ["screens"] -> do
      -- List the screen names.
      screens <- selectScreens' databasePath
      mapM_ (T.putStrLn . screenName) screens

    ["tables"] -> do
      -- List the table names found in the `PROTOTYPE_DB` SQLite database. This
      -- is used to generate the targets in the Makefile.
      tables <- selectTables' databasePath
      mapM_ (T.putStrLn . tableName) tables

    ["begin-html"] ->
      -- Generate the "top" of an HTML file.
      putStrLn beginHtml

    ["end-html"] ->
      -- Generate the "bottom" of an HTML file.
      putStrLn endHtml

    ["screen-index-html"] -> do
      -- Generate an HTML page listing all the screens. Screens are like pages,
      -- but considered as part of an application (although, a documentation
      -- page that would be customized with, say, an API key could be seen as
      -- "applicative").
      screens <- selectScreens' databasePath
      putStrLn beginHtml
      putStrLn "<ul>"
      mapM_ (putStrLn. screenNameToLink) screens
      putStrLn "</ul>"
      -- Dont close the HTML, so it is easy to append additional content.

    ["screen-html", screen] -> do
      -- Generate an HTML page for a given screen name.
      putStrLn beginHtml
      putStrLn ("<strong>" ++ screen ++ "</strong>")
      -- Dont close the HTML, so it is easy to append additional content.

    ["table-index-html"] -> do
      -- Generate an HTML page listing all the tables.
      tables <- selectTables' databasePath
      putStrLn beginHtml
      putStrLn "<ul>"
      mapM_ (putStrLn. tableNameToLink) tables
      putStrLn "</ul>"
      -- Dont close the HTML, so it is easy to append additional content.

    ["table-html", table] -> do
      -- Generate an HTML page for a given table name.
      putStrLn beginHtml
      putStrLn ("<strong>" ++ table ++ "</strong>")
      -- Dont close the HTML, so it is easy to append additional content.

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
        "INSERT INTO prototype_sources (path, size) VALUES (?,?)"
        files'
      close conn

    _ -> inProgress databasePath
