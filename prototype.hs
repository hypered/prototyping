-- This script offers a few sub-commands that are used by the Makefile to build
-- the `_site/` directory. Some of the sub-commands are used to populate the
-- SQLite database `prototype.db`, some others are used to read the database
-- and generate HTML files.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (foldM)
import Control.Monad.Extra (partitionM)
import Control.Applicative
import Data.Aeson (eitherDecodeStrict, Value)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs, getEnv)
import System.FilePath (takeExtension, (</>), FilePath)
import System.Posix (fileSize, getFileStatus)

import Text.Mustache (automaticCompile, substitute, toMustache)

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

    ["screen"] -> do
      putStrLn ("TODO Describe the screen subcommand.")

    ["screen", screen] -> do
      putStrLn ("TODO Describe the " ++ screen ++ " screen.")

    ["screen", screen, i] -> do
      -- Lookup a VIEW screen and template its data with a corresponding
      -- Mustache template. The data can be viewed with the `--json` variant
      -- of this command below.
      -- This is similar to
      --   $ runghc prototype.hs screen view-item <i> --json > a
      --   $ haskell-mustache mustache/view-item.mustache a
      mtemplate <- automaticCompile ["mustache"] (screen ++ ".mustache")
      case mtemplate of
        Left err -> error (show err)
        Right compiledTemplate -> do
          mscreen <- getScreenJSON databasePath screen (read i)
          maybe
            (error "No such screen")
            (\a -> do
              either
                putStrLn
                (T.putStrLn . substitute compiledTemplate . toMustache)
                (eitherDecodeStrict (T.encodeUtf8 a) :: Either String Value)
            )
            mscreen

    ["screen", screen, i, "--json"] -> do
      -- Lookup a VIEW screen and returns the result of its query.
      mscreen <- getScreenJSON databasePath screen (read i)
      maybe
        (error "No such screen")
        T.putStrLn
        mscreen

    _ -> inProgress databasePath

getScreenJSON :: String -> String -> Int -> IO (Maybe Text)
getScreenJSON fn screen i = do
  -- TODO Use bracket for open/close.
  conn <- open fn
  mscreen <- selectScreen conn (T.pack screen)
  case mscreen of
    [Screen{..}] -> do
      -- TODO No such record.
      [Only a] <- query conn (Query screenQuery) (Only i)
      close conn
      return a
    _ -> do
      close conn
      return Nothing
      -- More than one screen is not possible because of the PRIMARY KEY
      -- constraint.
