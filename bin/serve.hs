-- A small Scotty-based web server exposing the routes found in prototype.db.
-- Simply run it with `runghc serve.hs` then navigate to
-- http://127.0.0.1:9011/.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Database.SQLite.Simple (close, open, query, Only(Only), Query(Query))
import Network.HTTP.Types.Method (StdMethod(GET))
import System.Environment (getEnv)
import Web.Scotty
  ( addroute, capture, get, html, liftAndCatchIO, param, scotty, setHeader
  , text, ActionM, ScottyM )

import Prototype


--------------------------------------------------------------------------------
main :: IO ()
main = do
  databasePath <- getEnv "PROTOTYPE_DB"
  screens <- selectScreens' databasePath
  scotty 9011 $ do
    get "/" $ do
      html "Try e.g. <a href=\"/item/1\">/item/1</a>."

    mapM_ (addView databasePath) (filter ((== "VIEW") . screenType) screens)


--------------------------------------------------------------------------------
-- | Add a screen of type VIEW to Scotty. Well actually this is just JSON for
-- now.
addView :: String -> Screen -> ScottyM ()
addView fn Screen{..} =
  addroute GET (capture $ T.unpack screenRoute) $ do
    i <- param "id" :: ActionM Int
    records <- liftAndCatchIO $ do
      conn <- open fn
      rs <- query conn (Query screenQuery) (Only i)
      close conn
      return rs
    setHeader "Content-Type" "application/json; charset=utf-8"
    -- TODO This assumes there is a single record. This should return a 500 if
    -- there is more than one, and a 404 if there is none.
    text $ LT.concat (map (\(Only a) -> LT.fromStrict a) records)
