-- This is the main file used to generate `prototype.db`.


--------------------------------------------------------------------------------
-- Schema: meta tables used by this prototyping tool.
--------------------------------------------------------------------------------

-- Source files. See tables/prototype_sources.html.
CREATE TABLE prototype_sources (
  path TEXT PRIMARY KEY,
  size INT NOT NULL
);

-- Directories. See tables/prototype_directories.html.
CREATE TABLE prototype_directories (
  path TEXT PRIMARY KEY
);

-- Screens. See tables/prototype_screens.html.
CREATE TABLE prototype_screens (
  name TEXT PRIMARY KEY,
  route TEXT NOT NULL,
  type ENUM NOT NULL CHECK(type IN ('VIEW','LIST')),
  query TEXT NOT NULL,
  ids TEXT NOT NULL
);

-- Pages. See tables/prototype_pages.html.
CREATE TABLE prototype_pages (
  url TEXT PRIMARY KEY
);

-- Metadata. See tables/prototype_metadata.html.
CREATE TABLE prototype_metadata (
  source TEXT PRIMARY KEY,
  type TEXT NOT NULL,
  data JSON
);


--------------------------------------------------------------------------------
-- Schema: tables of the application being prototyped. Here a dead-simple
-- todolist application.
-- The ENUM type doesn't exist in SQLite, and is used here to facilitate the
-- extraction of those constants with something like
--   grep ENUM prototype.sql | sed 's/.*IN (\(.*\))),\?/\1/'
--------------------------------------------------------------------------------

CREATE TABLE items (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  status ENUM NOT NULL CHECK(status IN ('TODO','DONE')),
  description TEXT NOT NULL CHECK(length(description) > 0)
);

--------------------------------------------------------------------------------
-- Data.
--------------------------------------------------------------------------------

INSERT INTO prototype_directories (path) VALUES
  ("_site/");

INSERT INTO prototype_pages (url) VALUES
  ("/index.html");

INSERT INTO items (status, description) VALUES
  ("TODO", "Add the first characters of each table description to /tables/index.html."),
  ("TODO", "Extract the values of each ENUMs to reuse them in table descriptions.");
