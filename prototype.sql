-- This is the main file used to generate `prototype.db`.


--------------------------------------------------------------------------------
-- Schema.
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
-- Data.
--------------------------------------------------------------------------------

INSERT INTO prototype_directories (path) VALUES
  ("_site/");

INSERT INTO prototype_pages (url) VALUES
  ("/index.html");
