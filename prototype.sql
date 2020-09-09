-- This is the main file used to generate `prototype.db`.


--------------------------------------------------------------------------------
-- Schema.
--------------------------------------------------------------------------------

-- Source files. See tables/sources.html.
CREATE TABLE sources (
  path TEXT PRIMARY KEY,
  size INT NOT NULL
);

-- Directories. See tables/directories.html.
CREATE TABLE directories (
  path TEXT PRIMARY KEY
);

-- Pages. See tables/pages.html.
CREATE TABLE pages (
  url TEXT PRIMARY KEY
);


--------------------------------------------------------------------------------
-- Data.
--------------------------------------------------------------------------------

INSERT INTO directories (path) VALUES
  ("_site/");

INSERT INTO pages (url) VALUES
  ("/index.html");
