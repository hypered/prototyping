-- Populate the prototype_screens table by copying data from the
-- prototype_metadata table. This is used in `db.Makefile`.

INSERT INTO prototype_screens
  SELECT
    replace(substr(source, 9),".md","")                             AS name,
    json_extract(data, "$.route")                                   AS route,
    json_extract(data, "$.screen")                                  AS type,
    replace(replace(json_extract(data, "$.query"),"‘","'"),"’","'") AS query,
    json_extract(data, "$.ids")                                     AS ids
  FROM prototype_metadata
  WHERE json_extract(data, "$.screen") IS NOT NULL
