-- Populate the prototype_screens table by copying data from the
-- prototype_metadata table.
INSERT INTO prototype_screens
  SELECT replace(substr(source, 9),".md","") AS name,
         json_extract(data, "$.screen") AS type,
         json_extract(data, "$.query") AS query
  FROM prototype_metadata
  WHERE json_extract(data, "$.screen") IS NOT NULL
