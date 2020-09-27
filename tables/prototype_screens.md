The `screens` table describes the different screens of the prototype. Screens
are like pages but more with a role within an application, whereas pages are
more informative. Its records are automatically created in `db.Makefile` by
copying part of the `prototype_metadata` table.

A screen specifies a SQL query to fetch the necessary data to populate it, and
a route where the screen should be visible. By combining the two pieces of
information, the `serve.hs` script is able to automatically make the right data
available at the right route. The query is expected to return JSON.

For instance given the `view-item` screen, (if `serve.hs` is running) you
should be able to visit [/item/1](http://127.0.0.1:9011/item/1).
