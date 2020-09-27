---
screen: VIEW
route: "/item/:id"
query: SELECT json_object('id', id, 'description', description, 'status', status) FROM items WHERE id=?
ids: "SELECT id FROM items"
---

`view-item` is a `VIEW` screen displaying a particular record of the `items`
table.
