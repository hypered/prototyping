---
screen: VIEW
route: "/item/:id"
query: SELECT json_object('id', id, 'description', description, 'status', status) FROM items WHERE id=?
ids: "SELECT id FROM items"
---

`view-item` is a `VIEW` screen displaying a particular record of the `items`
table. See e.g. [`/item/1`](http://127.0.0.1:9011/item/1).
