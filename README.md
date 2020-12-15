# Prototyping

This is a small SQLite-based programming environment, that is mostly generating
static HTML pages from a database. You can visit the generated pages at
[prototyping.hypered.design](https://prototyping.hypered.design).

- This uses `direnv` with its `use_nix` feature (which loads the environment
described in `shell.nix`).

- This also uses a `Makefile`. I didn't specify `gnumake` in `shell.nix`, but I
guess that its use of `mkShell` automatically provide it. The `Makefile` helps
to re-generate only the necessary files upon changes when developing.

- This also uses `ghcid`, which can be run with `make ghcid`.


## Principles

A SQLite database called `prototype.db` is generated from `.sql` files, and
other data available within this repository. For instance, the names of the
Markdown files are inserted into a table called `sources`.

Using primarily the `prototype.db`, but also other files, static HTML files are
generated in the `_site/` directory.

Populating the database, or generating HTML from it, is done by a `Makefile`,
which itself calls the `prototype.hs` script.

Everything is done by running the `scripts/build.sh` script:

```
prototype.sql                            _site/*.html
              \                        /
pages/*.md     -->-- prototype.db -->--
              /
...
```


## Building

Ensure there is a file `.envrc` with the following content[^envrc] and activate
it with `direnv allow`:

```
$ cat .envrc
NIX_PATH=nixpkgs=channel:nixos-unstable
use_nix
export PROTOTYPE_DB="prototype.db"
$ direnv allow
```

[^envrc]: The file `.envrc` is not versioned because it can also be used to
define environment variables containing credentials. You can change the
recommanded value for `PROTOTYPE_DB` to something else if necessary.

The `PROTOTYPE_DB` is used by the `prototype.hs` script. Currently the
`Makefile` assumes it is `prototype.db` though.

The `NIX_PATH` environment variable, placed before `use_nix`, is taken into
account, and it matches what is used by the GitHub workflow.

The following command should be enough to generate a complete `_site/`
directory that can be served with something like Nginx or Browser Sync.

```
$ scripts/build.sh
```


## Command-line

In addition of the main build script `scripts/build.sh`, some command-line
tools can be usefull, e.g. `sqlite3`, `haskell-mustache`, ... but also some of
them are implemented in `prototype.hs`.


### Screens

The following example shows the JSON returned by a particular `VIEW` screen. It
is thus similar to using the route `http://127.0.0.1:9011/item/1`:

```
$ runghc prototype.hs screen view-item 2 --json
{"id":2,"description":"Extract the values of each ENUMs to reuse them in table descriptions.","status":"TODO"}
```

The screen can be rendered with (the corresponding Mustache template is
`view-item.mustache`):

```
$ runghc prototype.hs screen view-item 2
ITEM-2
Status: TODO
Description:
  Extract the values of each ENUMs to reuse them in table descriptions.
```


## Troubleshooting

```
$ scripts/build.sh 
make: 'prototype.db' is up to date.
make: *** No rule to make target '_site/tables/sources.html', needed by 'all'.  Stop
```

This can be caused by a missing `tables/sources.md` file, used to generate the
corresponding `.html` one.
