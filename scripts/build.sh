#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

set -e

make -f db.Makefile
make
