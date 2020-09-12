#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

# This is the same script as build/script.sh but specifies a nix-shell sha
# bang. This is used in .github/workflows/deployment.yml.

set -e

make -f db.Makefile
make -j 8
