#! /usr/bin/env bash

# This scripts assume a Nix shell has been enterd with the necessary
# dependencies. This is normally the case if both Nix and direnv are installed.

set -e

make -j 8 -f db.Makefile  # Build prototype.db.
make -j 8                 # Build _site/.
