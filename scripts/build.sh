#! /usr/bin/env bash

# This scripts assume a Nix shell has been enterd with the necessary
# dependencies. This is normally the case if both Nix and direnv are installed.

set -e

make -f db.Makefile
make -j 8
