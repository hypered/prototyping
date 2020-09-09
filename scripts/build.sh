#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

make -f db.Makefile
make
