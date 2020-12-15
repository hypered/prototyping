# This file defines a Nix shell environment. It is normally used in conjunction
# with a `.envrc` file and the direnv tool (see the README). But it can also be
# manually activated by runnin `nix-shell` in this directory. Once activated,
# programs such as ghci and sqlite3 will be in scope.

{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = [
      pkgs.ghcid
      pkgs.jq
      pkgs.sqlite
      (pkgs.haskellPackages.ghcWithPackages (hpkgs: [
        hpkgs.extra
        hpkgs.mustache
        hpkgs.pandoc
        hpkgs.scotty
        hpkgs.sqlite-simple
      ]))
    ];
}
