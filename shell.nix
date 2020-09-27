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
