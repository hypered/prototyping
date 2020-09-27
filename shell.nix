{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = [
      pkgs.ghcid
      pkgs.jq
      pkgs.sqlite
      (pkgs.haskellPackages.ghcWithPackages (hpkgs: [
        hpkgs.extra
        hpkgs.pandoc
        hpkgs.scotty
        hpkgs.sqlite-simple
      ]))
    ];
}
