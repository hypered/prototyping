{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = [
      pkgs.ghcid
      pkgs.sqlite
      (pkgs.haskellPackages.ghcWithPackages (hpkgs: [
        hpkgs.extra
        hpkgs.pandoc
        hpkgs.sqlite-simple
      ]))
    ];
}
