{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./nix-project-cabal.nix {}
