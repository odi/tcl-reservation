{ pkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:
pkgs.haskell.packages.${compiler}.callPackage ./tclres-local.nix {}
