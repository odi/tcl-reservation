{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, snap, stdenv }:
      mkDerivation {
        pname = "reservation";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [ base snap ];
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
