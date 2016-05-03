{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, base64-bytestring, bytestring
      , either, errors, lens, lists, old-time, snap, snap-core
      , snap-server, snaplet-sqlite-simple, sqlite-simple, stdenv, text
      , time, transformers, utf8-string
      }:
      mkDerivation {
        pname = "reservation";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base base64-bytestring bytestring either errors lens lists
          old-time snap snap-core snap-server snaplet-sqlite-simple
          sqlite-simple text time transformers utf8-string
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
