{ mkDerivation, aeson, base, base64-bytestring, bytestring, either
, errors, lens, lists, old-time, snap, snap-core, snap-server
, snaplet-sqlite-simple, sqlite-simple, stdenv, text, time
, transformers, utf8-string, snap-cors
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
    sqlite-simple text time transformers utf8-string snap-cors
  ];
  postInstall = ''
    cp -R ./static $out/
  '';
  license = stdenv.lib.licenses.bsd3;
}
