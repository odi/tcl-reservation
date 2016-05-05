{ mkDerivation, aeson, base, base64-bytestring, bytestring, either
, errors, lens, lists, old-time, snap, snap-core, snap-server
, snaplet-sqlite-simple, sqlite-simple, stdenv, text, time
, transformers, utf8-string, fetchFromGitHub
}:
mkDerivation {
  pname = "reservation";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "odi";
    repo = "tcl-reservation";
    rev = "105a5c5d86edee4a53fe721ae6a98ba922265aba";
    sha256 = "1b4jr0qygzrpbmxyaqq15wgap91bij0hl0qakz3gm51cjcfkmbbx";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base64-bytestring bytestring either errors lens lists
    old-time snap snap-core snap-server snaplet-sqlite-simple
    sqlite-simple text time transformers utf8-string
  ];
  postInstall = ''
    cp -R static $out/static
  '';
  license = stdenv.lib.licenses.bsd3;
}
