{ mkDerivation, base, bytestring, containers, foldl, lens, mtl
, parallel-io, stdenv, system-filepath, terminal-progress-bar, text
, time, turtle
}:
mkDerivation {
  pname = "shrinkmusic";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers foldl lens mtl parallel-io
    system-filepath terminal-progress-bar text time turtle
  ];
  description = "shrink a directory of music into another directory";
  license = stdenv.lib.licenses.bsd3;
}
