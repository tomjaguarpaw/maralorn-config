{ mkDerivation, async, base, bytestring, directory, exceptions, filepath, lib
, relude, say, shh, stm, string-interpolate, time }:
mkDerivation {
  pname = "status-script";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async
    base
    bytestring
    directory
    exceptions
    filepath
    relude
    say
    shh
    stm
    string-interpolate
    time
  ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "status-script";
}
