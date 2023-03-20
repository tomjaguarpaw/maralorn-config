{
  mkDerivation,
  async,
  base,
  bytestring,
  directory,
  filepath,
  lib,
  relude,
  say,
  shh,
  stm,
  streamly,
  streamly-core,
  string-interpolate,
  time,
}:
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
    filepath
    relude
    say
    shh
    stm
    streamly
    streamly-core
    string-interpolate
    time
  ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "status-script";
}
