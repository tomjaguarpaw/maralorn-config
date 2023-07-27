{
  mkDerivation,
  aeson,
  async,
  base,
  bytestring,
  directory,
  exceptions,
  filepath,
  fsnotify,
  lib,
  network,
  reflex,
  relude,
  say,
  shh,
  stm,
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
    aeson
    async
    base
    bytestring
    directory
    exceptions
    filepath
    fsnotify
    network
    reflex
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
