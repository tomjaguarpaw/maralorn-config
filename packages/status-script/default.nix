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
  taskwarrior,
  time,
  uuid,
}:
mkDerivation {
  pname = "status-script";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    relude
    string-interpolate
    taskwarrior
    time
    uuid
  ];
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
    taskwarrior
    time
    uuid
  ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "status-script";
}
