{
  mkDerivation,
  async,
  base,
  bytestring,
  directory,
  filepath,
  lib,
  reflex,
  relude,
  say,
  shh,
  stm,
  string-interpolate,
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
    reflex
    relude
    say
    shh
    stm
    string-interpolate
  ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "status-script";
}
