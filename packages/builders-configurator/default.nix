{
  mkDerivation,
  base,
  containers,
  effectful,
  effectful-th,
  lib,
  relude,
  say,
  shh,
  string-interpolate,
  text,
  witch,
}:
mkDerivation {
  pname = "builders-configurator";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    containers
    effectful
    effectful-th
    relude
    say
    shh
    string-interpolate
    text
    witch
  ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "builders-configurator";
}
