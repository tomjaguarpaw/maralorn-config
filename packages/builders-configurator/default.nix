{ mkDerivation, base, containers, effectful, effectful-th, lib
, relude, req, say, string-interpolate, text, witch
}:
mkDerivation {
  pname = "builders-configurator";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers effectful effectful-th relude req say
    string-interpolate text witch
  ];
  license = "unknown";
  mainProgram = "builders-configurator";
}
