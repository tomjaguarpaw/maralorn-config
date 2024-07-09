{ mkDerivation, aeson, aeson-schemas, async, base, bytestring
, directory, exceptions, extra, filepath, fsnotify, lib, network
, nonempty-containers, optics, reflex, relude, safe-exceptions, say
, shh, stm, string-interpolate, template-haskell, time, uuid
}:
mkDerivation {
  pname = "status-script";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-schemas async base bytestring directory exceptions
    extra filepath fsnotify network nonempty-containers optics reflex
    relude safe-exceptions say shh stm string-interpolate
    template-haskell time uuid
  ];
  executableHaskellDepends = [
    aeson aeson-schemas async base bytestring directory exceptions
    extra filepath fsnotify network nonempty-containers optics reflex
    relude safe-exceptions say shh stm string-interpolate
    template-haskell time uuid
  ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "status-script";
}
