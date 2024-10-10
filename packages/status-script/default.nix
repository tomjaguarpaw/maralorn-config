{ mkDerivation, aeson, aeson-schemas, async, base, bytestring
, directory, exceptions, extra, filepath, fsnotify, github, lib
, network, nonempty-containers, optics, reflex, relude
, safe-exceptions, say, shh, stm, string-interpolate
, template-haskell, time, typed-process, uuid, vikunja-tools, wreq
}:
mkDerivation {
  pname = "status-script";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-schemas async base bytestring directory exceptions
    extra filepath fsnotify github network nonempty-containers optics
    reflex relude safe-exceptions say shh stm string-interpolate
    template-haskell time typed-process uuid vikunja-tools wreq
  ];
  executableHaskellDepends = [
    aeson aeson-schemas async base bytestring directory exceptions
    extra filepath fsnotify github network nonempty-containers optics
    reflex relude safe-exceptions say shh stm string-interpolate
    template-haskell time typed-process uuid vikunja-tools wreq
  ];
  license = "unknown";
  mainProgram = "status-script";
}
