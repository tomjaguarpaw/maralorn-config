{ mkDerivation, aeson, aeson-optics, base, directory, filelock
, filepath, lib, optics, os-string, relude, string-interpolate
, text, time, transformers, typed-process, which, wreq
}:
mkDerivation {
  pname = "vikunja-tools";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-optics base directory filelock filepath optics
    os-string relude string-interpolate text time transformers
    typed-process which wreq
  ];
  executableHaskellDepends = [
    aeson aeson-optics base directory filelock filepath optics
    os-string relude string-interpolate text time transformers
    typed-process which wreq
  ];
  license = "unknown";
}
