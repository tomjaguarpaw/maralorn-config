{ mkDerivation, aeson, aeson-optics, base, directory, filepath, lib
, optics, os-string, relude, shh, string-interpolate, text, time
, transformers, wreq
}:
mkDerivation {
  pname = "vikunja-tools";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-optics base directory filepath optics os-string relude
    shh string-interpolate text time transformers wreq
  ];
  executableHaskellDepends = [
    aeson aeson-optics base directory filepath optics os-string relude
    shh string-interpolate text time transformers wreq
  ];
  license = "unknown";
}
