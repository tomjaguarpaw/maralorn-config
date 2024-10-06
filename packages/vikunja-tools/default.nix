{ mkDerivation, aeson, aeson-optics, base, lib, optics, relude
, string-interpolate, text, time, transformers, wreq
}:
mkDerivation {
  pname = "vikunja-tools";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-optics base optics relude string-interpolate text time
    transformers wreq
  ];
  license = "unknown";
  mainProgram = "vikunja-tools";
}
