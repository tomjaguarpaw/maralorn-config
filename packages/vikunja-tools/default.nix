{ mkDerivation, aeson, base, lib, optics, relude
, string-interpolate, text, wreq
}:
mkDerivation {
  pname = "vikunja-tools";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base optics relude string-interpolate text wreq
  ];
  license = "unknown";
  mainProgram = "vikunja-tools";
}
