{ mkDerivation, aeson, aeson-optics, base, lib, optics, relude
, string-interpolate, wreq
}:
mkDerivation {
  pname = "merge-bot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-optics base optics relude string-interpolate wreq
  ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "merge-bot";
}
