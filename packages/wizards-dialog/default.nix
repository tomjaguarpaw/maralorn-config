{ mkDerivation, aeson, ansi-terminal, base, daemons, haskeline, lib
, process, relude, text, witch, wizards, yaml
}:
mkDerivation {
  pname = "wizards-dialog";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base haskeline process relude text witch wizards
  ];
  executableHaskellDepends = [
    aeson ansi-terminal base daemons haskeline process relude text
    witch wizards yaml
  ];
  license = "unknown";
  mainProgram = "hotkeys";
}
