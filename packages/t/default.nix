{ mkDerivation, base, directory, filepath, lens, lib, megaparsec
, relude, time, unix
}:
mkDerivation {
  pname = "t";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [
    base directory filepath lens megaparsec relude time unix
  ];
  doHaddock = false;
  license = lib.licenses.agpl3Plus;
  mainProgram = "t";
}
