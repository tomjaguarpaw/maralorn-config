{ mkDerivation, base, directory, filepath, generic-lens, lens, lib
, megaparsec, mtl, relude, streamly, streamly-core
, string-interpolate, time, unix, witch
}:
mkDerivation {
  pname = "t";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base generic-lens lens megaparsec mtl relude streamly streamly-core
    string-interpolate time witch
  ];
  executableHaskellDepends = [
    base directory filepath lens megaparsec relude time unix
  ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "t";
}
