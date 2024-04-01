{ mkDerivation, base, bluefin, lib, optics, reflex, reflex-dom
, text, witch
}:
mkDerivation {
  pname = "kass";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bluefin optics reflex reflex-dom text witch
  ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "kass";
}
