{ mkDerivation, base, bluefin, bluefin-internal, lib, optics
, reflex, reflex-dom, relude, text, witch
}:
mkDerivation {
  pname = "kass";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bluefin bluefin-internal optics reflex reflex-dom relude text
    witch
  ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "kass";
}
