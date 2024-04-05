{ mkDerivation, base, bluefin, bluefin-internal, dependent-sum
, exceptions, lib, optics, primitive, ref-tf, reflex, reflex-dom
, relude, text, witch
}:
mkDerivation {
  pname = "kass";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bluefin bluefin-internal dependent-sum exceptions optics
    primitive ref-tf reflex reflex-dom relude text witch
  ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "kass";
}
