{ mkDerivation, base, bluefin, bluefin-internal, dependent-sum
, exceptions, jsaddle, lib, optics, primitive, ref-tf, reflex
, reflex-dom, relude, text, witch
}:
mkDerivation {
  pname = "kass";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bluefin bluefin-internal dependent-sum exceptions jsaddle
    optics primitive ref-tf reflex reflex-dom relude text witch
  ];
  executableHaskellDepends = [ base reflex reflex-dom ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "kass";
}
