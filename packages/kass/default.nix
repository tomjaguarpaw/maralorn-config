{ mkDerivation, aeson, ansi-terminal, async, base, bluefin
, bluefin-internal, dependent-sum, exceptions, jsaddle
, jsaddle-warp, jsaddle-webkit2gtk, lib, optics, primitive, ref-tf
, reflex, reflex-dom, reflex-dom-core, relude, say
, string-interpolate, template-haskell, text, typed-process
, unliftio, witch, witherable, wreq
}:
mkDerivation {
  pname = "kass";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-terminal async base bluefin bluefin-internal
    dependent-sum exceptions jsaddle jsaddle-warp jsaddle-webkit2gtk
    optics primitive ref-tf reflex reflex-dom-core relude say
    string-interpolate template-haskell text typed-process unliftio
    witch witherable wreq
  ];
  executableHaskellDepends = [ base reflex reflex-dom ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "kass";
}
