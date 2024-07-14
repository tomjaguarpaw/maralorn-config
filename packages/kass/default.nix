{ mkDerivation, aeson, ansi-terminal, async, base, bluefin
, bluefin-internal, containers, data-default, dependent-sum
, exceptions, extra, falsify, jsaddle, jsaddle-warp, lib, optics
, primitive, ref-tf, reflex, reflex-dom-core, relude, say
, string-interpolate, tasty, template-haskell, text, time
, typed-process, unliftio, witch, witherable, wreq
}:
mkDerivation {
  pname = "kass";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-terminal async base bluefin bluefin-internal containers
    dependent-sum exceptions jsaddle jsaddle-warp optics primitive
    ref-tf reflex reflex-dom-core relude say string-interpolate
    template-haskell text time typed-process unliftio witch witherable
    wreq
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base containers data-default extra falsify optics relude
    tasty time witch
  ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "kass";
}
