{ mkDerivation, aeson, ansi-terminal, async, base, clay, containers
, data-default-class, extra, generic-optics, jsaddle, jsaddle-dom, lib
, nonempty-containers, optics, optics-th, password, patch, process, reflex
, reflex-dom, relude, say, scientific, stm, string-interpolate, taskwarrior
, template-haskell, text, these, time, unordered-containers, uuid, witherable }:
mkDerivation {
  pname = "kassandra";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson
    ansi-terminal
    async
    base
    clay
    containers
    data-default-class
    extra
    generic-optics
    jsaddle
    jsaddle-dom
    nonempty-containers
    optics
    optics-th
    password
    patch
    process
    reflex
    reflex-dom
    relude
    say
    scientific
    stm
    string-interpolate
    taskwarrior
    template-haskell
    text
    these
    time
    unordered-containers
    uuid
    witherable
  ];
  license = "unknown";
}
