{ mkDerivation, aeson, base, bytestring, containers, data-default
, deferred-folds, dhall, directory, either, filepath, filepattern, iCalendar
, kassandra, lib, network-simple, nonempty-containers, password, paths, reflex
, reflex-dom, relude, say, stm, stm-containers, streamly, streamly-bytestring
, streamly-core, taskwarrior, text, time, tz, unix, uuid }:
mkDerivation {
  pname = "standalone";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    containers
    data-default
    deferred-folds
    dhall
    directory
    either
    filepath
    filepattern
    iCalendar
    kassandra
    network-simple
    nonempty-containers
    password
    paths
    reflex
    reflex-dom
    relude
    say
    stm
    stm-containers
    streamly
    streamly-bytestring
    streamly-core
    taskwarrior
    text
    time
    tz
    unix
    uuid
  ];
  executableHaskellDepends = [ base kassandra ];
  license = "unknown";
  mainProgram = "kassandra2";
}
