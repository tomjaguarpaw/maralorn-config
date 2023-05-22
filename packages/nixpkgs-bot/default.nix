{ mkDerivation, aeson-schemas, async, base, clock, containers, esqueleto
, exceptions, graphql-client, http-client, lib, matrix-client, monad-logger, mtl
, persistent, persistent-sqlite, random, relude, resourcet, time, typed-process
, yaml, }:
mkDerivation {
  pname = "nixpkgs-bot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson-schemas
    async
    base
    clock
    containers
    esqueleto
    exceptions
    graphql-client
    http-client
    matrix-client
    monad-logger
    mtl
    persistent
    persistent-sqlite
    random
    relude
    resourcet
    time
    typed-process
    yaml
  ];
  executableHaskellDepends = [
    aeson-schemas
    async
    base
    clock
    containers
    esqueleto
    exceptions
    graphql-client
    http-client
    matrix-client
    monad-logger
    mtl
    persistent
    persistent-sqlite
    random
    relude
    resourcet
    time
    typed-process
    yaml
  ];
  homepage = "https://git.maralorn.de/nixpkgs-bot";
  description =
    "A matrix bot which watches nixpkgs and informs about PR progress";
  license = lib.licenses.agpl3Plus;
  mainProgram = "nixpkgs-bot";
}
