{ mkDerivation, aeson-schemas, async, base, clock, containers
, esqueleto, exceptions, graphql-client, http-client, lib
, matrix-client, monad-logger, mtl, persistent, persistent-sqlite
, random, relude, resourcet, time, typed-process, yaml
}:
mkDerivation {
  pname = "nixpkgs-bot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson-schemas async base clock containers esqueleto exceptions
    graphql-client http-client matrix-client monad-logger mtl
    persistent persistent-sqlite random relude resourcet time
    typed-process yaml
  ];
  executableHaskellDepends = [
    aeson-schemas async base clock containers esqueleto exceptions
    graphql-client http-client matrix-client monad-logger mtl
    persistent persistent-sqlite random relude resourcet time
    typed-process yaml
  ];
  license = "unknown";
  mainProgram = "nixpkgs-bot";
}
