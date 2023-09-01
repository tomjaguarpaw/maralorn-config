{ mkDerivation, aeson, async, base, containers, data-default-class
, dhall, frontend, kassandra, lib, network-simple, obelisk-backend
, obelisk-route, password, relude, say, snap-core, standalone, stm
, taskwarrior, uuid, websockets, websockets-snap
}:
mkDerivation {
  pname = "backend";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base containers data-default-class dhall frontend
    kassandra network-simple obelisk-backend obelisk-route password
    relude say snap-core standalone stm taskwarrior uuid websockets
    websockets-snap
  ];
  executableHaskellDepends = [
    base frontend kassandra obelisk-backend
  ];
  license = "unknown";
  mainProgram = "backend";
}
