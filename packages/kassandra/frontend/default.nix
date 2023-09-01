{ mkDerivation, aeson, base, dependent-map, dependent-sum-template
, extra, generic-optics, jsaddle, kassandra, lib, mtl
, obelisk-executable-config-lookup, obelisk-frontend
, obelisk-generated-static, obelisk-route, optics, optics-th
, reflex, reflex-dom, relude, scientific, taskwarrior, text, these
, time, unordered-containers, uuid
}:
mkDerivation {
  pname = "frontend";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base dependent-map dependent-sum-template extra
    generic-optics jsaddle kassandra mtl
    obelisk-executable-config-lookup obelisk-frontend
    obelisk-generated-static obelisk-route optics optics-th reflex
    reflex-dom relude scientific taskwarrior text these time
    unordered-containers uuid
  ];
  executableHaskellDepends = [
    base kassandra obelisk-frontend obelisk-generated-static
    obelisk-route reflex-dom
  ];
  license = "unknown";
  mainProgram = "frontend";
}
