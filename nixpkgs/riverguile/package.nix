{
  stdenv,
  fetchFromSourcehut,
  wayland-scanner,
  wayland,
  gnused,
  lib,
  guile_2_2,
  pkg-config,
  ...
}:
stdenv.mkDerivation rec {
  name = "riverguile";
  version = "25fcd37930e97d67557d0995db25c30e439b349b";
  src = fetchFromSourcehut {
    owner = "~leon_plickat";
    repo = "riverguile";
    rev = version;
    sha256 = "sha256-v8XJwS6P0pIBVsg2R4Jgf5nr96X4Ac1ulXH2Gqa7JNQ=";
  };
  makeFlags = [ "PREFIX=$(out)" ];
  patchPhase = ''
    ${lib.getExe gnused} -i s/-Werror// Makefile
  '';
  buildInputs = [
    wayland-scanner
    wayland
    guile_2_2
    pkg-config
  ];
}
