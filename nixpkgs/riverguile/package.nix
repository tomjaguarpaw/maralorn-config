{
  stdenv,
  fetchFromSourcehut,
  wayland-scanner,
  wayland,
  guile_2_2,
  pkg-config,
  ...
}:
stdenv.mkDerivation rec {
  name = "riverguile";
  version = "aab2f4ef173e2164f8e0b73d77476813c6844739";
  src = fetchFromSourcehut {
    owner = "~leon_plickat";
    repo = "riverguile";
    rev = version;
    sha256 = "sha256-WNv7mCpUydIBVpAMm6EiBEWSZYdlTr5hTr0O3RvUMcs=";
  };
  buildInputs = [
    wayland-scanner
    wayland
    guile_2_2
    pkg-config
  ];
}
