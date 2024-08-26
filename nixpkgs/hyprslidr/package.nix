{
  stdenv,
  pkg-config,
  hyprland,
  fetchFromGitea,
}:
stdenv.mkDerivation {
  pname = "hyprslidr";
  version = "maralorn-fork";

  src = fetchFromGitea {
    owner = "maralorn";
    repo = "hyprslidr";
    rev = "bb58ceb336e6e6ea2d1f05f628bfcca1f6d616e1";
    domain = "code.maralorn.de";
    hash = "sha256-w8DPflSDLQ9XMVnOh8tOPECE9R48PZdsIetpzY0m9UQ=";
  };

  nativeBuildInputs = [
    pkg-config
    hyprland
  ];
  buildInputs = [ hyprland ] ++ hyprland.buildInputs;

  buildPhase = ''
    make all
  '';

  installPhase = ''
    mkdir -p $out/lib
    cp ./hyprslidr.so $out/lib/libhyprslidr.so
  '';
}
