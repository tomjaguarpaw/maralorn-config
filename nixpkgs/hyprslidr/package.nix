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
    rev = "703fab6c59a764dcaddc1d90dd43c4817960097c";
    domain = "code.maralorn.de";
    hash = "sha256-xj7bX46lCXFaCEVV0aewEq3HNsds9O6qY0bC4V5gFp0=";
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
