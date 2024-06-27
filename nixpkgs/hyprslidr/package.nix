{
  stdenv,
  pkg-config,
  hyprland,
  fetchFromGitea,
}:
stdenv.mkDerivation rec {
  pname = "hyprslidr";
  version = "main";
  src = fetchFromGitea {
    owner = "maralorn";
    repo = "hyprslidr";
    rev = version;
    domain = "code.maralorn.de";
    hash = "sha256-oAJhuJ+QECpBfpF/+2O87R+2xpAxXbIoOWAp6XaKraE=";
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
