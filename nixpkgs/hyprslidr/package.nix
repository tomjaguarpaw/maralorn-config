{
  stdenv,
  pkg-config,
  hyprland,
  fetchFromGitea,
}:
stdenv.mkDerivation rec {
  pname = "hyprslidr";
  version = "e5f54117a1db5ad1f66d33c70cd8dcf0b7cbdbac";

  src = fetchFromGitea {
    owner = "maralorn";
    repo = "hyprslidr";
    rev = version;
    domain = "code.maralorn.de";
    hash = "sha256-3PmrxnOtrt/JQiN3RPusR7eG6ONXfj7/xkD4hxUEEYs=";
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
