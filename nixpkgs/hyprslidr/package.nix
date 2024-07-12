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
    rev = "fc9345544e671ff0d03971d92db270dfbf2ec546";
    domain = "code.maralorn.de";
    hash = "sha256-FMjtcYlibhPyr5Z3d1xoR+US+lvR5yFLuBiOKMZc5OY=";
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
