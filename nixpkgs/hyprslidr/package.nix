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
    rev = "e13acbb53b0e917f98f50c673281593a4e9a20b5";
    domain = "code.maralorn.de";
    hash = "sha256-jnisHvse+yRkjAbd3hZdJMwsgZbDulL7pbfxiTp00fY=";
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
