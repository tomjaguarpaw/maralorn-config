{
  stdenv,
  pkg-config,
  hyprland,
  fetchFromGitea,
}:
stdenv.mkDerivation rec {
  pname = "hyprslidr";
  version = "02b10c786631df132e306f2cfc96fdd002d4a7dd";

  src = fetchFromGitea {
    owner = "maralorn";
    repo = "hyprslidr";
    rev = version;
    domain = "code.maralorn.de";
    hash = "sha256-euB6oaagyYqObPrmGxCuSGft+tX+lDU4MtU+5B3czLw=";
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
