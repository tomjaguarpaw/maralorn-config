{
  stdenv,
  pkg-config,
  hyprland,
  fetchFromGitLab,
}:
stdenv.mkDerivation rec {
  pname = "hyprslidr";
  version = "b93e04a82978b0caa5ecbf21407fcba883fec0e4";
  src = fetchFromGitLab {
    owner = "magus";
    repo = "hyprslidr";
    rev = version;
    hash = "sha256-AAbeW8AA1OcOh9gBK2Ae9EpWHwMRXMDdJ+hw2y/R/0Y=";
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
