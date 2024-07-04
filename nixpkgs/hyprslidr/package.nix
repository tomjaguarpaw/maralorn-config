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
    rev = "e29acba2640dd109ef9281a9692cbd779c4a89f5";
    domain = "code.maralorn.de";
    hash = "sha256-dnqnhekP2Odnay7bWTvhlatyirk9NP3jarIIRwhjekY=";
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
