{
  pkg-config,
  hyprland,
  fetchFromGitHub,
  gcc13Stdenv,
}:
gcc13Stdenv.mkDerivation rec {
  pname = "Hyprspace";
  version = "2f3edb68f47a8f5d99d10b322e9a85a285f53cc7";

  src = fetchFromGitHub {
    owner = "KZDKM";
    repo = "Hyprspace";
    rev = version;
    hash = "sha256-iyj4D6c77uROAH9QdZjPd9SKnS/DuACMESqaEKnBgI8=";
  };

  nativeBuildInputs = [
    pkg-config
    hyprland
  ];
  installPhase = ''
    mkdir -p $out/lib
    cp ./Hyprspace.so $out/lib/libHyprspace.so
  '';
  buildInputs = [ hyprland ] ++ hyprland.buildInputs;
  dontUseCmakeConfigure = true;
}
