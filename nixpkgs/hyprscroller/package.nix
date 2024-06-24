{
  stdenv,
  cmake,
  pkg-config,
  hyprland,
  lib,
  fetchFromGitHub,
}:
stdenv.mkDerivation {
  pname = "hyprscroller";
  version = "44c48d9d800d4aa679126677182354eff5c03c9c";
  src = fetchFromGitHub {
    owner = "dawsers";
    repo = "hyprscroller";
    rev = "44c48d9d800d4aa679126677182354eff5c03c9c";
    hash = "sha256-QPsJO9ntgQnznTNJkG7hOwyWY/7cKHWDKizWAKQiQ14=";
  };

  nativeBuildInputs = [
    cmake
    pkg-config
    hyprland
  ];
  buildInputs = [ hyprland ] ++ hyprland.buildInputs;

  buildPhase = ''
    make all
  '';

  installPhase = ''
    mkdir -p $out/lib
    cp ./hyprscroller.so $out/lib/libhyprscroller.so
  '';

  meta = with lib; {
    homepage = "https://github.com/dawsers/hyprscroller";
    description = "Hyprland layout plugin providing a scrolling layout like PaperWM";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
