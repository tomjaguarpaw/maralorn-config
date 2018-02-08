let
  unstable = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
  habitask = with unstable; with rustPlatform; buildRustPackage rec {
     name = "habitask";
     version = "0.1.0";
     src = ~/data/aktuell/it/code/habitask;
     depsSha256 = "0clac943ajxns64jkdcg312a4x4jgd239jb4yd5qm32nnkj62ym7";
     cargoSha256 = "0clac943ajxns64jkdcg312a4x4jgd239jb4yd5qm32nnkj62ym7";
     buildInputs = [ openssl pkgconfig ];
  };
in {
  home.packages = [ habitask ];
}
