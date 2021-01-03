{ pkgs, ... }:
let
  laminarPkgs = builtins.fetchGit {
    url = "https://github.com/maralorn/nixpkgs.git";
    rev = "e37cd38effca0310cfe4d27583102175b4a456c9";
  };
in { systemd.packages = [ laminarPkgs.laminar ]; }
