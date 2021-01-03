{ pkgs, ... }:
let
  laminarPkgs = builtins.fetchGit {
    url = "https://github.com/maralorn/nixpkgs.git";
    rev = "e37cd38";
  };
in { systemd.packages = [ laminarPkgs.laminar ]; }
