self: super:
let
  laminarPkgs = import (builtins.fetchGit {
    url = "https://github.com/maralorn/nixpkgs.git";
    rev = "e37cd38effca0310cfe4d27583102175b4a456c9";
    ref = "laminar";
  }) { };
in { laminar = laminarPkgs.laminar; }
