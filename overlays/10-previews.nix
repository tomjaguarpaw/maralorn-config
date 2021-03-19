self: super:
let
  laminarPkgs = import (builtins.fetchGit {
    url = "https://github.com/maralorn/nixpkgs.git";
    rev = "e37cd38effca0310cfe4d27583102175b4a456c9";
    ref = "laminar";
  }) { };
  unstable = import super.sources.nixos-unstable { };
in {
  laminar = laminarPkgs.laminar;
  syncthingNext = unstable.syncthing;
  upterm = unstable.upterm;
  syncthing = unstable.syncthing;
  vimPlugins = unstable.vimPlugins;
  fzf = unstable.fzf;
  matrix-synapse-tools.rust-synapse-compress-state = unstable.matrix-synapse-tools.rust-synapse-compress-state;
}
