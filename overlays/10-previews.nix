self: super:
let
  unstable = import super.sources.nixos-unstable { };
in
{
  inherit (unstable) laminar upterm syncthing vimPlugins dhallPackages fzf gomuks;
  matrix-synapse-tools.rust-synapse-compress-state = unstable.matrix-synapse-tools.rust-synapse-compress-state;
}
