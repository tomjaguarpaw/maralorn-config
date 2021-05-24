self: super:
let
  unstable = import super.sources.nixos-unstable { };
in
{
  inherit (unstable) laminar upterm syncthing vimPlugins dhallPackages fzf gomuks neochat mirage-im nheko haskellPackages ghc cachix nix-output-monitor matrix-synapse-tools;
}
