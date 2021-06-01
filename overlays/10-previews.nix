self: super:
let
  unstable = import super.sources.nixos-unstable { };
in
{
  inherit (unstable) haskellPackages ghc cachix nix-output-monitor;
}
