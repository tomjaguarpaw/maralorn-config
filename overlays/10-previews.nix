self: super:
let
  unstable = import super.sources.nixos-unstable { };
in
{
  inherit unstable;
  inherit (unstable) cachix nix-output-monitor cabal2nix;
  nix = super.nix.overrideAttrs (o: {
    patches = (o.patches or [ ]) ++ [
      (super.fetchpatch {
        # Overridable cache priorities
        url = "https://github.com/NixOS/nix/commit/f8abbdd4565542464f31f4dc203a9c3e091b3536.patch";
        sha256 = "0vbxiab0wd55ccnxkbi86lzmyrvjw946r9scxgq59xiyyss420rl";
      })
    ];
  });
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
  mautrix-signal = unstable.mautrix-signal;
  signald = unstable.signald;
}
