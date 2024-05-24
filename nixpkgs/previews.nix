final: _prev: {
  unstable = final.flake-inputs'.nixos-unstable.legacyPackages;
  unstableHaskellPackages = final.unstable.haskellPackages;
  nix-output-monitor = final.flake-inputs'.nix-output-monitor.packages.default;
  inherit (final.flake-inputs'.nixos-oldstable.legacyPackages) ddcutil;
  helix =
    # PR https://github.com/helix-editor/helix/pull/6417
    (builtins.getFlake "github:helix-editor/helix/c2268bb7acd365e075c2718e6356c866f26cfc90")
    .packages.x86_64-linux.default;
}
