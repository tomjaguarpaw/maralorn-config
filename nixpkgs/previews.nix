final: _prev: {
  unstable = final.flake-inputs'.nixos-unstable.legacyPackages;
  unstableHaskellPackages = final.unstable.haskellPackages;
  nix-output-monitor = final.flake-inputs'.nix-output-monitor.packages.default;
  inherit (final.flake-inputs'.nixos-oldstable.legacyPackages) ddcutil;
  inherit (final.unstable) jujutsu;
  helix =
    # PR https://github.com/helix-editor/helix/pull/6417
    (builtins.getFlake "github:helix-editor/helix/107cdf3e43fef79d36a0bdc8b59327abe85472c9")
    .packages.x86_64-linux.default;
}
