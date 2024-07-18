final: _prev: {
  unstable = final.flake-inputs'.nixos-unstable.legacyPackages;
  unstableHaskellPackages = final.unstable.haskellPackages;
  nix-output-monitor = final.flake-inputs'.nix-output-monitor.packages.default;
  inherit (final.flake-inputs'.nixos-oldstable.legacyPackages) ddcutil;
  inherit (final.unstable) jujutsu;
  helix =
    # PR https://github.com/helix-editor/helix/pull/5063
    (builtins.getFlake "github:helix-editor/helix/3e838fff106f9dc5bef37b691ade97fb4cd885b4")
    .packages.x86_64-linux.default;
}
