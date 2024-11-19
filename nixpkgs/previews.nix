final: prev: {
  unstable = import prev.flake-inputs.nixos-unstable {
    inherit (prev) system;
    #  config.allowUnfreePredicate = pkg: builtins.elem (prev.lib.getName pkg) [ "factorio-space-age" ];
  };
  unstableHaskellPackages = final.unstable.haskellPackages;
  nix-output-monitor = prev.flake-inputs'.nix-output-monitor.packages.default;
  #inherit (prev.flake-inputs'.nixos-oldstable.legacyPackages) ddcutil;
  #inherit (final.unstable) jujutsu factorio-space-age;
  helix =
    # PR https://github.com/helix-editor/helix/pull/5063
    (builtins.getFlake "github:helix-editor/helix/3e838fff106f9dc5bef37b691ade97fb4cd885b4")
    .packages.x86_64-linux.default;
}
