final: _prev: {
  unstable = final.flake-inputs'.nixos-unstable.legacyPackages;
  unstableHaskellPackages = final.unstable.haskellPackages;
  nix-output-monitor = final.flake-inputs'.nix-output-monitor.packages.default;
  inherit (final.unstable)
    nix # For testing with nix-output-monitor
    nil # lings against nix
    nix-serve-ng # links against nix
    forgejo # I am apparently stuck here, because forgejo frequently receives
    # updates and when switching back to stable I run into database version
    # conflicts.
    klog # not in 23.11
    ;
}
