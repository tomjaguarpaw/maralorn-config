final: _prev: {
  unstable = final.flake-inputs'.nixos-unstable.legacyPackages;
  unstableHaskellPackages = final.unstable.haskellPackages;
  nix-output-monitor = final.flake-inputs'.nix-output-monitor.packages.default;
  inherit (final.unstable)
    nix # For testing with nix-output-monitor
    nil # Because old nil with new nix does not get cached.
    forgejo # I am apparently stuck here, because forgejo frequently receives
    # updates and when switching back to stable I run into database version
    # conflicts.
    ;
}
