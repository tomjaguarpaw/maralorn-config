final: _prev: {
  unstable = final.flake-inputs'.nixos-unstable.legacyPackages;
  unstableHaskellPackages = final.unstable.haskellPackages;
  nix-output-monitor = final.flake-inputs'.nix-output-monitor.packages.default;
  inherit (final.unstable)
    eww
    forgejo # I am apparently stuck here, because forgejo frequently receives
    # updates and when switching back to stable I run into database version
    # conflicts.
    forgejo-actions-runner
    nixfmt-rfc-style
    klog-time-tracker # not in 23.11
    ;
  helix =
    (builtins.getFlake "github:helix-editor/helix/58bd26823fe431cf7391fc27949b215ab2a4e7ac")
    .packages.x86_64-linux.default;
}
