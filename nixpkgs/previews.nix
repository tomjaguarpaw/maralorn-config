final: prev: {
  unstable = final.flake-inputs'.nixos-unstable.legacyPackages;
  unstableHaskellPackages = final.unstable.haskellPackages;
  nix-output-monitor = final.flake-inputs'.nix-output-monitor.packages.default;
  inherit (final.unstable)
    nix
    nil # Because old nil with new nix does not get cached.
    ;
  lib = prev.lib // {
    inherit (final.unstable) getExe';
  };
}
