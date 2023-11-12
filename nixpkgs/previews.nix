final: prev: {
  unstable = final.flake-inputs'.nixos-unstable.legacyPackages;
  unstableHaskellPackages = final.unstable.haskellPackages;
  nix-output-monitor = final.flake-inputs'.nix-output-monitor.packages.default;
  inherit (final.unstable)
    nil # Because old nil with new nix does not get cached.
    forgejo
    eww-wayland
  ;
  lib = prev.lib // {
    inherit (final.unstable) getExe';
  };
  nix = final.nixVersions.nix_2_16; # I am not sure why I have exactly this override older or newer versions are broken.
}
