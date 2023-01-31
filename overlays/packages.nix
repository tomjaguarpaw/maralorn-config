_: prev: let
  inherit (import ../packages {pkgs = prev;}) haskellPackagesOverlay selectHaskellPackages;
in
  selectHaskellPackages (prev.unstable.haskellPackages.override {
    overrides = haskellPackagesOverlay;
  })
