{ pkgs ? import <unstable> { } }:

let
  haskellPackages = pkgs.haskellPackages;
  drv = haskellPackages.callCabal2nix "logfeed" ./. { };
in {
  taskwarrior = drv;
  shell = haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [ drv ];
    buildInputs = with haskellPackages; [
      hlint
      cabal-install
      brittany
      pkgs.coreutils
      pkgs.zlib
    ];
  };
}
