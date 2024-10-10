final: prev:
let
  myPkgs = import ./packages.nix;
  inherit (prev.flake-inputs) self;
  inherit (self.lib) selectHaskellPackages;
  hpkgs = final.unstableHaskellPackages.override {
    overrides = self.overlays.haskellPackagesOverlay;
  };
in
{
  myHaskellScriptPackages = myPkgs.makeHaskellScriptPackages final.haskellPackages;
  ghcWithPackages = hpkgs.ghc.withHoogle (
    p:
    builtins.attrValues (
      myPkgs.makeHaskellScriptPackages p
      // selectHaskellPackages p
      // {
        inherit (p) ghc-debug-client doctest;
      }
    )
  );
}
// selectHaskellPackages hpkgs
