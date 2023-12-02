final: prev:
let
  myPkgs = import ./packages.nix;
  inherit (prev.flake-inputs) self;
  inherit (self.lib) selectHaskellPackages;
  hpkgs = final.unstableHaskellPackages.override {overrides = self.overlays.haskellPackagesOverlay;};
  shell = hpkgs.shellFor {
    withHoogle = true;
    packages = p: builtins.attrValues (self.lib.selectHaskellPackages p);
    extraDependencies = p: {
      libraryHaskellDepends = builtins.attrValues (
        myPkgs.makeHaskellScriptPackages p // selectHaskellPackages p // {inherit (p) ghc-debug-client;}
      );
    };
  };
in
{
  myHaskellScriptPackages = myPkgs.makeHaskellScriptPackages final.haskellPackages;
  ghcWithPackages = builtins.head shell.nativeBuildInputs;
}
// selectHaskellPackages hpkgs
