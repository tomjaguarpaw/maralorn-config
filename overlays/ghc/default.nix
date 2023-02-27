final: _: let
  myPkgs = import ./packages.nix;
in {
  myHaskellScriptPackages = myPkgs.makeHaskellScriptPackages final.haskellPackages;
  ghcWithPackages = final.unstableGhc.withHoogle (p: builtins.attrValues (myPkgs.makeHaskellPackages p // (final.flake-inputs.self.overlays.addMyHaskellPackages "" "")));
}
