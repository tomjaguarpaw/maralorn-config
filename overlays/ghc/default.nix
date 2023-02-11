final: _: let
  myPkgs = import ./packages.nix;
in {
  myHaskellPackages = myPkgs.makeHaskellPackages final.unstableHaskellPackages;
  myHaskellScriptPackages = myPkgs.makeHaskellScriptPackages final.haskellPackages;
  ghcWithPackages = final.unstableGhc.withHoogle (p: builtins.attrValues (myPkgs.makeHaskellPackages p));
}
