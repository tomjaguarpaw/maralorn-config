final: prev: let
  myPkgs = import ./packages.nix;
in {
  myHaskellPackages = myPkgs.makeHaskellPackages prev.unstableHaskellPackages;
  myHaskellScriptPackages = myPkgs.makeHaskellScriptPackages prev.haskellPackages;
  ghcWithPackages = prev.unstableGhc.withHoogle (p: builtins.attrValues (myPkgs.makeHaskellPackages p));
}
