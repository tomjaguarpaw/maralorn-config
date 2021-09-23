final: prev:
let
  inherit (prev.haskell.lib) overrideCabal unmarkBroken doJailbreak dontCheck;
  myPkgs = import ./packages.nix;
in
{
  myHaskellPackages = myPkgs.makeHaskellPackages prev.unstableHaskellPackages;
  myHaskellScriptPackages = myPkgs.makeHaskellScriptPackages prev.haskellPackages;
  ghcWithPackages = prev.unstableGhc.withHoogle (p: builtins.attrValues (myPkgs.makeHaskellPackages p));
}
