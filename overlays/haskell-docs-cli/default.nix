self: super: {
  haskell-docs-cli = self.haskellPackages.callPackage ./package.nix { };
}
