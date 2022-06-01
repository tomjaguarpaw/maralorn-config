final: prev: let
  haskellPackages = prev.haskellPackages.extend (self: super: {
    kassandra = self.callCabal2nix "kassandra" (prev.sources.kassandra + "/kassandra") {};
    standalone = self.callCabal2nix "standalone" (prev.sources.kassandra + "/standalone") {};
  });
in {
  kassandra = haskellPackages.standalone;
}
