final: prev: let
  haskellPackages = prev.haskellPackages.extend (self: super: {
    kassandra = self.callCabal2nix "kassandra" ../apps/kassandra/kassandra {};
    standalone = self.callCabal2nix "standalone" ../apps/kassandra/standalone {};
  });
in {
  kassandra = haskellPackages.standalone;
}
