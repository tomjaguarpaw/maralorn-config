final: prev: let
  kassandra = prev.haskellPackages.callCabal2nix "kassandra" (prev.sources.kassandra2 + "/kassandra") {};
  standalone = prev.haskellPackages.callCabal2nix "standalone" (prev.sources.kassandra2 + "/standalone") { inherit kassandra; };
in  {
  kassandra2 = standalone;
}
