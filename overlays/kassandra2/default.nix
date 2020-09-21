final: prev: let
  master = import prev.sources.nixpkgs-master {};
  kassandra = master.haskellPackages.callCabal2nix "kassandra" (prev.sources.kassandra2 + "/kassandra") {};
  standalone = master.haskellPackages.callCabal2nix "standalone" (prev.sources.kassandra2 + "/standalone") { inherit kassandra; };
in  {
  kassandra2 = standalone;
}
