final: prev: let
  master = import prev.sources.nixpkgs-master {};
in {
  logfeed = master.haskellPackages.callCabal2nix "logfeed" prev.sources.logfeed {};
}
