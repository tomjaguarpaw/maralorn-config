final: prev: let
  master = import prev.sources.nixpkgs-master {};
in master.haskellPackages.callCabal2nix "logfeed" prev.sources.logfeed {}
