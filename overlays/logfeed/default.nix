final: prev: {
  logfeed = prev.haskellPackages.callCabal2nix "logfeed" prev.sources.logfeed {};
}
