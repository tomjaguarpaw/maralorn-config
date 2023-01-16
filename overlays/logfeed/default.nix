final: prev: {
  logfeed = prev.haskellPackages.callCabal2nix "logfeed" ../apps/logfeed {};
}
