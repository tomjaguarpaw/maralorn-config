final: prev: {
  logfeed = prev.unstableHaskellPackages.callCabal2nix "logfeed" ../../apps/logfeed {};
}
