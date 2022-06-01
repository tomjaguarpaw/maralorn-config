final: prev: {
  logfeed =
    prev.haskellPackages.callCabal2nix "logfeed" prev.sources.logfeed
    {
      purebred-email = prev.haskell.lib.unmarkBroken prev.haskellPackages.purebred-email;
    };
}
