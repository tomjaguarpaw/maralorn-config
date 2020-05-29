self: super:
let inherit (self.haskell.lib) unmarkBroken dontCheck doJailbreak;
in {
  haskellPackages = super.haskellPackages // {
    shh = doJailbreak (unmarkBroken (dontCheck super.haskellPackages.shh));
  };
}
