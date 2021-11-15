final: prev:
let
  override = hfinal: hprev: {
    relude = hfinal.callHackage "relude" "1.0.0.1" { };
    semialign = hfinal.callHackage "semialign" "1.2" { };
    time-compat = hfinal.callHackage "time-compat" "1.9.6.1" { };
    aeson = hfinal.callHackage "aeson" "2.0.1.0" { };
    hashable = hfinal.callHackage "hashable" "1.3.4.1" { };
    yaml = hfinal.callHackage "yaml" "0.11.7.0" { };
  };
in
{
  haskell-dialog = (prev.unstableHaskellPackages.override { overrides = override; }).callCabal2nix "haskell-dialog" prev.sources.haskell-dialog { };
}
