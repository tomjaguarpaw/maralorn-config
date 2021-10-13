final: prev:
let
  override = hfinal: hprev: {
    relude = hfinal.relude_1_0_0_1;
    semialign = hfinal.semialign_1_2;
    time-compat = hfinal.time-compat_1_9_6_1;
    hashable = hfinal.hashable_1_3_4_1;
    yaml = hfinal.callHackageDirect
      {
        ver = "0.11.7.0";
        pkg = "yaml";
        sha256 = "0mn08dh5h49np5rd8ym7wl3i8nfdj9vgawprcdgrmvz5j59nsp0n";
      }
      { };
    aeson = hfinal.callHackageDirect
      {
        ver = "2.0.1.0";
        pkg = "aeson";
        sha256 = "0nhzbnygj17m4x39mmf8r13xisc0hnkijnrwyqskf8gk276x9dpz";
      }
      { };
  };
in
{
  haskell-dialog = (prev.unstableHaskellPackages.override { overrides = override; }).callCabal2nix "haskell-dialog" prev.sources.haskell-dialog { };
}
