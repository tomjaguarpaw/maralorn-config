final: prev:
let
  haskellPackages = prev.haskellPackages.extend (self: super: {
    kassandra = self.callCabal2nix "kassandra" (prev.sources.kassandra + "/kassandra") { };
    standalone = self.callCabal2nix "standalone" (prev.sources.kassandra + "/standalone") { };
    taskwarrior = self.callHackageDirect { pkg = "taskwarrior"; ver = "0.5.0.0"; sha256 = "sha256-elDUtz0NSG4WHxkyCQ1CunYXWIVRj6EqkKSchPy+c3E="; } { };
  });
in
{
  kassandra = haskellPackages.standalone;
}
