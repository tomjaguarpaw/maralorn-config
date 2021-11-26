final: prev:
let
  haskellPackages = prev.haskellPackages.extend (self: super: {
    kassandra = self.callCabal2nix "kassandra" (prev.sources.kassandra2 + "/kassandra") { };
    standalone = self.callCabal2nix "standalone" (prev.sources.kassandra2 + "/standalone") { };
    taskwarrior = self.callHackageDirect { pkg = "taskwarrior"; ver = "0.3.1.0"; sha256 = "sha256-XUoa+xWUHfr080za07/4Xxcic6jgfljrTIXbLaXzoqQ="; } { };
  });
in
{
  kassandra = haskellPackages.standalone;
}
