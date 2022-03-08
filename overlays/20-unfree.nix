self: super: let
  unfree = import self.sources."${self.nixpkgs-channel}" {config.allowUnfree = true;};
  unstableUnfree = import self.sources.nixos-unstable {config.allowUnfree = true;};
in {
  inherit (unfree) discord zoom-us minecraft teamviewer steam;
  inherit (unstableUnfree) minecraft-server;
}
