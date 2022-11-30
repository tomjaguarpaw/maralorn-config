self: super: let
  unfree = import self.sources."${self.nixpkgs-channel}" {
    config = {
      allowUnfree = true;
      android_sdk.accept_license = true;
    };
  };
  unstableUnfree = import self.sources.nixos-unstable {config.allowUnfree = true;};
in {
  inherit (unfree) discord zoom-us minecraft teamviewer steam androidsdk_9_0;
  inherit (unstableUnfree) minecraft-server;
}
