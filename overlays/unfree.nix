self: super: let
  unfree = import self.flake-inputs.nixos-stable {
    inherit (self) system;
    config = {
      allowUnfree = true;
      android_sdk.accept_license = true;
    };
  };
  unstableUnfree = import self.flake-inputs.nixos-unstable {
    config.allowUnfree = true;
    inherit (self) system;
  };
in {
  inherit (unfree) discord zoom-us minecraft teamviewer steam androidsdk_9_0;
  inherit (unstableUnfree) minecraft-server;
}
