self: super:
let
  unfree = import self.flake-inputs.nixos-stable {
    inherit (self) system;
    config = {
      allowUnfree = true;
      android_sdk.accept_license = true;
    };
  };
in {
  inherit (unfree)
    discord zoom-us minecraft teamviewer steam androidsdk_9_0 factorio
    minecraft-server;
}
