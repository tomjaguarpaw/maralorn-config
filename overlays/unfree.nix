final: _prev:
let
  unfree = import final.flake-inputs.nixos-stable {
    inherit (final) system;
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
