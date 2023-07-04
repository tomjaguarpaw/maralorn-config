{
  config,
  pkgs,
  lib,
  ...
}:
let
  openssh.authorizedKeys.keys = pkgs.privateValue [ ] "ssh-keys";
  passwordFile = lib.mkIf (config.networking.hostName != "chor-cloud") (
    lib.mkDefault config.age.secrets.pam-long-password.path
  );
in
{
  users.users = {
    maralorn = {
      description = "maralorn";
      isNormalUser = true;
      uid = 1000;
      extraGroups = [
        "wheel"
        "systemd-journal"
        "networkmanager"
        "docker"
        "video"
        "keys"
        "adbusers"
        "dialout"
      ];
      inherit openssh passwordFile;
    };
    root = {
      inherit openssh passwordFile;
    };
  };
}
