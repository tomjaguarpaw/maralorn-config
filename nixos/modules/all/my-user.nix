{
  config,
  pkgs,
  lib,
  ...
}:
let
  openssh.authorizedKeys.keys = pkgs.privateValue [] "ssh-keys";
  passwordFile = lib.mkDefault config.age.secrets.pam-long-password.path;
in
{
  # Enable lingering
  systemd.tmpfiles.rules = ["f /var/lib/systemd/linger/maralorn"];
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
        "audio"
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
