{
  config,
  pkgs,
  lib,
  ...
}: let
  openssh.authorizedKeys.keys = pkgs.privateValue [] "ssh-keys";
  passwordFile = config.age.secrets.pam-login-password.path;
in {
  users.users = {
    maralorn = {
      description = "maralorn";
      isNormalUser = true;
      uid = 1000;
      extraGroups = ["wheel" "systemd-journal" "networkmanager" "docker" "video" "adbusers" "dialout"];
      inherit openssh passwordFile;
    };
    root = {inherit openssh passwordFile;};
  };
}
