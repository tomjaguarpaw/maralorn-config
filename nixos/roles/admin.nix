{ config, pkgs, lib, ... }:
let
  passwordFile = pkgs.privatePath "pam-login-password";
  openssh.authorizedKeys.keys = pkgs.privateValue [ ] "ssh-keys";
in
{
  users.users = {
    maralorn = {
      description = "maralorn";
      isNormalUser = true;
      uid = 1000;
      extraGroups =
        [ "wheel" "systemd-journal" "networkmanager" "docker" "video" ];
      inherit openssh passwordFile;
    };
    root = { inherit openssh passwordFile; };
  };
}
