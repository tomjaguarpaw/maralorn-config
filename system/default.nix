{ pkgs, config, lib, ... }:
{
  # channel = 18.03

  imports = [
    <home-manager/nixos>
    ../common/secret
    ../common/private-options.nix
    ./modules/laptop.nix
    ./modules/server
    ./modules/standalone
    ./modules/cdarknet
    ./modules/loginctl-linger.nix
  ];


  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  networking = {
    firewall.allowPing = true;
    useDHCP = false;
  };

  users = {
    mutableUsers = false;
  };
}
