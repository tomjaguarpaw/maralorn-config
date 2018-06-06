{ pkgs, ... }:
{
  # channel = 18.03

  imports = [
    <home-manager/nixos>
    ../common/secret
    ../common/private-options.nix
    ./modules/laptop.nix
    ./modules/server.nix
    ./admin.nix
    ./syncthing.nix
    ./modules/cdarknet
    ./modules/loginctl-linger.nix
  ];

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  # So that boot does not fill up with old kernels
  boot.loader.grub.configurationLimit = 5;

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;
  };

  security.sudo.extraConfig = "
    Defaults timestamp_type=global, timestamp_timeout=15
  ";

  networking = {
    firewall.allowPing = true;
    useDHCP = false;
  };

  services = {
    sshd.enable = true;
  };

  environment = {
    # Put these into an extra file so the essential packages can also be included on non selfadminstrated systems from home-manager
    systemPackages = let essentials = import ../common/essentials.nix;
  in (essentials.core pkgs) ++ (essentials.extra pkgs);
    sessionVariables = {
      TERMINFO = "/run/current-system/sw/share/terminfo";
    };
  };

  programs = {
    mtr.enable = true;
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
    };
  };
}
