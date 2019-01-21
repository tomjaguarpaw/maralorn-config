{ pkgs, config, lib, ... }:
with lib;
{

imports = [ ./admin.nix ];

options.m-0.standalone.enable = mkOption {
  type = types.bool;
  default = false;
};

config = mkIf config.m-0.standalone.enable {
  # So that boot does not fill up with old kernels
  boot.loader.grub.configurationLimit = 5;

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;
  };

  security.sudo.extraConfig = "
    Defaults timestamp_type=global, timestamp_timeout=15
  ";

  services = {
    sshd.enable = true;
  };

  environment = {
    # Put these into an extra file so the essential packages can also be included on non selfadminstrated systems from home-manager
    systemPackages = let essentials = import ../../../common/essentials.nix;
  in (essentials.core pkgs) ++ (essentials.extra pkgs);
    sessionVariables = {
      TERMINFO = "/run/current-system/sw/share/terminfo";
    };
  };

  programs = {
    mtr.enable = true;
    zsh = {
      enable = true;
      autosuggestions.enable = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
    };
  };
};

}
