{ pkgs, ... }:
{
  imports = [
    ./admin.nix
    ./syncthing.nix
  ];

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };
  time.timeZone = "Europe/Berlin";

  # So that boot does not fill up with old kernels
  boot.loader.grub.configurationLimit = 5;

  nix = {
    gc = {
       automatic = true;
       options = "--delete-older-than 5d";
    };
    optimise.automatic = true;
    package = pkgs.nixUnstable;
  };
  system.autoUpgrade.enable = true;
  system.autoUpgrade.dates = "22:00";

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;
  };

  networking.firewall.allowPing = true;

  services = {
    sshd.enable = true;
  };

  environment = {
    systemPackages = with pkgs; [
      git
      gnumake
      python3
      python
      pandoc
      mkpasswd
      rxvt_unicode.terminfo
      htop
      file
      tmux
      socat
      tcpdump
      wget
      curl
      neovim
    ];
    sessionVariables = {
      TERMINFO = "/run/current-system/sw/share/terminfo";
    };
  };

  programs = {
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
    };
    vim.defaultEditor = true;
  };
}
