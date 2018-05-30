{ pkgs, ... }:
{
  # channel = 18.03

  imports = [
    ./secret-option.nix
    ./laptop.nix
    ./admin.nix
    ./syncthing.nix
    ./cdarknet
  ];

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  security.rngd.enable = true;

  # So that boot does not fill up with old kernels
  boot.loader.grub.configurationLimit = 5;

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;
  };

  security.sudo.extraConfig = "
    Defaults timestamp_type=global, timestamp_timeout=15
  ";

  networking.firewall.allowPing = true;

  services = {
    sshd.enable = true;
  };

  environment = {
    systemPackages = with pkgs; [
      git-crypt
      git
      gnumake
      python3
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
