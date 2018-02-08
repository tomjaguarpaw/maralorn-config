{ pkgs, ... }:
{
  programs.home-manager = {
    enable = true;
    path = https://github.com/rycee/home-manager/archive/release-17.09.tar.gz;
  };
  systemd.user.startServices = true;

  home.packages = with pkgs; [
    htop
    tree
    rxvt_unicode.terminfo

    socat
    nmap
    tcpdump

    rcm
    tmux
    tig

    neovim
    taskwarrior

    vimPlugins.vundle
    vimPlugins.deoplete-nvim
    vimPlugins.vim-nix
    pythonPackages.qrcode
    ranger
  ];
}
