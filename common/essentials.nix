{
  core = pkgs: with pkgs; [
    git
    gnumake
    python3
    mkpasswd
    file
    wget
    curl
    wireguard
    (pkgs.neovim.override {
      vimAlias = true;
      withPython3 = true;
    })
    gnupg
    mutt
    bind
    liboping
    ];

  extra = pkgs: with pkgs; [
    git-crypt
    htop
    tree
    rxvt_unicode.terminfo
    st
    pwgen
    borgbackup

    home-manager

    socat
    nmap
    tcpdump

    tmux
    tig
    exa
    fzf
    ag
    fd
    ripgrep

    pythonPackages.qrcode
    ranger

    pass
    sshuttle
    ];
}
