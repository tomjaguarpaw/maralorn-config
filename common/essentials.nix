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
    ];

  extra = pkgs: with pkgs; [
    git-crypt
    htop
    tree
    rxvt_unicode.terminfo
    st
    pwgen
    borgbackup

    (import <unstable> {}).home-manager

    socat
    nmap
    tcpdump

    tmux
    tig
    exa
    fzf
    ag

    pythonPackages.qrcode
    ranger

    pass
    sshuttle
    ];
}
