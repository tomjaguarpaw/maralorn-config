{
  core = pkgs: with pkgs; [
    git
    gnumake
    python3
    mkpasswd
    file
    wget
    curl
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

    (import <unstable> {}).home-manager

    socat
    nmap
    tcpdump

    git-crypt
    tmux
    tig
    exa
    fzf
    ag

    pythonPackages.qrcode
    ranger

    pass
    sshuttle
    mtr
    ];
}
