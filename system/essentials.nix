{
  core = pkgs: with pkgs; [
    git
    gnumake
    python3
    mkpasswd
    file
    wget
    curl
    (pkgs.neovim.override {vimAlias = true;})
    ];
  extra = pkgs: with pkgs; [
    git-crypt
    htop
    tree
    rxvt_unicode.terminfo
    st.terminfo

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
    ];
}
