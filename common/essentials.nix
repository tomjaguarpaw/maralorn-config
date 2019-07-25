let
  inherit (import ../common/lib.nix) niv;
in
{
  core = pkgs: with pkgs; [
    gitFull
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
    psmisc
    unzip
    rename
    whois
    lsof
    ];

    extra = pkgs: with pkgs; let
      lorriSrc = builtins.fetchGit { url = "https://github.com/target/lorri.git"; ref = "rolling-release"; };
      lorri = import "${lorriSrc}/default.nix" { src = lorriSrc; inherit pkgs; };
    in
    [
    niv
    git-crypt
    gitAndTools.git-annex
    htop
    tree
    rxvt_unicode.terminfo
    pwgen
    borgbackup
    inotifyTools

    direnv
    #lorri

    socat
    nmap
    tcpdump

    tmux
    tig
    exa
    fzf
    ag
    fd
    bat

    ripgrep

    pythonPackages.qrcode
    ranger

    pass
    sshuttle
    ];
}
