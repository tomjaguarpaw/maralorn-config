let
  inherit (import ../common/lib.nix) niv;
  pkgs = import <nixpkgs> {};
  unstable = import <unstable> {};
  lorriSrc = (import ../nix/sources.nix).lorri;
  lorri = import lorriSrc { src = lorriSrc; pkgs = unstable; };
  neovim = pkgs.neovim.override {
      vimAlias = true;
      withPython3 = true;
    };
in
{
  core = builtins.attrValues {
    inherit neovim;
      inherit (pkgs)
    gitFull
    gnumake
    python3
    mkpasswd
    file
    wget
    curl
    wireguard
    gnupg
    mutt
    bind
    liboping
    psmisc
    unzip
    rename
    whois
    lsof;
  };

  extra = builtins.attrValues {
    inherit lorri niv;
    inherit (pkgs.gitAndTools) git-annex;
    inherit (pkgs.rxvt_unicode) terminfo;
    inherit (pkgs.pythonPackages) qrcode;
    inherit (pkgs)

    git-crypt
    htop
    tree
    pwgen
    borgbackup
    inotifyTools

    direnv

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

    ranger

    pass
    sshuttle;
  };
}
