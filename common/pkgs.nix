rec {
  inherit (import ../common/lib.nix) niv;
  pkgs = import <nixpkgs> {};
  unstable = import <unstable> {};
  lorriSrc = (import ../nix/sources.nix).lorri;
  lorri = import lorriSrc { src = lorriSrc; pkgs = unstable; };
  tasktree = pkgs.callPackage ../packages/tasktree {};
  neovim = pkgs.neovim.override {
      vimAlias = true;
      withPython3 = true;
    };
  home-neovim = (import ../home-manager/nvim) neovim;

  # pkgs assumed to be present on a non nixos host
  core-system-pkgs = {
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

  extra-system-pkgs = {
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

  laptop-home-pkgs = {
    maintenance = pkgs.writeShellScriptBin "maintenance" ''
      git -C ~/git/nixos/config pull
      update-home
      sudo system-maintenance
    '';
    rewlan = pkgs.writeShellScriptBin "rewlan" ''
      nmcli r wifi off;
      sleep 0.1s;
      nmcli r wifi on;
    '';
    cachix = import (import ../nix/sources.nix).cachix {};
    nixfmt = import (import ../nix/sources.nix).nixfmt {};
    inherit (pkgs.gnome3) nautilus;
    inherit (unstable.haskellPackages) brittany;
    inherit (pkgs.xorg) xev xbacklight;
    inherit (pkgs)
    # web
    chromium

    # communication
    signal-desktop
    tdesktop
    acpi
    dino
    mumble

    # config
    arandr

    #dev
    meld
    icedtea8_web
    octave
    filezilla

    # tools & office
    feh
    gimp
    imagemagick
    ghostscript
    libreoffice-fresh
    pandoc
    xournal
    musescore
    handbrake
    evince


    networkmanagerapplet
#    teamviewer

    # media
    ncpamixer
    pavucontrol
    deluge
    mpd
    gmpc
    calibre
    mpv
    youtubeDL

    minetest
  ;};

  my-home-pkgs = {
    print215 = pkgs.writeShellScriptBin "print215" ''
      scp "$@" ag-forward:
      ssh ag-forward lpr -Zduplex -r "$@"
    '';
  print215single = pkgs.writeShellScriptBin "print215single" ''
    scp "$@" ag-forward:
    ssh ag-forward lpr -r "$@"
  '';
};
  urxvt = pkgs.rxvt_unicode-with-plugins;
  terminal = pkgs.writeShellScriptBin "terminal" ''
      ${urxvt}/bin/urxvtc "$@"
      if [ $? -eq 2 ]; then
         ${urxvt}/bin/urxvtd -q -o -f
         ${urxvt}/bin/urxvtc "$@"
      fi
    '';
  desktop-pkgs = {
    inherit urxvt tasktree terminal;
    inherit (pkgs) xautolock;
    inherit (pkgs.gnome3) dconf;
  };
home-pkgs = {
  inherit (pkgs) ncmpcpp;
  inherit home-neovim;
};
accounting-pkgs = {
  jali = pkgs.callPackage ../packages/jali {};
  inherit (pkgs.haskellPackages) hledger hledger-ui;
  inherit (pkgs) ledger;
};
  system-pkgs = core-system-pkgs // extra-system-pkgs // {
    inherit (import ./test-lib.nix) test-system-config test-home-config test-and-bump-config;
    inherit (import ../common/lib.nix) home-manager;
   };
  foreign-home-pkgs = extra-system-pkgs;
  eventd = pkgs.callPackage ../packages/eventd {};
}
