let
  my-lib = import ../lib;
  inherit (my-lib) pkgs unstable sources writeHaskellScript gcRetentionDays;
in rec {
  tasktree = pkgs.callPackage ./tasktree { };
  neovim = pkgs.neovim.override {
    vimAlias = true;
    withPython3 = true;
  };
  lorri = import sources.lorri {
    src = sources.lorri;
    pkgs = unstable;
  };
  home-neovim = (import ./nvim) neovim;
  niv = (import sources.niv { }).niv;

  # pkgs assumed to be present on a non nixos host
  core-system-pkgs = {
    inherit neovim;
    inherit (pkgs)
      gitFull gnumake python3 mkpasswd file wget curl wireguard gnupg mutt bind
      liboping psmisc unzip rename whois lsof;
  };

  extra-system-pkgs = {
    inherit niv lorri;
    inherit (pkgs.gitAndTools) git-annex;
    inherit (pkgs.rxvt_unicode) terminfo;
    inherit (pkgs.pythonPackages) qrcode;
    inherit (pkgs)
      git-crypt htop tree pwgen borgbackup inotifyTools direnv socat nmap
      tcpdump tmux tig exa fzf ag fd bat ripgrep ranger pass sshuttle;
  };
  gw2wrapper = writeHaskellScript {
    name = "gw2wrapper";
    bins = [ pkgs.procps ];
    imports =
      [ "System.Directory (withCurrentDirectory)" "Control.Monad (when)" ];

  } ''
    waitForExit = do
      sleep "5s"
      processes <- readTrim $ ps "aux"
      when
        (BSC.isInfixOf (BSC.pack "GW2.exe") (LBSC.toStrict processes))
        waitForExit
    main = do
      withCurrentDirectory "/home/maralorn/GW2" $ exe "./play.sh"
      waitForExit
  '';

  laptop-home-pkgs = {
    maintenance = pkgs.writeShellScriptBin "maintenance" ''
      git -C ~/git/nixos/config pull
      update-home
      sudo -A update-system
      sudo -A nix-collect-garbage --delete-older-than ${
        toString gcRetentionDays
      }d
      sudo -A nix optimise-store
    '';
    rewlan = pkgs.writeShellScriptBin "rewlan" ''
      nmcli r wifi off;
      sleep 0.1s;
      nmcli r wifi on;
    '';
    gw2 = pkgs.buildFHSUserEnv {
      name = "gw2";
      targetPkgs = pkgs: (with pkgs; [ sambaFull ]);
      multiPkgs = pkgs:
        (with pkgs;
          with xorg; [
            file
            freetype
            libpng
            mesa_drivers
            zlib
            libXi
            libXcursor
            libXrandr
            libXrender
            libXxf86vm
            libXcomposite
            libXext
            libX11
            libudev
            libGLU_combined
            mesa_noglu.osmesa
            libdrm
            libpulseaudio
            alsaLib
            openal
            mpg123
            libtxc_dxtn
            gnutls
            krb5Full
          ]);
      runScript = "${gw2wrapper}/bin/gw2wrapper";
    };
    cachix = import sources.cachix { };
    inherit (pkgs.gnome3) nautilus;
    inherit (pkgs.xorg) xev xbacklight;
    inherit (pkgs)
    # web
      chromium

      # communication
      signal-desktop tdesktop acpi dino mumble

      # config
      arandr

      #dev
      meld icedtea8_web octave filezilla

      # tools & office
      feh gimp imagemagick ghostscript libreoffice-fresh pandoc xournal
      musescore handbrake evince

      networkmanagerapplet
      #    teamviewer

      # media
      ncpamixer pavucontrol deluge mpd gmpc calibre mpv youtubeDL

      # games
      minetest;
  };

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
    nixfmt = import sources.nixfmt { };
    inherit (pkgs.pythonPackages) yapf jsbeautifier;
    inherit (pkgs) ncmpcpp shfmt htmlTidy astyle;
    inherit (my-lib) ghc;
    inherit home-neovim;
  };
  accounting-pkgs = {
    jali = pkgs.callPackage ./jali { };
    inherit (pkgs.haskellPackages) hledger hledger-ui;
    inherit (pkgs) ledger;
  };
  system-pkgs = core-system-pkgs // extra-system-pkgs // {
    inherit (import ../lib/test.nix)
      test-system-config test-home-config test-config;
    inherit (my-lib) home-manager;
  };
  foreign-home-pkgs = extra-system-pkgs;
  eventd = pkgs.callPackage ./eventd { };
  email2matrix = pkgs.callPackage ./email2matrix { };
}
