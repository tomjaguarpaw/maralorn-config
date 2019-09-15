let
  my-lib = import ../lib;
  inherit (my-lib) pkgs unstable sources writeHaskellScript gcRetentionDays;
in rec {
  gitstatus = pkgs.callPackage ./powerlevel10k/gitstatus.nix {
    libgit2 = pkgs.libgit2.overrideAttrs (attrs: {
      src = pkgs.fetchFromGitHub {
        owner = "romkatv";
        repo = "libgit2";
        rev = "a546b232b23814de9c561fc750791363a5ed347e";
        sha256 = "1xx782qa36f5gfjflw64r383g5gh7wgkvxk5q4w0rg8c0xwa3hk6";
      };
    });
  };
  zsh-powerlevel10k =
    pkgs.callPackage ./powerlevel10k/zsh-powerlevel10k.nix { };
  ghcide = (import sources.ghcide { }).ghcide-ghc865;
  tasktree = pkgs.callPackage ./tasktree { };

  libluv = unstable.callPackage ./libluv { };

  libvterm-neovim = unstable.libvterm-neovim.overrideAttrs (attrs: {
    version = "2019-08-28";

    src = pkgs.fetchFromGitHub {
      owner = "neovim";
      repo = "libvterm";
      rev = "1aa95e24d8f07a396aa80b7cd52f93e2b5bcca79";
      sha256 = "0vjd397lqrfv4kc79i5izva4bynbymx3gllkg281fnk0b15vxfif";
    };
  });

  neovim-unwrapped = unstable.neovim-unwrapped.overrideAttrs (attrs: {
    version = "nightly-2019-0914";
    src = pkgs.fetchFromGitHub {
      owner = "neovim";
      repo = "neovim";
      rev = "8c88d98";
      sha256 = "11n0yzk55x9w3hldq7mp07q2fa94ksc20qmh3llq8mj976675i48";
    };
    buildInputs = [ libvterm-neovim ] ++ attrs.buildInputs ++ [ libluv ];
  });

  neovim = (unstable.wrapNeovim neovim-unwrapped { }).override {
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
      processes <- ps "aux" |> captureTrim
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
    cachix = import sources.cachix;
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
    inherit (pkgs) ncmpcpp shfmt htmlTidy astyle nodejs;
    inherit (my-lib) ghc;
    inherit home-neovim ghcide;
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
