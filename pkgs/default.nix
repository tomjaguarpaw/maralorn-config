let
  my-lib = import ../lib;
  unfreePkgs = import <nixpkgs> { config = { allowUnfree = true; }; };
  inherit (my-lib)
    pkgs unstable sources writeHaskellScript gcRetentionDays unBreak colors;
in rec {
  ate = pkgs.callPackage (import sources.ate) {
    config.ate = {
      options = {
        BLACK = colors.black;
        RED = colors.red;
        GREEN = colors.green;
        YELLOW = colors.yellow;
        BLUE = colors.blue;
        MAGENTA = colors.magenta;
        CYAN = colors.cyan;
        WHITE = colors.white;
        BRIGHT_BLACK = colors.brightBlack;
        BRIGHT_RED = colors.brightRed;
        BRIGHT_GREEN = colors.brightGreen;
        BRIGHT_YELLOW = colors.brightYellow;
        BRIGHT_BLUE = colors.brightBlue;
        BRIGHT_MAGENTA = colors.brightMagenta;
        BRIGHT_CYAN = colors.brightCyan;
        BRIGHT_WHITE = colors.brightWhite;
        FOREGROUND_COLOR = colors.foreground;
        BACKGROUND_COLOR = colors.background;
      };
      keybindings = {
        INCREMENT_FONT = "control+plus";
        DECREMENT_FONT = "control+minus";
      };
    };
  };

  start-ssh-agent = pkgs.writeShellScriptBin "start-ssh-agent" ''
    ${pkgs.psmisc}/bin/killall -q ssh-agent
    eval `${pkgs.openssh}/bin/ssh-agent -s`
    systemctl --user set-environment SSH_AUTH_SOCK="$SSH_AUTH_SOCK"
    systemctl --user set-environment SSH_AGENT_PID="$SSH_AGENT_PID"
  '';
  cat-pw = pkgs.writeShellScriptBin "cat-ssh-pw" ''
    pass eu/m-0/$(hostname).m-0.eu/ssh-key
  '';
  my-ssh-add = pkgs.writeShellScriptBin "my-ssh-add" ''
    SSH_ASKPASS=${cat-pw}/bin/cat-ssh-pw ${pkgs.openssh}/bin/ssh-add < /dev/null
  '';

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

  neovim = unstable.neovim.override {
    vimAlias = true;
    withPython3 = true;
  };

  home-neovim = (import ./nvim) neovim;

  niv = (import sources.niv { }).niv;

  # pkgs assumed to be present on a non nixos host
  core-system-pkgs = {
    inherit neovim;
    inherit (pkgs)
      gitFull gnumake python3 mkpasswd file wget curl wireguard gnupg mutt bind
      liboping psmisc unzip rename whois lsof parted;
  };

  extra-system-pkgs = {
    inherit niv;
    inherit (pkgs.gitAndTools) git-annex;
    inherit (pkgs.rxvt_unicode) terminfo;
    inherit (pkgs.pythonPackages) qrcode;
    inherit (pkgs)
      git-crypt htop tree pwgen borgbackup inotifyTools direnv socat nmap ncdu
      tcpdump tmux tig exa fzf ag fd bat ripgrep ranger pass sshuttle vnstat;
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
      git -C ~/git/config pull
      update-home
      sudo -A update-system
      #sudo -A nix-collect-garbage --delete-older-than ${
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
    discord = unfreePkgs.discord;
    cachix = import sources.cachix;
    inherit (pkgs.gnome3) nautilus;
    inherit (pkgs.xorg) xev xbacklight;
    inherit (pkgs.gitAndTools) hub;
    inherit (pkgs)
    # web
      chromium

      upower speedtest-cli

      # communication
      signal-desktop tdesktop acpi dino mumble

      # config
      arandr

      #dev
      meld icedtea8_web octave filezilla

      # tools & office
      feh gimp imagemagick ghostscript libreoffice-fresh pandoc xournal
      musescore handbrake evince abcde beets

      networkmanagerapplet
      #    teamviewer

      # media
      ncpamixer pavucontrol deluge gmpc calibre mpv youtubeDL

      # games
      minetest

      nix-review gparted;
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
    if [[ -z "$@" ]]; then
      ${ate}/bin/ate
    else
      shift
      ${ate}/bin/ate /usr/bin/env "$@"
    fi
  '';
  desktop-pkgs = {
    inherit urxvt terminal ate;
    inherit (pkgs.gnome3) dconf;
    inherit (pkgs)
      lm_sensors sway swaylock swayidle xwayland rofi i3status-rust waybar dmenu
      xdg_utils;
    inherit (unstable) wofi;

  };
  home-pkgs = {
    nixfmt = import sources.nixfmt { };
    inherit (pkgs.pythonPackages) yapf jsbeautifier;
    inherit (pkgs) ncmpcpp shfmt htmlTidy astyle nodejs;
    inherit (my-lib) ghc;
    inherit home-neovim ghcide;
    cabal-fmt = (unBreak unstable.haskell.packages.ghc881.cabal-fmt);
  };
  accounting-pkgs = {
    jali = pkgs.callPackage ./jali { };
    inherit (pkgs.haskellPackages) hledger hledger-ui hledger-web;
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
