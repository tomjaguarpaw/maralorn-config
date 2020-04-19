let
  my-lib = import ../lib;
  unfreePkgs = import <nixpkgs> { config = { allowUnfree = true; }; };
  inherit (my-lib)
    pkgs unstable sources writeHaskellScript gcRetentionDays unBreak colors;
in rec {
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
  zsh-powerlevel10k = unstable.zsh-powerlevel10k;
  ghcide = (import sources.ghcide { }).ghcide-ghc865;
  obelisk = (import sources.obelisk { }).command;
  nix-direnv = sources.nix-direnv + "/direnvrc";
  neovim = unstable.neovim.override {
    vimAlias = true;
    withPython3 = true;
    withPython = false;
  };
  home-neovim = (import ./nvim) neovim;
  niv = unstable.niv;

  # pkgs assumed to be present on a non nixos host
  core-system-pkgs = {
    inherit neovim;
    inherit (pkgs)
      gitFull gnumake mkpasswd file wget curl wireguard gnupg mutt bind liboping
      psmisc unzip rename whois lsof parted python3 binutils;
  };

  extra-system-pkgs = {
    inherit niv;
    inherit (pkgs.gitAndTools) git-annex;
    inherit (pkgs.rxvt_unicode) terminfo;
    inherit (pkgs.python3Packages) qrcode;
    inherit (pkgs)
      git-crypt htop tree pwgen borgbackup inotifyTools direnv socat nmap ncdu
      tcpdump tmux tig exa fzf ag fd bat ripgrep ranger pass sshuttle vnstat
      entr libargon2 mblaze;
  };
  gw2wrapper = writeHaskellScript {
    name = "gw2wrapper";
    bins = [ pkgs.procps ];
    imports = [ "System.Directory (withCurrentDirectory)" ];

  } ''
    waitForExit = do
      sleep "5s"
      processes <- ps "aux" |> captureTrim
      when
        (BS.isInfixOf (encodeUtf8 "GW2.exe") (toStrict processes))
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
    inherit (unfreePkgs) discord zoom-us skypeforlinux;
    inherit (pkgs.gnome3) nautilus;
    inherit (pkgs.xorg) xev xbacklight;
    inherit (pkgs.gitAndTools) hub;
    inherit (unstable) mumble;
    inherit (pkgs)
    # web
      chromium

      upower speedtest-cli

      anki

      cachix

      # communication
      signal-desktop tdesktop acpi dino

      # config
      arandr

      #dev
      meld icedtea8_web octave filezilla

      # tools & office
      feh gimp imagemagick libreoffice-fresh xournal musescore handbrake evince
      abcde beets

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
    shift
    ${pkgs.kitty}/bin/kitty "$@"
  '';
  desktop-pkgs = {
    inherit urxvt terminal;
    inherit (pkgs.gnome3) dconf;
    inherit (pkgs)
      kitty lm_sensors sway swaylock swayidle xwayland rofi dmenu xdg_utils
      libnotify mako;
    inherit (unstable) wofi;

  };
  home-pkgs = {
    inherit (pkgs.pythonPackages) yapf jsbeautifier;
    inherit (pkgs)
      mpc_cli ncmpcpp shfmt htmlTidy astyle nodejs tasksh magic-wormhole nixfmt
      stack ghcid;
    inherit (my-lib) ghc;
    inherit home-neovim ghcide obelisk;
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
  email2matrix = pkgs.callPackage ./email2matrix { };
}
