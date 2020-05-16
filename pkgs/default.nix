let
  my-lib = import ../lib;
  inherit (my-lib) pkgs unstable sources writeHaskellScript unBreak colors;
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
  obelisk = (import sources.obelisk { }).command;
  nix-direnv = sources.nix-direnv + "/direnvrc";
  neovim = unstable.neovim.override {
    vimAlias = true;
    withPython3 = true;
    withPython = false;
  };

  # pkgs assumed to be present on a non nixos host
  core-system-pkgs = {
    inherit neovim;
    inherit (pkgs)
      gitFull gnumake mkpasswd file wget curl wireguard gnupg mutt bind liboping
      psmisc unzip rename whois lsof parted python3 binutils ntfsprogs;
  };

  extra-system-pkgs = {
    inherit (pkgs.gitAndTools) git-annex;
    inherit (pkgs.rxvt_unicode) terminfo;
    inherit (pkgs.python3Packages) qrcode;
    inherit (pkgs)
      git-crypt htop tree pwgen borgbackup inotifyTools direnv socat nmap ncdu
      tcpdump tmux tig exa fzf ag fd bat ripgrep ranger pass sshuttle vnstat
      entr libargon2 mblaze niv;
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
    inherit (pkgs)
      kitty lm_sensors sway swaylock swayidle xwayland rofi dmenu xdg_utils
      gnome-themes-extra gnome-themes-standard libnotify mako wofi;
    inherit (pkgs.gnomeExtensions)
      appindicator arc-menu sound-output-device-chooser system-monitor;
    inherit (pkgs.gnome3)
      dconf dconf-editor gnome-tweaks gnome-shell-extensions adwaita-icon-theme
      gnome-session;
  };
  home-pkgs = {
    inherit (pkgs.pythonPackages) yapf jsbeautifier;
    inherit (pkgs)
      mpc_cli ncmpcpp shfmt htmlTidy astyle nodejs tasksh magic-wormhole nixfmt
      stack ghcid rnix-lsp;
    inherit (my-lib) ghc;
    inherit obelisk;
    cabal-fmt = (unBreak pkgs.haskell.packages.ghc881.cabal-fmt);
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
