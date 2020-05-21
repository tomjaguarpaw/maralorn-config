let
  my-lib = import ../lib;
  inherit (my-lib) pkgs unstable sources writeHaskellScript unBreak colors;
in rec {
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
  desktop-pkgs = {
    inherit (pkgs) lm_sensors xwayland dmenu xdg_utils libnotify pinentry;
    inherit (pkgs.gnomeExtensions) appindicator system-monitor;
    inherit (pkgs.gnome3)
      dconf dconf-editor gnome-tweaks gnome-shell-extensions adwaita-icon-theme
      gnome-session;
  };
  home-pkgs = {
    inherit (pkgs.pythonPackages) yapf jsbeautifier;
    inherit (pkgs)
      go gdb mpc_cli ncmpcpp shfmt htmlTidy astyle nodejs tasksh magic-wormhole
      nixfmt stack ghcid rnix-lsp tmate rustup kitty;
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
