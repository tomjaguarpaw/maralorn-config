self: super: {
  # pkgs assumed to be present on a non nixos host
  core-system-pkgs = {
    inherit (self)
      gitFull gnumake mkpasswd file wget curl wireguard gnupg mutt bind liboping
      psmisc unzip rename whois lsof parted python3 binutils ntfsprogs neovim;
  };

  extra-system-pkgs = {
    inherit (self.python3Packages) qrcode;
    inherit (self)
      git-crypt htop tree pwgen borgbackup inotifyTools direnv socat nmap ncdu
      tcpdump tmux tig exa fzf ag fd bat ripgrep ranger pass sshuttle vnstat
      entr libargon2 mblaze niv;
  };

  my-home-pkgs = {
    print215 = super.writeShellScriptBin "print215" ''
      scp "$@" ag-forward:
      ssh ag-forward lpr -Zduplex -r "$@"
    '';
    print215single = super.writeShellScriptBin "print215single" ''
      scp "$@" ag-forward:
      ssh ag-forward lpr -r "$@"
    '';
  };
  desktop-pkgs = {
    inherit (self) lm_sensors xwayland xdg_utils libnotify;
    inherit (self.gnomeExtensions) appindicator system-monitor;
    inherit (self.gnome3)
      dconf dconf-editor gnome-tweaks gnome-shell-extensions adwaita-icon-theme
      gnome-session;
    hotkeys = super.writeShellScriptBin "hotkeys"
      ''exec /home/maralorn/.cargo/bin/hotkeys "$@"'';
  };
  home-pkgs = {
    inherit (self.pythonPackages) yapf jsbeautifier;
    inherit (self)
      go gdb mpc_cli ncmpcpp shfmt htmlTidy astyle nodejs tasksh magic-wormhole
      nixfmt rnix-lsp tmate rustup kitty nix-top ghc ghcid ormolu;
    obelisk = (import self.sources.obelisk { }).command;
  };
  accounting-pkgs = {
    inherit (self.haskellPackages) hledger hledger-ui hledger-web;
    inherit (self) ledger jali;
  };
  system-pkgs = self.core-system-pkgs // self.extra-system-pkgs // {
    inherit (self) test-system-config test-home-config test-config;
    home-manager =
      self.callPackage "${self.sources.home-manager}/home-manager" { };
  };
  foreign-home-pkgs = self.extra-system-pkgs;
}
