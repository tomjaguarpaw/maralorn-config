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
      htop tree pwgen borgbackup inotifyTools direnv socat nmap ncdu
      tcpdump tmux tig exa fzf ag fd bat ripgrep ranger pass sshuttle vnstat
      entr libargon2 mblaze niv compsize mediainfo asciinema gomuks nix-output-monitor fdroidserver;
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
    mytmux = super.writeShellScriptBin "mytmux" ''
     session=$(${self.tmux}/bin/tmux ls | grep -v attached | head -1 | cut -f1 -d:)
     if [[ -n $session ]]; then
        exec ${self.tmux}/bin/tmux attach -t $session;
     else
        exec ${self.tmux}/bin/tmux;
     fi
    '';
  };
  desktop-pkgs = {
    radicle-upstream = import self.sources.radicle-upstream-nix {};
    unison = (import self.sources.unison-nix).unison-ucm;
    inherit (self) lm_sensors xwayland xdg_utils libnotify kassandra kassandra2 shotcut mlt audacity;
    inherit (self.gnomeExtensions) appindicator system-monitor;
    inherit (self.gnome3)
      dconf dconf-editor gnome-tweaks gnome-shell-extensions adwaita-icon-theme
      gnome-session;
  };
  home-pkgs = {
    inherit (self.pythonPackages) yapf jsbeautifier;
    inherit (self)
      go gdb mpc_cli ncmpcpp shfmt htmlTidy astyle nodejs tasksh magic-wormhole
      nixfmt nixpkgs-fmt rnix-lsp tmate rustup kitty nix-top ghcWithPackages ghcid matrix-commander;
    obelisk = (import self.sources.obelisk { }).command;
  };
  accounting-pkgs = {
    inherit (self.haskellPackages) hledger hledger-ui hledger-web hledger-iadd;
    inherit (self) ledger jali aqbanking;
  };
  system-pkgs = self.core-system-pkgs // self.extra-system-pkgs // {
    home-manager =
      self.callPackage "${self.sources.${self.home-manager-channel}}/home-manager" { };
  };
  foreign-home-pkgs = self.extra-system-pkgs;
}
