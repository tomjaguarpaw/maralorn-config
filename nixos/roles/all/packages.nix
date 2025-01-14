{ pkgs, ... }:
{
  environment.systemPackages = builtins.attrValues {
    inherit (pkgs)
      asciinema
      bat
      moreutils
      bind
      binutils
      borgbackup
      btop
      builders-configurator
      compsize
      curl
      direnv
      entr
      bcachefs-tools
      eza
      fd
      file
      fzf
      gnumake
      gnupg
      helix
      home-manager
      htop
      inotifyTools
      jq
      libargon2
      liboping
      lsof
      mblaze
      mediainfo
      mkpasswd
      ncdu
      niv
      nix-output-monitor
      nmap
      ntfsprogs
      parted
      psmisc
      pwgen
      python3
      ranger
      broot
      nnn
      rename
      ripgrep
      # build error: ntfy
      sd
      socat
      sshuttle
      sysbench
      tcpdump
      tmux
      tree
      unzip
      ventoy-bin
      vnstat
      wget
      whois
      wireguard-tools
      ;
    inherit (pkgs.bat-extras)
      prettybat
      batwatch
      batpipe
      batman
      batgrep
      batdiff
      ;
    inherit (pkgs.python3Packages) qrcode;
    inherit (pkgs.matrix-synapse-tools) synadm;
  };
}
