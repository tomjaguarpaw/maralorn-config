{ pkgs, config, lib, ... }: {

  imports = [
    ./zsh
    ./taskwarrior.nix
    ./home-options.nix
    ../../common
    ./unlock.nix
    ./mpclient.nix
    ./neovim
  ];
  nixpkgs.overlays = import ../../overlays { inherit lib; };

  programs = {
    home-manager.enable = true;
    direnv = {
      enable = true;
      enableZshIntegration = true;
      enableNixDirenvIntegration = true;
    };
    tmux = {
      enable = true;
      escapeTime = 1;
      historyLimit = 50000;
      terminal = "screen-256color";
      extraConfig = ''
        set -g set-titles on
        set -g status off
      '';
    };
    password-store = {
      package = pkgs.pass-wayland.withExtensions
        (exts: [ exts.pass-update pkgs.pass-clip exts.pass-otp ]);
      enable = true;
      settings.PASSWORD_STORE_DIR =
        "${config.home.homeDirectory}/git/password-store";
    };
    git = {
      aliases = {
        sync = "!git pull -r && git push";
        cpr =
          "!f() { git fetch origin refs/pull/$1/head && git checkout FETCH_HEAD; }; f";
      };
      extraConfig.pull.ff = "only";
      extraConfig.core.editor = "nvim";
      enable = true;
      ignores = [
        ".syncthing*.tmp"
        "*.swp"
        "*.autosave~"
        "*.aux"
        "*.bbl"
        "*.fls"
        "*.idx"
        "*.ilg"
        "*.ind"
        "*.log"
        "*.out"
        "*.toc"
        "*.bcf"
        "*.blg"
        "*.fdb*"
        "*.thm"
        "*.run.xml"
        "*.slnc"
        "*.glade~"
        "__pycache__"
        ".hledger-web_client_session_key.aes"
        ".nix-gc-roots"
      ];
      userEmail = "malte.brandy@maralorn.de";
      userName = "Malte Brandy";
    };
    htop = {
      enable = true;
      hideThreads = true;
      hideUserlandThreads = true;
      highlightBaseName = true;
      shadowOtherUsers = true;
      showProgramPath = false;
      treeView = true;
      sortKey = "USER";
    };
    ssh = {
      controlMaster = "auto";
      controlPersist = "120";
      enable = true;
      matchBlocks = let agHost = "fb04217.mathematik.tu-darmstadt.de";
      in {
        athene.hostname = "192.168.178.22";
        git-auto = {
          hostname = "hera.m-0.eu";
          user = "git";
          identityFile = "~/.ssh/id_auto_ed25519";
        };
        git = {
          hostname = "hera.m-0.eu";
          user = "git";
        };
        hera = {
          hostname = "hera.m-0.eu";
          user = "maralorn";
        };
        ag-forward = {
          hostname = agHost;
          proxyJump = "gw";
          user = "brandy";
        };
        ag = {
          hostname = agHost;
          user = "brandy";
        };
        gw = {
          hostname = "gwres4.mathematik.tu-darmstadt.de";
          user = "brandy";
        };
        shells = {
          hostname = "shells.darmstadt.ccc.de";
          user = "maralorn";
        };
        whisky = {
          hostname = "whisky.w17.io";
          user = "chaos";
        };
        kitchen = {
          hostname = "kitchen.w17.io";
          user = "chaos";
        };
        "door.w17.io".identityFile = "~/.ssh/door_rsa";
      };
    };
  };

  home = {
    packages = builtins.attrValues pkgs.home-pkgs ++ [
      (pkgs.writeShellScriptBin "unlock-ssh" ''
        SSH_ASKPASS="${config.home.sessionVariables.SSH_ASKPASS}" DISPLAY="a" ssh-add < /dev/null
      '')
    ];
    sessionVariables = {
      PATH = "$HOME/.nix-profile/bin:$PATH";
      BROWSER = "${pkgs.firefox}/bin/firefox";
      EMAIL = "malte.brandy@maralorn.de";
      SUDO_ASKPASS = toString (pkgs.writeShellScript "print-sudo-pw"
        "pass show eu/m-0/${config.m-0.hostName}.m-0.eu/${config.home.username}");
      SSH_ASKPASS = toString (pkgs.writeShellScript "print-ssh-pw"
        "pass show eu/m-0/${config.m-0.hostName}.m-0.eu/ssh-key");
    };
  };

  systemd.user.startServices = true;

  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 31536000; # 1year
      maxCacheTtl = 31536000; # 1year
    };
  };

  xdg.enable = true;
}
