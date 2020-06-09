{ pkgs, config, lib, ... }:
let inherit (config.m-0.private) me meWork;
in {

  imports = [
    ./zsh
    ./taskwarrior.nix
    ./home-options.nix
    ../common
    ./unlock.nix
    ./mpclient.nix
    ./neovim
  ];
  nixpkgs.overlays = import ../overlays.nix { inherit lib; };

  programs = {
    home-manager.enable = true;
    direnv = {
      enable = true;
      enableZshIntegration = true;
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
        (exts: [ exts.pass-update pkgs.pass-clip ]);
      enable = true;
      settings.PASSWORD_STORE_DIR =
        "${config.home.homeDirectory}/git/password-store";
    };
    git = {
      aliases = { sync = "!git pull -r && git push"; };
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
      userEmail = me.mail;
      userName = me.name;
    };
    htop = {
      enable = true;
      hideThreads = true;
      hideUserlandThreads = true;
      highlightBaseName = true;
      shadowOtherUsers = true;
      showProgramPath = false;
      treeView = true;
    };
    ssh = {
      controlMaster = "auto";
      controlPersist = "120";
      enable = true;
      matchBlocks = let
        matheGwProxy = "ssh -q gw nc -q0 %h %p";
        agHost = "fb04217.mathematik.tu-darmstadt.de";
      in {
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
          user = me.user;
        };
        ag-forward = {
          hostname = agHost;
          proxyCommand = matheGwProxy;
          user = meWork.user;
        };
        ag = {
          hostname = agHost;
          user = meWork.user;
        };
        gw = {
          hostname = "gwres4.mathematik.tu-darmstadt.de";
          user = meWork.user;
        };
        shells = {
          hostname = "shells.darmstadt.ccc.de";
          user = me.user;
        };
        vorstand = {
          hostname = "vorstand.darmstadt.ccc.de";
          user = me.user;
        };
        whisky = {
          hostname = "whisky.w17.io";
          user = "chaos";
        };
        kitchen = {
          hostname = "kitchen.w17.io";
          user = "chaos";
        };
        "door.w17.io" = { identityFile = "~/.ssh/door_rsa"; };
      };
    };
  };

  home = {
    packages = builtins.attrValues pkgs.home-pkgs;
    sessionVariables = {
      PATH = "$HOME/.nix-profile/bin:$PATH";
      BROWSER = "${pkgs.firefox}/bin/firefox";
      EMAIL = me.mail;
      SUDO_ASKPASS = let
        print-pw = pkgs.writeShellScriptBin "print-pw"
          "pass show eu/m-0/${config.m-0.hostName}.m-0.eu/${config.home.username}";
      in "${print-pw}/bin/print-pw";
    };
    file.".direnvrc".text = "source ${pkgs.sources.nix-direnv}/direnvrc";
  };

  systemd.user = { startServices = true; };

  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 31536000; # 1year
      maxCacheTtl = 31536000; # 1year
    };
  };

  xdg.enable = true;
}
