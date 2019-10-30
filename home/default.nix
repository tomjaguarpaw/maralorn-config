{ pkgs, config, ... }:
let
  inherit (config.m-0.private) me meWork;
  my-pkgs = import ../pkgs;
  inherit (my-pkgs) lorri;
in {

  imports = [
    ./zsh
    ./update-script.nix
    ./modules/taskwarrior.nix
    ./modules/force-copies.nix
    ./modules/accounting
    ./modules/rustdev.nix
    ./modules/latex.nix
    ./modules/mail.nix
    ./modules/home-options.nix
    ./modules/unlock.nix
    ./modules/weechat
    ./modules/bugwarrior.nix
    ./modules/pythia.nix
    ../common
  ];

  programs = {
    home-manager.enable = true;
    direnv = {
      enable = true;
      enableZshIntegration = true;
    };
    tmux = {
      enable = true;
      extraConfig = ''
        set default-terminal "screen-256color"
        set -g set-titles on
        set -g status off
        set -g escape-time 1
      '';
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
      in [
        {
          host = "git-auto";
          hostname = "hera.m-0.eu";
          user = "git";
          identityFile = "~/.ssh/id_auto_ed25519";
        }
        {
          host = "git";
          hostname = "hera.m-0.eu";
          user = "git";
        }
        {
          host = "hera";
          hostname = "hera.m-0.eu";
          user = me.user;
        }
        {
          host = "ag-forward";
          hostname = agHost;
          proxyCommand = matheGwProxy;
          user = meWork.user;
        }
        {
          host = "ag";
          hostname = agHost;
          user = meWork.user;
        }
        {
          host = "gw";
          hostname = "gwres4.mathematik.tu-darmstadt.de";
          user = meWork.user;
        }
        {
          host = "shells";
          hostname = "shells.darmstadt.ccc.de";
          user = me.user;
        }
        {
          host = "vorstand";
          hostname = "vorstand.darmstadt.ccc.de";
          user = me.user;
        }
        {
          host = "whisky";
          hostname = "whisky.w17.io";
          user = "chaos";
        }
        {
          host = "kitchen";
          hostname = "kitchen.w17.io";
          user = "chaos";
        }
        {
          host = "door.w17.io";
          identityFile = "~/.ssh/door_rsa";
        }
      ];
    };
  };

  home = {
    packages = builtins.attrValues my-pkgs.home-pkgs;
    sessionVariables = {
      PATH =
        "$HOME/.cargo/bin:/etc/profiles/per-user/${config.home.username}/bin:$HOME/.nix-profile/bin:$PATH";
      BROWSER = "${pkgs.firefox}/bin/firefox";
      EDITOR = "${pkgs.neovim}/bin/nvim";
      TERMINAL = config.m-0.terminal;
      EMAIL = me.mail;
      SUDO_ASKPASS = let
        print-pw = pkgs.writeShellScriptBin "print-pw"
          "pass show eu/m-0/${config.m-0.hostName}/user/${config.home.username}";
      in "${print-pw}/bin/print-pw";
    };
    file.".config/nvim/coc-settings.json".text = builtins.toJSON {
      "diagnostic.maxWindowHeight" = 60;
      languageserver = {
        haskell = {
          command = "ghcide";
          args = [ "--lsp" ];
          rootPatterns = [
            ".stack.yaml"
            ".hie-bios"
            "BUILD.bazel"
            "cabal.config"
            "package.yaml"
          ];
          filetypes = [ "hs" "lhs" "haskell" ];
        };
      };
    };
  };

  systemd.user = {
    startServices = true;
    services.lorri-daemon = {
      Unit = { Description = "Run lorri daemon"; };
      Service = {
        Environment =
          "RUST_BACKTRACE=1 PATH=${pkgs.nix}/bin:${pkgs.coreutils}/bin:${pkgs.gnutar}/bin:${pkgs.gzip}/bin";
        ExecStart = "${lorri}/bin/lorri daemon";
      };
      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 31536000; # 1year
      maxCacheTtl = 31536000; # 1year
    };
  };

  xdg.enable = true;
}
