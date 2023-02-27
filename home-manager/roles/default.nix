{
  pkgs,
  config,
  lib,
  ...
}: {
  imports = [
    ./zsh
    ./home-options.nix
    ../../common
    ./unlock.nix
    ./mpclient.nix
    ./helix.nix
    ./nvd.nix
  ];

  news.display = "silent";

  home.stateVersion = "22.05";

  programs = {
    home-manager.enable = true;
    exa = {
      enable = true;
      enableAliases = true;
    };
    lazygit = {
      settings = {
        gui.skipUnstageLineWarning = true;
        customCommands = [
          {
            key = "s";
            command = "git sync";
            context = "global";
            subprocess = true;
          }
          {
            key = "s";
            command = "git sync";
            context = "files";
            subprocess = true;
          }
        ];
      };
      enable = true;
    };
    direnv = {
      enable = true;
      config.global.warn_timeout = "1h";
      # default: enableZshIntegration = true;
      nix-direnv.enable = true;
    };

    tmux = {
      enable = true;
      escapeTime = 1;
      historyLimit = 50000;
      extraConfig = ''
        set -g set-titles on
        set -g status off
        set -g set-titles-string '#T - #W - tmux @ #h'
        set -g allow-rename on
      '';
    };
    password-store = {
      package = pkgs.pass-wayland.withExtensions (exts: [exts.pass-update exts.pass-otp]);
      enable = true;
      settings.PASSWORD_STORE_DIR = "${config.home.homeDirectory}/git/password-store";
    };
    git = {
      aliases = {
        sync = "!git pull -r && git push";
        cpr = "!f() { git fetch origin refs/pull/$1/head && git checkout FETCH_HEAD; }; f";
      };

      extraConfig = {
        merge.conflictStyle = "diff3";
        pull.ff = "only";
        core.editor = "hx";
        init.defaultBranch = "main";
      };
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
      userEmail = "mail@maralorn.de";
      userName = "maralorn";
    };
    htop = {
      enable = true;
      settings = {
        hide_threads = true;
        hide_userland_threads = true;
        highlight_base_name = true;
        shadow_other_users = true;
        show_program_path = false;
        tree_view = true;
        sort_key = "USER";
      };
    };
    ssh = {
      controlMaster = "auto";
      controlPersist = "120";
      enable = true;
      matchBlocks = {
        git-auto.identityFile = "~/.ssh/id_auto_ed25519";
        "door.w17.io".identityFile = "~/.ssh/door_rsa";
      };
    };
  };

  home = {
    packages =
      builtins.attrValues {
        inherit
          (pkgs)
          go
          gdb
          mpc_cli
          ncmpcpp
          shfmt
          astyle
          nodejs
          tasksh
          magic-wormhole
          alejandra
          nix-top
          ghcid
          matrix-commander
          upterm
          lazygit
          gh
          ledger
          aqbanking
          ;
        inherit (pkgs.haskellPackages) hledger hledger-ui hledger-web;
        pass-fzf = pkgs.writeShellScriptBin "pass-fzf" (builtins.readFile ./pass-fzf.sh);
        mytmux = pkgs.writeShellScriptBin "mytmux" ''
          session=$(${pkgs.tmux}/bin/tmux ls | grep -v attached | head -1 | cut -f1 -d:)
          if [[ -n $session ]]; then
             exec ${pkgs.tmux}/bin/tmux attach -t $session;
          else
             exec ${pkgs.tmux}/bin/tmux;
          fi
        '';
      }
      ++ [
        (
          pkgs.writeShellScriptBin "unlock-ssh" ''
            SSH_ASKPASS="print-ssh-pw" DISPLAY="a" ssh-add < /dev/null
          ''
        )
        (
          pkgs.writeShellScriptBin "print-ssh-pw"
          "pass show eu/m-0/${config.m-0.hostName}.m-0.eu/ssh-key"
        )
        (
          pkgs.writeShellScriptBin "dingdingding" (builtins.readFile ./signal.sh)
        )
      ];
    sessionVariables = {
      PATH = "$HOME/.nix-profile/bin:$PATH";
      BROWSER = "firefox";
      SUDO_ASKPASS = toString (
        pkgs.writeShellScript "print-sudo-pw"
        "pass show eu/m-0/${config.m-0.hostName}.m-0.eu/${config.home.username}"
      );
    };
  };

  systemd.user = {
    startServices = true;
    inherit (config.home) sessionVariables;
  };

  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 31536000; # 1year
      maxCacheTtl = 31536000; # 1year
    };
  };

  xdg = {
    configFile."mimeapps.list".force = true;
    enable = true;
    mime.enable = true;
    mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = ["org.gnome.Evince.desktop"];
        "x-scheme-handler/http" = ["firefox.desktop"];
        "x-scheme-handler/https" = ["firefox.desktop"];
        "x-scheme-handler/chrome" = ["firefox.desktop"];
        "text/html" = ["firefox.desktop"];
        "application/x-extension-htm" = ["firefox.desktop"];
        "application/x-extension-html" = ["firefox.desktop"];
        "application/x-extension-shtml" = ["firefox.desktop"];
        "application/xhtml+xml" = ["firefox.desktop"];
        "application/x-extension-xhtml" = ["firefox.desktop"];
        "application/x-extension-xht" = ["firefox.desktop"];
      };
    };
    userDirs = {
      enable = true;
      createDirectories = false;
      desktop = "$HOME";
      download = "$HOME";
      documents = "$HOME/media/documents/aktuell/";
      music = "$HOME/media/audio";
      pictures = "$HOME/media/images";
      videos = "$HOME/media/video";
    };
  };
}
