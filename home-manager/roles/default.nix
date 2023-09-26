{
  pkgs,
  config,
  lib,
  ...
}:
{
  imports = [
    ./zsh
    ./home-options.nix
    ../../common
    ./unlock.nix
    ./mpclient.nix
    ./nvd.nix
  ];

  news.display = "silent";

  home = {
    stateVersion = "22.05";
    enableNixpkgsReleaseCheck = true;
  };

  programs = {
    nix-index.enable = true;
    home-manager.enable = true;
    exa = {
      enable = true;
      enableAliases = true;
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
        ".direnv"
        ".syncthing*.tmp"
        ".helix"
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
        "result"
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
  };

  home = {
    packages =
      builtins.attrValues {
        inherit (pkgs)
          mpc_cli
          ncmpcpp
          magic-wormhole
          matrix-commander
          upterm
          tasksh
          ledger
          aqbanking
        ;
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
        (pkgs.writeShellScriptBin "unlock-keys" ''
          ssh-add ~/.ssh/id_ed25519_sk-solo-1
          ${
            lib.getBin pkgs.dbus
          }/bin/dbus-update-activation-environment --systemd SSH_AUTH_SOCK
          if ! rbw unlocked; then killall rbw-agent; fi
          rbw unlock
        '')
        (pkgs.writeShellScriptBin "dingdingding" (builtins.readFile ./signal.sh))
      ];
    sessionVariables = {
      PATH = "$HOME/.nix-profile/bin:$PATH";
      BROWSER = "firefox";
      SSH_AUTH_SOCK = "/run/user/1000/ssh-agent";
    };
  };

  systemd.user = {
    startServices = true;
    inherit (config.home) sessionVariables;
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 31536000; # 1year
    maxCacheTtl = 31536000; # 1year
  };

  xdg = {
    configFile."mimeapps.list".force = true;
    enable = true;
    mime.enable = true;
    mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = [ "org.gnome.Evince.desktop" ];
        "x-scheme-handler/http" = [ "firefox.desktop" ];
        "x-scheme-handler/https" = [ "firefox.desktop" ];
        "x-scheme-handler/chrome" = [ "firefox.desktop" ];
        "text/html" = [ "firefox.desktop" ];
        "application/x-extension-htm" = [ "firefox.desktop" ];
        "application/x-extension-html" = [ "firefox.desktop" ];
        "application/x-extension-shtml" = [ "firefox.desktop" ];
        "application/xhtml+xml" = [ "firefox.desktop" ];
        "application/x-extension-xhtml" = [ "firefox.desktop" ];
        "application/x-extension-xht" = [ "firefox.desktop" ];
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
