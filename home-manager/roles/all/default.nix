{
  pkgs,
  config,
  lib,
  ...
}:
{
  news.display = "silent";

  home = {
    stateVersion = "22.05";
    enableNixpkgsReleaseCheck = true;
  };

  programs = {
    nix-index.enable = true;
    home-manager.enable = true;
    eza = {
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
    packages = builtins.attrValues {
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
      unlock-keys = pkgs.writeShellScriptBin "unlock-keys" ''
        ${lib.getBin pkgs.dbus}/bin/dbus-update-activation-environment --systemd SSH_AUTH_SOCK
        if ! rbw unlocked; then killall rbw-agent; fi
        rbw unlock
        ssh-add <(rbw get "SSH code")
        ssh-add <(rbw get "SSH signing")
        ssh-add <(rbw get "SSH login")
      '';
    };
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
