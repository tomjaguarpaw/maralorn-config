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
    packages = builtins.attrValues rec {
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
           exec ${lib.getBin pkgs.tmux} attach -t $session;
        else
           exec ${lib.getBin pkgs.tmux};
        fi
      '';
      ssh-rbw-pubkey = pkgs.writeShellScriptBin "ssh-rbw-pubkey" ''
        file=$(mktemp)
        ${lib.getBin ssh-rbw-privkey} "$1" > $file
        ssh-keygen -y -f $file
        rm $file
      '';
      ssh-rbw-privkey = pkgs.writeShellScriptBin "ssh-rbw-privkey" ''
        ${lib.getBin config.programs.rbw.package} get --folder ssh "sshkey: $1"
      '';
      ssh-rbw-gen = pkgs.writeShellScriptBin "ssh-rbw-gen" ''
        ssh-keygen -ted25519 -C "maralorn.$1@pantheon"
      '';
      ssh-rbw-add = pkgs.writeShellScriptBin "ssh-rbw-add" ''
        ssh-add <(${lib.getBin ssh-rbw-privkey} "$1")
      '';
      unlock-keys = pkgs.writeShellScriptBin "unlock-keys" ''
        ${lib.getBin pkgs.dbus}/bin/dbus-update-activation-environment --systemd SSH_AUTH_SOCK
        if ! rbw unlocked; then killall rbw-agent; fi
        rbw unlock

        ${lib.getBin ssh-rbw-add} code
        ${lib.getBin ssh-rbw-add} signing
        ${lib.getBin ssh-rbw-add} login
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
