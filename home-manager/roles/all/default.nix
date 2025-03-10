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
    eza.enable = true;
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
        upterm
        ledger
        aqbanking
        ;
      mytmux = pkgs.writeShellScriptBin "mytmux" ''
        session=$(${pkgs.tmux}/bin/tmux ls | grep -v attached | head -1 | cut -f1 -d:)
        if [[ -n $session ]]; then
           exec ${lib.getExe pkgs.tmux} attach -t $session;
        else
           exec ${lib.getExe pkgs.tmux};
        fi
      '';
    };
    sessionVariables.SSH_AUTH_SOCK = "/run/user/1000/ssh-agent";
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

}
