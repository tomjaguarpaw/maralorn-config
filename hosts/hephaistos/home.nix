{ pkgs, config, ... }: {
  imports = [ ../../home ../../home/on-foreign-machine.nix ];

  systemd.user.systemctlPath = "/usr/bin/systemctl";

  m-0 = { hostName = "fb04217"; };

  home = {
    username = "brandy";
    homeDirectory = "/home/brandy";
    language = {
      base = "C.UTF-8";
      address = "C.UTF-8";
      monetary = "C.UTF-8";
      paper = "C.UTF-8";
      time = "C.UTF-8";
    };
    sessionVariables = {
      LANGUAGE = "en_US";
      LC_CTYPE = "C.UTF-8";
      LC_NUMERIC = "C.UTF-8";
      LC_COLLATE = "C.UTF-8";
      LC_MESSAGES = "C.UTF-8";
      LC_NAME = "C.UTF-8";
      LC_TELEPHONE = "C.UTF-8";
      LC_MEASUREMENT = "C.UTF-8";
      LC_IDENTIFICATION = "C.UTF-8";
    };
    forceCopies.paths =
      [ "bin/proot" "bin/with-nix" "bin/run-in-nix" ".bashrc" ".zshrc" ];
    file = {
      ".bashrc".text = ''
        [ -z "$PS1" ] && return
        unset __HM_SESS_VARS_SOURCED
        if [[ -z "$NIX_PATH" ]]
        then
            exec ~/bin/with-nix zsh
        else
            exec zsh
        fi
      '';
      "bin" = {
        source = ./bootstrap-bin;
        recursive = true;
      };
    };

    packages = [
      (pkgs.writeShellScriptBin "maintenance" ''
        set -e
        cd ~/git/nixos/nixpkgs
        git checkout nixos-local
        git pull --no-edit upstream nixos-19.03
        git pull --no-edit origin nixos-maralorn
        cd ~/git/nixos/home-manager
        git checkout home-manager-local
        git pull --no-edit upstream release-19.03
        git pull --no-edit origin home-manager-maralorn
        home-manager switch
        nix-collect-garbage --delete-older-than 5d
        nix-store --optimise
      '')
    ];
  };

}
