{ pkgs, config, ... }:
{
imports = [
  ../../home-manager
];

systemd.user.systemctlPath = "/usr/bin/systemctl";

programs.home-manager.enable = true;

m-0 = {
  hostName = "fb04217";
  #sleep-nag.enable = true;
  #latex.enable = true;
  #graphical.enable = true;
  #rustdev.enable = true;
  #taskwarrior = {
  #  enable = true;
  #  git_active = true;
  #};
  #update_tasks.enable = true;
  #eventd.enable = true;
  #pythia.enable = true;
};

home = {
  language.base = "C.UTF-8";
  sessionVariables = {
    NIX_PATH = "nixpkgs=$HOME/git/nixos/nixpkgs:home-manager=$HOME/git/nixos/home-manager:$HOME/.nix-defexpr/channels";
  };
  forceCopies.paths = [ "bin/proot" "bin/with-nix" "bin/run-in-nix" ".bashrc" ".zshrc" ];
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
    home-manager switch
    nix-collect-garbage --delete-older-than 5
    nix-optimise
  '')
  ] ++ ((import ../../common/essentials.nix).extra pkgs);
};

}
