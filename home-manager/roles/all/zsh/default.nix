{ pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    enableCompletion = true;
    shellAliases = {
      nom-build-remote = "nom build --builders @$(builders-configurator $(hostname) --force)";
      nix-build-remote = "nix build --builders @$(builders-configurator $(hostname) --force)";
      nb = "nom build";
      nbr = "nom-build-remote";
      sj = "journalctl -efu";
      uj = "journalctl --user -efu";
      o = "xdg-open";
      s = "sudo systemctl";
      g = "set_terminal_title lazygit $(pwd); lazygit";
      u = "systemctl --user";
      m = "man";
      h = ''f() { if [[ "$1" == "" ]]; then set_terminal_title hx $(pwd); hx .; else set_terminal_title hx $(pwd) $@; hx "$@"; fi}; f'';
      vim = "echo use hx";
      r = "NIX_AUTO_RUN=1";
      git-nosign = "git commit.gpgsign=false";
    };
    history = {
      path = "$HOME/.persist/.zsh_history";
      extended = true;
      expireDuplicatesFirst = true;
      save = 10000000;
      size = 10000000;
    };
    # Workaround for the limitation that gnome unsets some variables for following sessions (like EDITOR) but __HM_SESS_VARS_SOURCED apparently not.
    initExtraFirst = ''
      export __HM_SESS_VARS_SOURCED=""
    '';
    initExtra = ''
      ${builtins.readFile ./zshrc}
      setopt HIST_FIND_NO_DUPS
      setopt INC_APPEND_HISTORY
      source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
      source ${./p10k.zsh}
      set inc_append_history
    '';
    oh-my-zsh = {
      enable = true;
      plugins = [ "colored-man-pages" ];
    };
    plugins = [
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = "${pkgs.zsh-nix-shell}/share/zsh-nix-shell";
      }
    ];
  };
}
