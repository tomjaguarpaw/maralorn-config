{ pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    history = {
      path = "/disk/persist$HOME/.zsh_history";
      extended = true;
      ignoreDups = true;
      share = true;
      save = 10000000;
      size = 10000000;
    };
    # Workaround for the limitation that gnome unsets some variables for following sessions (like EDITOR) but __HM_SESS_VARS_SOURCED apparently not.
    initExtraFirst = ''
      export __HM_SESS_VARS_SOURCED=""
    '';
    initExtra = ''
      ${builtins.readFile ./zshrc}
      source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
      source ${./p10k.zsh}
      set inc_append_history
    '';
    oh-my-zsh = {
      enable = true;
      plugins = [ "colored-man-pages" ];
    };
    plugins = [ {
      name = "zsh-nix-shell";
      file = "nix-shell.plugin.zsh";
      src = "${pkgs.zsh-nix-shell}/share/zsh-nix-shell";
    } ];
  };
}
