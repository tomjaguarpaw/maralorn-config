{pkgs, ...}: {
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    history = {
      path = "$HOME/.persist/.zsh_history";
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
      plugins = ["colored-man-pages"];
    };
    plugins = [
      # {
      #   name = "auto-notify";
      #   src = pkgs.fetchFromGitHub {
      #     owner = "MichaelAquilina";
      #     repo = "zsh-auto-notify";
      #     rev = "0.8.0";
      #     sha256 = "02x7q0ncbj1bn031ha7k3n2q2vrbv1wbvpx9w2qxv9jagqnjm3bd";
      #   };
      # }
      {
        name = "you-should-use";
        src = "${pkgs.zsh-you-should-use}/share/zsh/plugins/you-should-use";
      }
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = "${pkgs.zsh-nix-shell}/share/zsh-nix-shell";
      }
    ];
  };
}
