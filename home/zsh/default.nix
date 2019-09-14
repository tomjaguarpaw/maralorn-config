{ pkgs, ... }: {

  programs = {
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      history = {
        save = 100000;
        size = 100000;
      };
      initExtra = builtins.readFile ./zshrc;
      oh-my-zsh = let my-pkgs = import ../../pkgs;
      in {
        enable = true;
        plugins = [ "colored-man-pages" ];
        theme = "powerlevel10k";
        custom = "${pkgs.runCommand "oh-my-zsh-custom" { } ''
          mkdir -p $out/themes
          ln -s ${my-pkgs.zsh-powerlevel10k}/powerlevel10k/powerlevel10k.zsh-theme $out/themes/.
        ''}";
      };
    };
  };

}
