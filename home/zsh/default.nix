{ pkgs, ... }: {

  programs = let my-pkgs = import ../../pkgs;
  in {
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      history = {
        save = 100000;
        size = 100000;
      };
      initExtra = builtins.readFile ./zshrc + ''
        GITSTATUS_DAEMON=${my-pkgs.gitstatus}/bin/gitstatusd source ${
          ./p10k.zsh
        }
      '';
      oh-my-zsh = {
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
