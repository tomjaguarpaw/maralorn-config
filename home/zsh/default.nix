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
      initExtra = ''
        # If running from tty1 start sway
        if [ "$(tty)" = "/dev/tty1" ]; then
           . ${my-pkgs.start-ssh-agent}/bin/start-ssh-agent
           exec ${pkgs.sway}/bin/sway >> ~/tmp/sway.log
        fi

        ${builtins.readFile ./zshrc}
        source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
        source ${./p10k.zsh}
      '';
      oh-my-zsh = {
        enable = true;
        plugins = [ "colored-man-pages" ];
      };
    };
  };

}
