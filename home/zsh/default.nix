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
           while true; do
             ${pkgs.dialog}/bin/dialog --menu "Select Mode" 20 80 5 research "" orga "" tinkering "" leisure "" --stderr 2> ~/tmp/mode
             update-home-mode
             echo Launching sway at $(date) >> ~/tmp/sway.log
             ${pkgs.sway}/bin/sway >> ~/tmp/sway.log
           done
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
