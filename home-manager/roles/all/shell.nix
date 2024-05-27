{
  programs = {
    zsh = {
      enable = true;
      history = {
        path = "$HOME/.persist/.zsh_history";
        extended = true;
        expireDuplicatesFirst = true;
      };
      initExtra = ''
        function set_terminal_title() {
          echo -en "\e]2;$@\a"
        }

        autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
        add-zsh-hook chpwd chpwd_recent_dirs
        zstyle ':chpwd:*' recent-dirs-max 100

        if [[ $PWD == "$HOME" ]] {
        	cd "$(head -n1 ~/.chpwd-recent-dirs | sed s/\\$// | sed s/\'//g)"
        }
        fzf-history-search() {
         LBUFFER=$( history | sed -E 's/ *[0-9]*\*? *//' | fzf +s --tac |  sed -E 's/\\/\\\\/g')
         zle redisplay
        }
        zle -N fzf-history-search
        bindkey '^R' fzf-history-search

        fzf-directory-history-search() {
         LBUFFER=$( sed "s/\\$//;s/'//g" ~/.chpwd-recent-dirs | fzf +s |  sed -E 's/\\/\\\\/g;s/^/cd /')
         zle redisplay
        }
        zle -N fzf-directory-history-search
        bindkey '^T' fzf-directory-history-search

        fzf-directory-search() {
         LBUFFER=$( fd -t d | fzf |  sed -E 's/\\/\\\\/g;s/^/cd /')
         zle redisplay
        }
        zle -N fzf-directory-search
        bindkey '^S' fzf-directory-search

        fzf-helix-search() {
         LBUFFER=$( fd -t f | fzf |  sed -E 's/\\/\\\\/g;s/^/hx /')
         zle redisplay
        }
        zle -N fzf-helix-search
        bindkey '^H' fzf-helix-search

        fzf-git-checkout() {
         LBUFFER=$( git branch --list | fzf |  sed -E 's/ \* //;s/^/git checkout /')
         zle redisplay
        }
        zle -N fzf-git-checkout
        bindkey '^G' fzf-git-checkout
      '';
    };
    direnv = {
      enable = true;
      config.global.warn_timeout = "1h";
    };
  };
}
