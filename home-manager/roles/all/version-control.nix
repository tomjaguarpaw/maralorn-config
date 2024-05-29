{ pkgs, ... }:
{
  home.packages = builtins.attrValues { inherit (pkgs) glab lazyjj difftastic; };
  programs = {
    jujutsu = {
      enable = true;
      settings = {
        user = {
          name = "maralorn";
          email = "mail@maralorn.de";
        };
        ui = {
          default-command = "log";
          diff.tool = [
            "difft"
            "--color=always"
            "$left"
            "$right"
          ];
        };
        revsets.log = "@ | trunks() | ancestors(trunks().. ~ (::tags() | ::remote_branches() ~ ::branches()),2)";
        revset-aliases = {
          "trunks()" = "remote_branches(exact:main,exact:origin) | remote_branches(exact:master,exact:origin) | remote_branches(exact:develop,exact:origin) | remote_branches(exact:converts,exact:origin) | remote_branches(exact:haskell-updates,exact:origin)";
          "immutable_heads()" = "trunks() | tags()";
          short = "@ | remotes() | ancestors(heads(trunks()..) ~ (remote_branches() ~ branches()),2)";
        };
        template-aliases."format_short_change_id(id)" = "id.shortest()";
      };
    };
    git = {
      aliases = {
        sync = "!git pull -r && git push";
        cpr = "!f() { git fetch origin refs/pull/$1/head && git checkout FETCH_HEAD; }; f";
      };

      extraConfig = {
        merge.conflictStyle = "diff3";
        pull.ff = "only";
        core.editor = "hx";
        init.defaultBranch = "main";
        push.autoSetupRemote = true;
      };
      enable = true;
      ignores = [
        ".direnv"
        ".syncthing*.tmp"
        ".helix"
        ".zsh_config"
        "*.swp"
        "*.autosave~"
        "*.aux"
        "*.bbl"
        "*.fls"
        "*.idx"
        "*.ilg"
        "*.ind"
        "*.log"
        "*.out"
        "*.toc"
        "*.bcf"
        "*.blg"
        "*.fdb*"
        "*.thm"
        "*.run.xml"
        "*.slnc"
        "*.glade~"
        "__pycache__"
        ".hledger-web_client_session_key.aes"
        ".nix-gc-roots"
        "result"
      ];
      userEmail = "mail@maralorn.de";
      userName = "maralorn";
    };
  };
}
