{ pkgs, ... }:
{
  home.packages = [
    pkgs.git-absorb
    pkgs.glab
    pkgs.lazyjj
  ];
  programs = {
    jujutsu = {
      enable = true;
      settings = {
        user = {
          name = "maralorn";
          email = "mail@maralorn.de";
        };
        ui.default-command = "log";
        revsets.log = "@ | remotes() | ancestors(remotes().. ~ (::tags() | ::remote_branches() ~ ::branches()),2)";
        revset-aliases = {
          "remotes()" = "remote_branches(exact:main,exact:origin) | remote_branches(exact:master,exact:origin) | remote_branches(exact:develop,exact:origin) | remote_branches(exact:haskell-updates,exact:origin)";
          "immutable_heads()" = "remotes() | (remotes().. & ::~mine()) | tags()";
          short = "@ | remotes() | ancestors(heads(trunks()..) ~ (remote_branches() ~ branches()),2)";
        };
        template-aliases = {
          "format_short_change_id(id)" = "id.shortest()";
          "quote(x)" = ''surround("\"","\"",x)'';
          "branchinfo(x)" = ''
            separate(" ",
              x.branches().map(|y| trunc(y.name())).join(" "),
              x.tags().map(|y| trunc(y.name())).join(" "),
            )
          '';
          "trunc(x)" = ''
            if(
               x.substr(0, 24).starts_with(x),
               x.substr(0, 24),
               x.substr(0, 23) ++ "…"
            )
          '';
        };
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
