{ pkgs, ... }:
let
  jjstat = pkgs.writeShellScript "jjstat" ''
    jj git fetch --quiet
    echo
    echo === JJ DIFF ===
    echo
    jj diff --no-pager
    echo
    echo === JJ DIFF STATS ===
    echo
    jj diff --stat --no-pager
    echo
    echo === JJ LOG == $PWD ======================================================
    echo
    jj log --no-pager
  '';
in
{
  home.packages = builtins.attrValues {
    inherit (pkgs) glab lazyjj difftastic;
    jjwatch = pkgs.writeShellScriptBin "jjwatch" "(jj file list; find .jj -maxdepth 3) | entr -c ${jjstat}";
    jjpr = pkgs.writeShellScriptBin "jjpr" ''
      set -ex -o pipefail
      shopt -s extglob
      if [[ "$1" == "" ]]; then
        rev="@"
      else
        rev="$1"
      fi

      desc="$(jj log -T 'self.description()' -r "$rev" --no-graph | head -n1)"

      if [[ "$desc" == "" ]]; then
        jj desc -r "$rev"
        desc="$(jj log -T 'self.description()' -r "$rev" --no-graph | head -n1)"
      fi

      branch=$(jj log -T 'self.branches()' -r "$rev" --no-graph)

      if [[ "$branch" == "" ]]; then
        branch=$(echo "''${desc//+([^[:alnum:]])/-}" | tr '[:upper:]' '[:lower:]')
        jj branch set "$branch" -r "$rev"
      fi

      jj git push -r "$rev"

      if glab repo view > /dev/null; then
        git checkout "$branch"
        glab mr create --reviewer spaunovic --title "$desc" --fill --no-editor --yes --remove-source-branch
        jj new work
      else
        tea pr create --assignees marabot --head "$branch" --title "$desc"
      fi

      if [[ "$rev" == "@" ]]; then
        jj new
      fi
    '';
  };
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
        revsets.log = "@ | trunks() | ancestors(trunks()..active(),2)";
        revset-aliases = {
          "trunks()" = "remote_branches(exact:main,exact:origin) | remote_branches(exact:master,exact:origin) | remote_branches(exact:develop,exact:origin) | remote_branches(exact:converts,exact:origin) | remote_branches(exact:haskell-updates,exact:origin)";
          "immutable_heads()" = "trunks() | tags()";
          "active()" = "heads(immutable_heads()..) ~ (remote_branches() ~ branches())";
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
