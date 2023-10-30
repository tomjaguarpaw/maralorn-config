{
  programs.git = {
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
}
