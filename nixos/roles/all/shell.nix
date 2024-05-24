{ pkgs, ... }:
{
  users.defaultUserShell = pkgs.zsh;

  programs = {
    starship = {
      enable = true;
      settings = {
        add_newline = false;
        right_format = "$nix_shell$cmd_duration$haskell$time";
        time.disabled = false;
        line_break.disabled = true;
        nix_shell.format = "[$symbol]($style)";
        character = {
          success_symbol = "[>](bold green)";
          error_symbol = "[>](bold red)";
        };
        git_branch.disabled = true;
        git_commit.disabled = true;
        git_status.disabled = true;
        time.format = "[$time]($style)";
        username.format = "[$user]($style) ";
        hostname.format = "[$ssh_symbol$hostname]($style) ";
        custom.jj = {
          command = ''
            jj log --ignore-working-copy --no-graph --color always -r @ -T '
              separate(" ",
                branches.map(|x| if(
                    x.name().substr(0, 10).starts_with(x.name()),
                    x.name().substr(0, 10),
                    x.name().substr(0, 9) ++ "…")
                  ).join(" "),
                tags.map(|x| if(
                    x.name().substr(0, 10).starts_with(x.name()),
                    x.name().substr(0, 10),
                    x.name().substr(0, 9) ++ "…")
                  ).join(" "),
                surround("\"","\"",
                  if(
                     description.first_line().substr(0, 24).starts_with(description.first_line()),
                     description.first_line().substr(0, 24),
                     description.first_line().substr(0, 23) ++ "…"
                  )
                ),
                if(conflict, "conflict"),
                if(divergent, "divergent"),
                if(hidden, "hidden"),
              )
            '
          '';
          style = "white";
          symbol = " ";
          detect_folders = [ ".jj" ];
        };
        custom.jjstate = {
          command = ''
            jj -r@ -l1 --no-graph -T "" --stat | tail -n1 | sd "(\d+) files? changed, (\d+) insertions?\(\+\), (\d+) deletions?\(-\)" " \''${1}󱇨 \''${2}+ \''${3}-" | sd " 0." ""
          '';
          style = "blue";
          detect_folders = [ ".jj" ];
        };
      };
    };
    zsh = {
      enable = true;
      autosuggestions.enable = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
      shellAliases = {
        nom-build-remote = "nom build --builders @$(builders-configurator $(hostname) --force)";
        nix-build-remote = "nix build --builders @$(builders-configurator $(hostname) --force)";
        nb = "nom build";
        nbr = "nom-build-remote";
        sj = "journalctl -efu";
        uj = "journalctl --user -efu";
        o = "xdg-open";
        s = "sudo systemctl";
        g = "set_terminal_title lazygit $(pwd); lazygit";
        j = "set_terminal_title lazyjj $(pwd); lazyjj";
        jpush = ''f() { if [[ "$2" == "" ]]; then jj branch set $1; jj git push -b $1; else jj branch set $1 -r "$2"; jj git push -b $1; fi}; f'';
        u = "systemctl --user";
        m = "man";
        h = ''f() { if [[ "$1" == "" ]]; then set_terminal_title hx $(pwd); hx .; else set_terminal_title hx $(pwd) $@; hx "$@"; fi}; f'';
        vim = "echo use hx";
        r = "NIX_AUTO_RUN=1";
        git-nosign = "git commit.gpgsign=false";
      };
      histSize = 10000000;
      setOptions = [
        "HIST_FIND_NO_DUPS"
        "INC_APPEND_HISTORY"
      ];
    };
  };
}
