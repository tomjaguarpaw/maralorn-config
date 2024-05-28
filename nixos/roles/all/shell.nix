{ pkgs, ... }:
{
  users.defaultUserShell = pkgs.zsh;

  programs = {
    direnv = {
      enable = true;
      silent = true;
    };
    starship = {
      enable = true;
      settings = {
        add_newline = false;
        right_format = "$cmd_duration$direnv$nix_shell$haskell$time";
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
        direnv.disabled = false;
        time.format = "[$time]($style)";
        username.format = "[$user]($style) ";
        hostname.format = "[$ssh_symbol$hostname]($style) ";
        custom =
          let
            templates = ''
              [template-aliases]
              "quote(x)" = """
                surround("\\"","\\"",x)
              """
              "branchinfo(x)" = """
                  separate(" ",
                    x.branches().map(|y| trunc(y.name())).join(" "),
                    x.tags().map(|y| trunc(y.name())).join(" "),
                  )
              """
              "trunc(x)" = """
                if(
                   x.substr(0, 24).starts_with(x),
                   x.substr(0, 24),
                   x.substr(0, 23) ++ "…"
                )
              """
              "parentinfo(x)" = """
                  x.parents().map(|p| coalesce(
                    branchinfo(p),
                    quote(trunc(p.description().first_line()))
                  )).join("󰜘 ")
              """
            '';
          in
          {
            jjtrouble = {
              when = "jj root";
              require_repo = true;
              command = ''
                jj log -r@ -l1 --ignore-working-copy --no-graph --color always  -T '
                  separate(" ",
                    if(conflict, " "),
                    if(divergent, " "),
                    if(hidden, " "),
                  )
                '
              '';
              style = "red";
            };
            jjstate = {
              when = "jj root";
              require_repo = true;
              command = ''
                jj log -r@ -l1 --ignore-working-copy --no-graph -T "" --stat\
                  | tail -n1\
                  | sd "(\d+) files? changed, (\d+) insertions?\(\+\), (\d+) deletions?\(-\)" " \''${1}󱇨 \''${2}+ \''${3}-"\
                  | sd " 0." ""
              '';
              style = "blue";
            };
            jj = {
              when = "jj root";
              require_repo = true;
              command = ''
                jj log -r@ -l1 --config-toml='${templates}' --ignore-working-copy --no-graph --color always  -T '
                  coalesce(
                    surround("󰜝 ",
                      " " ++ coalesce(
                        branchinfo(self),
                        surround("󰜘 ","",parentinfo(self))
                      ),
                      quote(trunc(description.first_line()))
                    ),
                    surround("󰜝 ","", branchinfo(self)),
                    "󰜘 " ++ parentinfo(self)
                  )
                '
              '';
              style = "white";
            };
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
