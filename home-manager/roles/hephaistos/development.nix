{ pkgs, ... }:
let
  language-config = [
    {
      name = "haskell";
      formatter = {
        command = "ormolu";
        args = [
          "--stdin-input-file"
          "/home/maralorn/git/connect-app/frontend/src/Frontend.hs"
        ];
      };
    }
    {
      name = "haskell";
      auto-format = true;
    }
  ];
in
{
  home.packages = [
    (pkgs.writeShellScriptBin "unpack" ''
      ob --no-handoff thunk unpack "$1"
      cd "$1"
      git config user.name "Malte Ott"
      git config user.email "mott@heilmannsoftware.de"
    '')
    (pkgs.writeShellScriptBin "pack" ''
      ob --no-handoff thunk pack "$1"
    '')
    pkgs.remmina
    pkgs.mdbtools
    (pkgs.writeShellScriptBin "for-model" ''
      set -e
      jj restore cabal.project --from develop
      cp ${./_for-model-shell.nix} shell.nix
      mkdir -p .helix
      ln -sf ${
        (pkgs.formats.toml { }).generate "languages.toml" { language = language-config; }
      } .helix/languages.toml
      echo "use nix" > .envrc
      direnv allow
    '')
    (pkgs.writeShellScriptBin "with-model" ''
      set -e
      jj restore cabal.project --from develop
      sd ", model.*\$" "" cabal.project
      cp ${./_with-model-shell.nix} shell.nix
      mkdir -p .helix
      ln -sf ${
        (pkgs.formats.toml { }).generate "languages.toml" { language = language-config; }
      } .helix/languages.toml
      echo "use nix" > .envrc
      direnv allow
    '')
  ];
}
