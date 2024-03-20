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
      sd "https://gitlab.heilmannsoftware.net" "ssh://git@gitlab.heilmannsoftware.net" .git/config

    '')
    (pkgs.writeShellScriptBin "pack" ''
      sd "ssh://git@gitlab.heilmannsoftware.net" "https://gitlab.heilmannsoftware.net" "$1/.git/config"
      ob --no-handoff thunk pack "$1"
    '')
    pkgs.glab
    pkgs.remmina
    pkgs.mdbtools
    (pkgs.writeShellScriptBin "for-model" ''
      git checkout cabal.project
      cp ${./_for-model-shell.nix} shell.nix
      mkdir -p .helix
      ln -sf ${
        (pkgs.formats.toml { }).generate "languages.toml" { language = language-config; }
      } .helix/languages.toml
      echo "use nix" > .envrc
      git update-index --assume-unchanged cabal.project shell.nix
      direnv allow
    '')
    (pkgs.writeShellScriptBin "with-model" ''
      sd ", model.*\$" "" cabal.project
      cp ${./_with-model-shell.nix} shell.nix
      mkdir -p .helix
      ln -sf ${
        (pkgs.formats.toml { }).generate "languages.toml" { language = language-config; }
      } .helix/languages.toml
      echo "use nix" > .envrc
      git update-index --assume-unchanged cabal.project shell.nix
      direnv allow
    '')
  ];
}
