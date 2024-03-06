{ pkgs, ... }:
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
    pkgs.remmina
    pkgs.mdbtools
    (pkgs.writeShellScriptBin "for-model" ''
      git checkout cabal.project
      cp ${./_for-model-shell.nix} shell.nix
      echo "use nix" > .envrc
      direnv allow
    '')
    (pkgs.writeShellScriptBin "with-model" ''
      sd ", model-?[a-z]*/\*.cabal" "" cabal.project
      cp ${./_with-model-shell.nix} shell.nix
      echo "use nix" > .envrc
      direnv allow
    '')
  ];
}
