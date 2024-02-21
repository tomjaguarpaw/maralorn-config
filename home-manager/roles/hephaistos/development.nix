{ pkgs, ... }:
{
  home.packages = [
    pkgs.remmina
    pkgs.mdbtools
    (pkgs.writeShellScriptBin "for-model" ''
      git checkout cabal.project
      cp ${./_for-model-shell.nix} shell.nix
      echo "use nix" > .envrc
      direnv allow
    '')
    (pkgs.writeShellScriptBin "with-model" ''
      sd ", model/\*.cabal" "" cabal.project
      cp ${./_with-model-shell.nix} shell.nix
      echo "use nix" > .envrc
      direnv allow
    '')
  ];
}
