{ pkgs, lib, ... }:
let
  path = [
    pkgs.git
    pkgs.nix
    pkgs.gnutar
    pkgs.gzip
    pkgs.openssh
    pkgs.laminar
  ];
  mkJob =
    name:
    pkgs.writeShellScript "${name}.run" ''
      set -e
      export PATH=${lib.makeBinPath path}:$PATH
      git clone git@localhost:${name} .
      git show -q --oneline
      export REMOTE_FLAG="--builders @$(${pkgs.builders-configurator}/bin/builders-configurator)"
      if [[ -e "flake.nix" ]]; then
        echo "Flake detected."
        echo "Running 'flake check'"
        ${lib.getExe pkgs.nix} flake check $REMOTE_FLAG
        echo "Running 'nix build'"
        ${lib.getExe pkgs.nix} build $REMOTE_FLAG
        ${lib.getExe pkgs.archive-nix-path} ./result
      else
        echo "Building default.nix"
        nix-build $REMOTE_FLAG
        ${lib.getExe pkgs.archive-nix-path} ./result
      fi
    ''
  ;
in
{
  services.laminar.cfgFiles.jobs = {
    "blog.run" = mkJob "blog";
    "haskell-taskwarrior.run" = mkJob "haskell-taskwarrior";
    "nix-output-monitor.run" = mkJob "nix-output-monitor";
    "hochzeitsseite.run" = mkJob "hochzeitsseite";
  };
}
