{
  pkgs,
  lib,
  config,
  ...
}: let
  path = [pkgs.git pkgs.nix pkgs.gnutar pkgs.gzip pkgs.openssh pkgs.laminar];
  mkJob = name:
    pkgs.writeShellScript "${name}.run" ''
      set -e
      export PATH=${lib.makeBinPath path}:$PATH
      git clone git@localhost:${name} .
      git show -q --oneline
      export REMOTE_FLAG="--builders @$(${pkgs.builders-configurator}/bin/builders-configurator)"
      export FLAGS="$REMOTE_FLAGS -o /var/cache/gc-links/$JOB"
      if [[ -e "flake.nix" ]]; then
        echo "Flake detected."
        echo "Running 'flake check'"
        ${pkgs.nix}/bin/nix flake check $REMOTE_FLAG
        echo "Running 'nix build'"
        ${pkgs.nix}/bin/nix build $FLAGS
      else
        echo "Building default.nix"
        nix-build $FLAGS
      fi
    '';
in {
  services.laminar.cfgFiles.jobs = {
    "blog.run" = mkJob "blog";
    "haskell-taskwarrior.run" = mkJob "haskell-taskwarrior";
    "nix-output-monitor.run" = mkJob "nix-output-monitor";
    "hochzeitsseite.run" = mkJob "hochzeitsseite";
  };
}
