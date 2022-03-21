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
      export FLAGS="--builders @/etc/nix/machines -o /var/cache/gc-links/$JOB"
      if [[ -e "flake.nix" ]]; then
        echo "Flake detected."
        echo "Running 'flake check'"
        ${pkgs.nixFlakes}/bin/flix flake check
        echo "Building 'defaultPackage' from 'flake.nix'"
        ${pkgs.nixFlakes}/bin/flix build $FLAGS
      else
        echo "Building default.nix"
        nix-build $FLAGS
      fi
    '';
in {
  services.laminar.cfgFiles.jobs = {
    "logfeed.run" = mkJob "logfeed";
    "blog.run" = mkJob "blog";
    "haskell-taskwarrior.run" = mkJob "haskell-taskwarrior";
    "nix-output-monitor.run" = mkJob "nix-output-monitor";
    "hochzeitsseite.run" = mkJob "hochzeitsseite";
  };
}
