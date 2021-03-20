{ pkgs, lib, config, ... }:
let
  path = [ pkgs.git pkgs.nix pkgs.gnutar pkgs.gzip pkgs.openssh pkgs.laminar ];
in {
  services.laminar.cfgFiles.jobs = {
    "logfeed.run" = pkgs.writeShellScript "logfeed.run" ''
      set -e
      export PATH=${lib.makeBinPath path}:$PATH
      git clone git@localhost:logfeed .
      git show -q
      echo "Evaluating nix-expression."
      export FLAGS='--builders @/etc/nix/machines --max-jobs 0'
      drv=$(readlink -f $(nix-instantiate default.nix -A pkg --add-root ./drv --indirect $FLAGS))
      echo "Evaluation done."
      nix-jobs realise $drv
      laminarc set "RESULTDRV=$drv"
    '';
  };
}
