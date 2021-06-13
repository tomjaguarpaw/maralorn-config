{ pkgs, lib, config, ... }:
let
  path = [ pkgs.git pkgs.nix pkgs.gnutar pkgs.gzip pkgs.openssh pkgs.laminar ];
  mkJob = name: pkgs.writeShellScript "${name}.run" ''
    set -e
    export PATH=${lib.makeBinPath path}:$PATH
    git clone git@localhost:${name} .
    git show -q --oneline
    echo "Evaluating nix-expression."
    export FLAGS='--builders @/etc/nix/machines --max-jobs 0'
    nix-instantiate --add-root ./drv --indirect $FLAGS
    drv=$(readlink -f ./drv)
    echo "Evaluation done."
    nix-jobs realise $drv
    laminarc set "RESULTDRV=$drv"
  '';
in
{
  services.laminar.cfgFiles.jobs = {
    "logfeed.run" = mkJob "logfeed";
    "blog.run" = mkJob "blog";
  };
}
