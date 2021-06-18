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
    if [[ -e "flake.nix" ]]; then
      echo "Flake detected. Using flake.nix"
      drv=$(${pkgs.nixFlakes}/bin/flaky-nix eval --raw ".#defaultPackage.x86_64-linux".drvPath")
    else
      nix-instantiate --add-root ./drv --indirect $FLAGS
      drv=$(readlink -f ./drv)
    fi
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
