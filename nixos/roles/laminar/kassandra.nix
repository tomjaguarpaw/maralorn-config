{ pkgs, lib, config, ... }:
let
  path = [ pkgs.git pkgs.nix pkgs.gnutar pkgs.gzip pkgs.openssh pkgs.laminar ];
  setup = ''
    export PATH=${lib.makeBinPath path}:$PATH
  '';
  target = name: ''
    set -e
    ${setup}
    export HOME=$PWD
    git clone git@localhost:kassandra2 kassandra
    cd kassandra
    git show -q
    echo "Evaluating nix-expression."
    export FLAGS='--builders @/etc/nix/machines --max-jobs 1'
    drv=$(readlink -f $(nix-instantiate release.nix -A ${name} --add-root ./drv --indirect $FLAGS))
    echo "Evaluation done."
    nix-jobs realise $drv
    laminarc set "RESULTDRV=$drv"
  '';
in {
  services.laminar.cfgFiles.jobs = {
    "kassandra.run" = pkgs.writeShellScript "kassandra" ''
      ${setup}
      echo Launching and waiting for jobs lib, app, android and server
      laminarc run kassandra-lib kassandra-android kassandra-app kassandra-server
    '';
    "kassandra-lib.run" = pkgs.writeShellScript "kassandra-lib" (target "lib");
    "kassandra-app.run" = pkgs.writeShellScript "kassandra-app" (target "app");
    "kassandra-android.run" =
      pkgs.writeShellScript "kassandra-android" (target "android");
    "kassandra-server.run" =
      pkgs.writeShellScript "kassandra-server" (target "server");
  };
}
