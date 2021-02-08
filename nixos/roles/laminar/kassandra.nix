{ pkgs, lib, config, ... }:
let
  path = [ pkgs.git pkgs.nix pkgs.gnutar pkgs.gzip pkgs.openssh pkgs.laminar ];
  setup = ''
    set -e
    export PATH=${lib.makeBinPath path}:$PATH
  '';
  repo = "/var/www/fdroid";
  appName = "de.maralorn.kassandra";
  deploy = "${pkgs.writeShellScript "deploy" ''
    systemctl restart kassandra
    cd /var/cache/gc-links/kassandra-android
    FILENAME=$(${pkgs.fd}/bin/fd .apk)
    rm -f ${repo}/repo/*.apk
    cp $FILENAME ${repo}/unsigned
    cd ${repo}
    export PATH=/run/current-system/sw/bin:$PATH
    export ANDROID_HOME=${pkgs.androidsdk_9_0}/libexec/android-sdk
    fdroid publish
    fdroid update
  ''}";
  target = name: ''
    ${setup}
    export HOME=$PWD
    git clone git@localhost:kassandra2 .
    git show -q
    echo "Evaluating nix-expression."
    export FLAGS='--builders @/etc/nix/machines --max-jobs 1'
    drv=$(readlink -f $(nix-instantiate release.nix -A ${name} --add-root ./drv --indirect $FLAGS))
    echo "Evaluation done."
    nix-jobs realise $drv
    laminarc set "RESULTDRV=$drv"
  '';
in {
  security.sudo.extraRules = [{
    commands = [{
      command = deploy;
      options = [ "NOPASSWD" ];
    }];
    users = [ "laminar" ];
  }];
  services.laminar.cfgFiles.jobs = {
    "kassandra.run" = pkgs.writeShellScript "kassandra" ''
      ${setup}
      echo Launching and waiting for jobs lib, app, android and server
      export LAMINAR_REASON="Started kassandra build $JOB:#$RUN"
      laminarc run kassandra-lib kassandra-android kassandra-app kassandra-server
      /run/wrappers/bin/sudo ${deploy}
    '';
    "kassandra-lib.run" = pkgs.writeShellScript "kassandra-lib" (target "lib");
    "kassandra-app.run" = pkgs.writeShellScript "kassandra-app" (target "app");
    "kassandra-android.run" =
      pkgs.writeShellScript "kassandra-android" (target "android");
    "kassandra-server.run" =
      pkgs.writeShellScript "kassandra-server" (target "server");
  };
}
