{ pkgs, lib, config, ... }:
let
  path = [ pkgs.git pkgs.nix pkgs.gnutar pkgs.gzip pkgs.openssh pkgs.laminar ];
  setup = ''
    export PATH=${lib.makeBinPath path}:$PATH
  '';
  target = name: ''
    set -ex
    ${setup}
    export HOME=$PWD
    git clone git@localhost:kassandra2 kassandra
    nix-build --no-out-link kassandra/release.nix -A ${name}
  '';
in {
  services.laminar.jobs = {
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
