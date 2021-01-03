{ pkgs, lib, config, ... }:
let
  path = [ pkgs.git pkgs.nix pkgs.gnutar pkgs.gzip pkgs.openssh pkgs.laminar ];
  setup = ''
    set -ex
    export PATH=${lib.makeBinPath path}:$PATH
  '';
  target = name: ''
    ${setup}
    export HOME=$PWD
    git clone git@localhost:kassandra2 kassandra
    nix-build --no-out-link kassandra/release.nix -A ${name}
  '';
in {
  services.laminar.jobs = {
    "kassandra.run" = pkgs.writeShellScript "kassandra" ''
      ${setup}
      laminarc run kassandra-lib &
      P1=$!
      laminarc run kassandra-android &
      P2=$!
      laminarc run kassandra-app &
      P3=$!
      laminarc run kassandra-server &
      P4=$!
      wait $P1 $P2 $P3 $P4
    '';
    "kassandra-lib.run" = pkgs.writeShellScript "kassandra-lib" (target "lib");
    "kassandra-app.run" = pkgs.writeShellScript "kassandra-app" (target "app");
    "kassandra-android.run" =
      pkgs.writeShellScript "kassandra-android" (target "android");
    "kassandra-server.run" =
      pkgs.writeShellScript "kassandra-server" (target "server");
  };
}
