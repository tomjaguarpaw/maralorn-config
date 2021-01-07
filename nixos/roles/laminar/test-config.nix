{ pkgs, lib, config, ... }:
let
  path = [ pkgs.git pkgs.nix pkgs.gnutar pkgs.gzip pkgs.openssh pkgs.laminar ];
  common = ''
    set -ex
    export PATH=${lib.makeBinPath path}:$PATH
    export NIX_PATH="/etc/nix-path:nixos-config=/etc/nixos/configuration.nix"
  '';
  checkout = ''
    git clone git@hera.m-0.eu:nixos-config config --config advice.detachedHead=false
    cd config
    REPODIR=`pwd`
    git checkout origin/$BRANCH
    cd /var/cache/gc-links
  '';
  update-config =
    "${pkgs.systemd}/bin/systemctl start --no-block update-config";
  systems = [ "apollo" "hera" ];
  homes = lib.attrNames (import ../../../home-manager/machines.nix);
  mkHomeJob = (host: {
    name = "home-config-${host}.run";
    value = pkgs.writeShellScript "test-${host}-home-config.run" ''
      ${common}
      ${checkout}
      ${pkgs.test-home-config}/bin/test-home-config $REPODIR ${host} --builders "@/etc/nix/machines" --max-jobs 1
      git -C $REPODIR submodule update --init
      ${pkgs.test-home-config}/bin/test-home-config $REPODIR ${host}
    '';
  });
  mkSystemJob = (host: {
    name = "system-config-${host}.run";
    value = pkgs.writeShellScript "test-${host}-system-config.run" ''
      ${common}
      ${checkout}
      ${pkgs.test-system-config}/bin/test-system-config $REPODIR ${host} --builders "@/etc/nix/machines" --max-jobs 1
      git -C $REPODIR submodule update --init
      ${pkgs.test-system-config}/bin/test-system-config $REPODIR ${host}
    '';
  });
in {
  services.laminar.cfgFiles.jobs = {
    "test-config.run" = pkgs.writeShellScript "test-config.run" ''
      ${common}
      ${pkgs.test-config}/bin/test-config
    '';
    "test-config.after" = pkgs.writeShellScript "test-config.after" ''
      ${common}
      if [[ "$RESULT" == "success" ]]; then
      /run/wrappers/bin/sudo ${update-config}
      fi
    '';
    "bump-and-test-config.run" =
      pkgs.writeShellScript "bump-and-test-config.run" ''
        ${common}
        ${pkgs.test-config}/bin/test-config bump
      '';
  } // lib.listToAttrs (map mkHomeJob homes)
    // lib.listToAttrs (map mkSystemJob homes);
  security.sudo.extraRules = [{
    commands = [{
      command = "${update-config}";
      options = [ "NOPASSWD" ];
    }];
    users = [ "laminar" ];
  }];
}
