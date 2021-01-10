{ pkgs, lib, config, ... }:
let
  path = [ pkgs.git pkgs.nix pkgs.gnutar pkgs.gzip pkgs.openssh pkgs.laminar ];
  common = ''
    set -e
    export PATH=${lib.makeBinPath path}:$PATH
    export NIX_PATH="/etc/nix-path:nixos-config=/etc/nixos/configuration.nix"
  '';
  checkout = ''
    git clone git@hera.m-0.eu:nixos-config . --config advice.detachedHead=false
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
      export FLAGS='--builders @/etc/nix/machines --max-jobs 1'
      ${pkgs.test-home-config}/bin/test-home-config $REPODIR ${host}
      git -C $REPODIR submodule update --init
      export FLAGS=""
      ${pkgs.test-home-config}/bin/test-home-config $REPODIR ${host}
    '';
  });
  mkSystemJob = (host: {
    name = "system-config-${host}.run";
    value = pkgs.writeShellScript "test-${host}-system-config.run" ''
      ${common}
      ${checkout}
      export FLAGS='--builders @/etc/nix/machines --max-jobs 1'
      ${pkgs.test-system-config}/bin/test-system-config $REPODIR ${host}
      git -C $REPODIR submodule update --init
      export FLAGS=""
      ${pkgs.test-system-config}/bin/test-system-config $REPODIR ${host}
    '';
  });
  deployCommand = "${let user = "maralorn";
  in pkgs.writeShellScript "deploy-system-config" ''
    /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos pull --ff-only
    /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos submodule update --init
    /var/cache/gc-links/result-system-hera/bin/switch-to-configuration switch
    /run/wrappers/bin/sudo -u ${user} /var/cache/gc-links/result-home-manager-hera/default/activate
  ''}";
in {
  services.laminar.cfgFiles.jobs = {
    "test-config.run" = pkgs.writeHaskell "test-config" {
      libraries = builtins.attrValues pkgs.myHaskellScriptPackages;
      ghcEnv = {
        HOMES = lib.concatStringsSep " " homes;
        SYSTEMS = lib.concatStringsSep " " systems;
        DEPLOY = deployCommand;
        PATH = "${lib.makeBinPath [ pkgs.laminar pkgs.git ]}:$PATH";
      };
      ghcArgs = [ "-threaded" ];
    } (builtins.readFile ./test-config.hs);
    "bump-config.run" = pkgs.writeHaskell "bump-config" {
      libraries = builtins.attrValues pkgs.myHaskellScriptPackages;
      ghcEnv.PATH = "${lib.makeBinPath [ pkgs.git pkgs.niv pkgs.nix ]}:$PATH";
      ghcArgs = [ "-threaded" ];
    } (builtins.readFile ./bump-config.hs);
  } // lib.listToAttrs (map mkHomeJob homes)
    // lib.listToAttrs (map mkSystemJob homes);
  security.sudo.extraRules = [{
    commands = [{
      command = deployCommand;
      options = [ "NOPASSWD" ];
    }];
    users = [ "laminar" ];
  }];
  systemd.services.bump-config = {
    startAt = "03:45";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.laminar}/bin/laminarc queue bump-config";
    };
  };
}
