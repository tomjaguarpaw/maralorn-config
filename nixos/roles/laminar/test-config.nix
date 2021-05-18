{ pkgs, lib, config, ... }:
let
  bins = [ pkgs.nix pkgs.laminar ];
  imports = [ "Control.Exception (onException)" ];
  haskellBody = name: drv: ''
    main = do
      (configDir:hostname:_) <-  getArgs
      (Text.dropAround ('"' ==) . decodeUtf8 . trim -> homeManagerChannel) <- nix_instantiate "--eval" "-E" ([i|(import #{configDir}/channels.nix).#{hostname}.home-manager-channel|] :: String) |> captureTrim
      (Text.dropAround ('"' ==) . decodeUtf8 . trim -> nixpkgsChannel) <- nix_instantiate "--eval" "-E" ([i|(import #{configDir}/channels.nix).#{hostname}.nixpkgs-channel|] :: String) |> captureTrim
      paths <- aNixPath homeManagerChannel nixpkgsChannel (toText configDir)
      say [i|Trying to build ${name} config for #{hostname}.|]
      (Text.dropAround ('"' ==) . decodeUtf8 . trim -> derivationName) <- (nix_instantiate $ ${drv}) |> captureTrim
      exe "nix-jobs" ["realise", toString derivationName]
      writeFileText "derivation" derivationName
      say [i|Build of ${name} config for #{hostname} was successful.|]
  '';
  test-system-config = pkgs.writeHaskellScript
    {
      name = "test-system-config";
      inherit bins;
      inherit imports;
    }
    (
      haskellBody "system" ''
        buildSystemParams ++ paths ++ ["-I", [i|nixos-config=#{configDir}/nixos/machines/#{hostname}/configuration.nix|]]''
    );

  test-home-config = pkgs.writeHaskellScript
    {
      name = "test-home-config";
      inherit bins;
      inherit imports;
    }
    (
      haskellBody "home"
        ''paths ++ [[i|#{configDir}/home-manager/target.nix|], "-A", hostname]''
    );
  path = [ pkgs.git pkgs.nix pkgs.gnutar pkgs.gzip pkgs.openssh pkgs.laminar ];
  common = ''
    set -e
    export PATH=${lib.makeBinPath path}:$PATH
    export NIX_PATH="/etc/nix-path:nixos-config=/etc/nixos/configuration.nix"
  '';
  checkout = ''
    git clone git@hera.m-0.eu:nixos-config . --config advice.detachedHead=false
    git checkout origin/$BRANCH
    git show -q
    REPODIR=.
  '';
  remoteFlags = "--builders @/etc/nix/machines --max-jobs 0";
  systems = [ "apollo" "hera" ];
  homes = lib.attrNames (import ../../../home-manager/machines.nix);
  mkHomeJob = (
    host: {
      name = "home-config-${host}.run";
      value = pkgs.writeShellScript "test-${host}-home-config.run" ''
        ${common}
        ${checkout}
        export FLAGS='${remoteFlags}'
        ${test-home-config}/bin/test-home-config $REPODIR ${host}
        git -C $REPODIR submodule update --init
        export FLAGS=""
        ${test-home-config}/bin/test-home-config $REPODIR ${host}
        laminarc set "RESULTDRV=$(cat ./derivation)"
      '';
    }
  );
  mkSystemJob = (
    host: {
      name = "system-config-${host}.run";
      value = pkgs.writeShellScript "test-${host}-system-config.run" ''
        ${common}
        ${checkout}
        export FLAGS='${remoteFlags}'
        ${test-system-config}/bin/test-system-config $REPODIR ${host}
        git -C $REPODIR submodule update --init
        export FLAGS=""
        ${test-system-config}/bin/test-system-config $REPODIR ${host}
        laminarc set "RESULTDRV=$(cat ./derivation)"
      '';
    }
  );
  deployCommand = "${pkgs.writeShellScript "deploy-system-config"
    "${pkgs.systemd}/bin/systemctl start --no-block update-config"}";
in
{
  services.laminar.cfgFiles.jobs = {
    "test-config.run" =
      let
        test-config = pkgs.writeHaskell "test-config"
          {
            libraries = builtins.attrValues pkgs.myHaskellScriptPackages;
            ghcEnv = {
              HOMES = lib.concatStringsSep " " homes;
              SYSTEMS = lib.concatStringsSep " " systems;
              DEPLOY = deployCommand;
              PATH = "${lib.makeBinPath [ pkgs.laminar pkgs.git pkgs.nix pkgs.gnutar ]}:$PATH";
            };
            ghcArgs = [ "-threaded" ];
          }
          (builtins.readFile ./test-config.hs);
      in
      pkgs.writeShellScript "test-config" ''
        PATH=${pkgs.gnutar}/bin:$PATH ${test-config}
      '';
    "bump-config.run" = pkgs.writeHaskell "bump-config"
      {
        libraries = builtins.attrValues pkgs.myHaskellScriptPackages;
        ghcEnv.PATH = "${lib.makeBinPath [ pkgs.git pkgs.niv pkgs.nix ]}:$PATH";
        ghcArgs = [ "-threaded" ];
      }
      (builtins.readFile ./bump-config.hs);
  } // lib.listToAttrs (map mkHomeJob homes)
  // lib.listToAttrs (map mkSystemJob homes);
  security.sudo.extraRules =
    let
      allowedCommands = [ deployCommand ];
    in
    [
      {
        commands = map
          (
            command: {
              inherit command;
              options = [ "NOPASSWD" ];
            }
          )
          allowedCommands;
        users = [ "laminar" ];
      }
    ];
  systemd.services = {
    update-config = {
      path = [ pkgs.git pkgs.nix ];
      restartIfChanged = false;
      unitConfig.X-StopOnRemoval = false;
      serviceConfig = {
        Type = "oneshot";
        Restart = "on-failure";
        RestartSec = 1;
      };
      unitConfig = {
        StartLimitIntervalSec = 360;
        StartLimitBurst = 3;
      };
      script =
        let
          user = "maralorn";
        in
        ''
          /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos pull --ff-only
          /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos submodule update --init
          /var/cache/gc-links/system-config-hera/bin/switch-to-configuration switch
          /run/wrappers/bin/sudo -u ${user} /var/cache/gc-links/home-config-hera/default/activate
        '';
    };
  };
}
