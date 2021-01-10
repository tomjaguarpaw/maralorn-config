{ pkgs, lib, config, ... }:
let
  bins = [ pkgs.nix ];
  imports = [ "Control.Exception (onException)" ];
  haskellBody = name: drv: target: ''
    main = do
      (configDir:hostname:_) <-  getArgs
      (Text.dropAround ('"' ==) . decodeUtf8 . trim -> homeManagerChannel) <- nix_instantiate "--eval" "-E" ([i|(import #{configDir}/channels.nix).#{hostname}.home-manager-channel|] :: String) |> captureTrim
      (Text.dropAround ('"' ==) . decodeUtf8 . trim -> nixpkgsChannel) <- nix_instantiate "--eval" "-E" ([i|(import #{configDir}/channels.nix).#{hostname}.nixpkgs-channel|] :: String) |> captureTrim
      paths <- aNixPath homeManagerChannel nixpkgsChannel (toText configDir)
      say [i|Trying to build ${name} config for #{hostname}.|]
      (Text.dropAround ('"' ==) . decodeUtf8 . trim -> derivationName) <- (nix_instantiate $ ${drv}) |> captureTrim
      exe "nix-jobs" ["realise", toString derivationName]
      exe "/run/wrappers/bin/sudo" ["${cacheResult}", toString derivationName, ${target}]
      say [i|Build of ${name} config for #{hostname} was successful.|]
  '';
  test-system-config = pkgs.writeHaskellScript {
    name = "test-system-config";
    inherit bins;
    inherit imports;
  } (haskellBody "system" ''buildSystemParams ++ paths ++ ["-I", [i|nixos-config=#{configDir}/nixos/machines/#{hostname}/configuration.nix|]]'' "[i|result-system-#{hostname}|]");

  test-home-config = pkgs.writeHaskellScript {
    name = "test-home-config";
    inherit bins;
    inherit imports;
  } (haskellBody "home" ''paths ++ [[i|#{configDir}/home-manager/target.nix|], "-A", hostname]'' "[i|result-home-manager-#{hostname}|]");
  path = [ pkgs.git pkgs.nix pkgs.gnutar pkgs.gzip pkgs.openssh pkgs.laminar ];
  common = ''
    set -e
    export PATH=${lib.makeBinPath path}:$PATH
    export NIX_PATH="/etc/nix-path:nixos-config=/etc/nixos/configuration.nix"
  '';
  checkout = ''
    git clone git@hera.m-0.eu:nixos-config . --config advice.detachedHead=false
    git checkout origin/$BRANCH
    REPODIR=.
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
      ${test-home-config}/bin/test-home-config $REPODIR ${host}
      git -C $REPODIR submodule update --init
      export FLAGS=""
      ${test-home-config}/bin/test-home-config $REPODIR ${host}
    '';
  });
  mkSystemJob = (host: {
    name = "system-config-${host}.run";
    value = pkgs.writeShellScript "test-${host}-system-config.run" ''
      ${common}
      ${checkout}
      export FLAGS='--builders @/etc/nix/machines --max-jobs 1'
      ${test-system-config}/bin/test-system-config $REPODIR ${host}
      git -C $REPODIR submodule update --init
      export FLAGS=""
      ${test-system-config}/bin/test-system-config $REPODIR ${host}
    '';
  });
  deployCommand = "${pkgs.writeShellScript "deploy-system-config"
    "${pkgs.systemd}/bin/systemctl start update-config"}";
  cacheResult = "${pkgs.writeShellScript "cache-result"
    "${pkgs.nix}/bin/nix-store -r --indirect --add-root /var/cache/gc-links/$2 $1"}";
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
  security.sudo.extraRules = let allowedCommands = [ deployCommand cacheResult ];
  in [{
    commands = map (command: {
      inherit command;
      options = [ "NOPASSWD" ];
    }) allowedCommands;
    users = [ "laminar" ];
  }];
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
        StartLimitIntervalSec = 180;
        StartLimitBurst = 3;
      };
      script = let user = "maralorn";
      in ''
        /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos pull --ff-only
        /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos submodule update --init
        /var/cache/gc-links/result-system-hera/bin/switch-to-configuration switch
        /run/wrappers/bin/sudo -u ${user} /var/cache/gc-links/result-home-manager-hera/default/activate
      '';
    };
    bump-config = {
      startAt = "03:45";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.laminar}/bin/laminarc queue bump-config";
      };
    };
  };
}
