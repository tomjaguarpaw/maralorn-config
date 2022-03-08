{
  pkgs,
  lib,
  config,
  ...
}: let
  bins = lib.attrValues {inherit (pkgs) git nix niv gnutar xz gzip openssh laminar;};
  standardPath = lib.makeBinPath bins;
  imports = ["Control.Exception (onException)"];
  haskellBody = name: drv: ''
    myTrim = Text.dropAround ('"' ==) . decodeUtf8 . trim

    main = do
      (configDir:hostname:remote:_) <-  getArgs
      let flags = if remote == "remote" then ["--builders", "@/etc/nix/machines"] else []
      (myTrim -> homeManagerChannel) <- nix_instantiate "--show-trace" "--eval" "-E" ([i|(import #{configDir}/channels.nix).#{hostname}.home-manager-channel|] :: String) |> captureTrim
      (myTrim -> nixpkgsChannel) <- nix_instantiate "--show-trace" "--eval" "-E" ([i|(import #{configDir}/channels.nix).#{hostname}.nixpkgs-channel|] :: String) |> captureTrim
      paths <- aNixPath homeManagerChannel nixpkgsChannel (toText configDir)
      say [i|Trying to build ${name} config for #{hostname}.|]
      nix_build $ ["--show-trace", "-o", [i|/var/cache/gc-links/${name}-config-#{hostname}|]] ++ flags ++ ${drv}
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
  common = ''
    set -e
    export PATH=${standardPath}:$PATH
    export NIX_PATH="/etc/nix-path:nixos-config=/etc/nixos/configuration.nix"
  '';
  checkout = ''
    git clone git@hera.m-0.eu:nixos-config . --config advice.detachedHead=false
    git checkout origin/$BRANCH
    git show -q
    REPODIR=.
  '';
  systems = builtins.attrNames (builtins.readDir ../../machines);
  homes = lib.attrNames (import ../../../home-manager/machines.nix);
  mkHomeJob = (
    host: {
      name = "home-config-${host}.run";
      value = pkgs.writeShellScript "test-${host}-home-config.run" ''
        ${common}
        ${checkout}
        ${test-home-config}/bin/test-home-config $REPODIR ${host} "remote"
        git -C $REPODIR submodule update --init
        ${test-home-config}/bin/test-home-config $REPODIR ${host} ""
      '';
    }
  );
  mkSystemJob = (
    host: {
      name = "system-config-${host}.run";
      value = pkgs.writeShellScript "test-${host}-system-config.run" ''
        ${common}
        ${checkout}
        ${test-system-config}/bin/test-system-config $REPODIR ${host} "remote"
        git -C $REPODIR submodule update --init
        ${test-system-config}/bin/test-system-config $REPODIR ${host} ""
      '';
    }
  );
  deployCommand = "${
    pkgs.writeShellScript "deploy-system-config"
    "${pkgs.systemd}/bin/systemctl start --no-block update-config"
  }";
in {
  services.laminar.cfgFiles.jobs =
    {
      "test-config.run" = let
        test-config = pkgs.writeHaskell "test-config"
        {
          libraries = builtins.attrValues pkgs.myHaskellScriptPackages;
          ghcEnv = {
            HOMES = lib.concatStringsSep " " homes;
            SYSTEMS = lib.concatStringsSep " " systems;
            DEPLOY = deployCommand;
            PATH = "${standardPath}:$PATH";
          };
          ghcArgs = ["-threaded"];
        }
        (builtins.readFile ./test-config.hs);
      in
        pkgs.writeShellScript "test-config" ''
          FLAGS="" PATH=${standardPath}:$PATH ${test-config}
        '';
      "bump-config.run" = pkgs.writeHaskell "bump-config"
      {
        libraries = builtins.attrValues pkgs.myHaskellScriptPackages;
        ghcEnv.PATH = "${standardPath}:$PATH";
        ghcArgs = ["-threaded"];
      }
      (builtins.readFile ./bump-config.hs);
    }
    // lib.listToAttrs (map mkHomeJob homes)
    // lib.listToAttrs (map mkSystemJob homes);
  security.sudo.extraRules = let
    allowedCommands = [deployCommand];
  in [
    {
      commands = map
      (
        command: {
          inherit command;
          options = ["NOPASSWD"];
        }
      )
      allowedCommands;
      users = ["laminar"];
    }
  ];
  systemd.services = {
    update-config = {
      path = [pkgs.git pkgs.nix];
      restartIfChanged = false;
      unitConfig.X-StopOnRemoval = false;
      serviceConfig = {
        Type = "oneshot";
      };
      script = let
        user = "maralorn";
      in ''
        /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos pull --ff-only
        /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos submodule update --init
        /var/cache/gc-links/system-config-hera/bin/switch-to-configuration switch
        /run/wrappers/bin/sudo -u ${user} /var/cache/gc-links/home-config-hera/default/activate
      '';
    };
  };
}
