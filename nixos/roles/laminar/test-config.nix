{
  pkgs,
  lib,
  config,
  ...
}: let
  bins = lib.attrValues {inherit (pkgs) git nix gnutar xz gzip openssh laminar builders-configurator;};
  standardPath = lib.makeBinPath bins;
  systems = builtins.attrNames (builtins.readDir ../../machines);
  homes = lib.attrNames (import ../../../home-manager/machines.nix);
  deployCommand = "${
    pkgs.writeShellScript "deploy-system-config"
    "${pkgs.systemd}/bin/systemctl start --no-block update-config"
  }";
in {
  services.laminar.cfgFiles.jobs = {
    "test-config.run" = let
      test-config =
        pkgs.writeHaskell "test-config"
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
    "bump-config.run" = let
      bump-config =
        pkgs.writeHaskell "bump-config"
        {
          libraries = builtins.attrValues pkgs.myHaskellScriptPackages;
          ghcEnv.PATH = "${standardPath}:$PATH";
          ghcArgs = ["-threaded"];
        }
        (builtins.readFile ./bump-config.hs);
    in
      pkgs.writeShellScript "bump-config" ''
        PATH=${standardPath}:$PATH ${bump-config}
      '';
  };
  security.sudo.extraRules = let
    allowedCommands = [deployCommand];
  in [
    {
      commands =
        map
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
      path = [pkgs.git pkgs.nix pkgs.openssh pkgs.nixos-rebuild pkgs.home-manager pkgs.builders-configurator];
      restartIfChanged = false;
      unitConfig.X-StopOnRemoval = false;
      serviceConfig = {
        Type = "oneshot";
      };
      script = let
        user = "maralorn";
      in ''
        /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos pull --ff-only
        nixos-rebuild switch --builders @$(builders-configurator)
        /run/wrappers/bin/sudo -u ${user} /nix/var/nix/profiles/per-user/maralorn/profile/bin/update-modes
      '';
    };
  };
}
