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
      path = [pkgs.git pkgs.nix pkgs.openssh pkgs.nixos-rebuild pkgs.builders-configurator];
      restartIfChanged = false;
      unitConfig.X-StopOnRemoval = false;
      serviceConfig = {
        Type = "oneshot";
      };
      script = let
        user = "maralorn";
        name = "update-config-after-build-if-forward";
        haskell_script =
          pkgs.writeHaskellScript
          {
            inherit name;
            bins = [pkgs.git];
            imports = ["Control.Exception qualified as Exception"];
          } ''
            exitOnError = \msg action -> try action >>= \case
                Left (_ :: Exception.IOException) -> say msg >> exitSuccess
                Right value -> pure value

            main = do
              cd "/etc/nixos"
              exitOnError "Cannot pull forward git config." $ exe "/run/wrappers/bin/sudo" "-u" "${user}" "git" "pull" "--ff-only"
              current_commit <- exitOnError "Current system is from a dirty commit." $ readFileBS "/run/current-system/config-commit"
              new_system <- readlink "-f" "/var/cache/gc-links/test-config/nixos-configurations/hera" |> captureTrim
              new_commit <- readFileBS [i|#{new_system}/config-commit|]
              is_direct_forward <- ("" ==) <$> (git "log" "-n1" "--oneline" ([i|^#{new_commit}|] :: String) (decodeUtf8 current_commit :: String) |> captureTrim)
              when is_direct_forward do
                nix_env "-p" "/nix/var/nix/profiles/system" "--set" (decodeUtf8 new_system :: String)
                exe ([i|#{new_system}/bin/switch-to-configuration|] :: String) "switch"
          '';
      in
        lib.getExe haskell_script;
    };
  };
}
