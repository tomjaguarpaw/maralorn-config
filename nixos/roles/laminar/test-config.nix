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
      path = [pkgs.git pkgs.openssh pkgs.nix];
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
            imports = [
              "Control.Exception qualified as Exception"
              "Data.ByteString.Char8 qualified as BSC"
            ];
            bins = [pkgs.nix-diff pkgs.jq];
          } ''
            exitOnError = \msg action -> try action >>= \case
                Left (_ :: Exception.IOException) -> say msg >> exitSuccess
                Right value -> pure value

            git arg = exe "/run/wrappers/bin/sudo" "-u" "${user}" "${lib.getExe pkgs.git}" arg |> captureTrim

            main = do
              cd "/etc/nixos"
              void $ git ["pull", "--ff-only"]
              new_system <- readlink "-f" "/var/cache/gc-links/test-config/nixos-configurations/hera" |> captureTrim
              old_system <- readlink "-f" "/run/current-system" |> captureTrim
              when (new_system == old_system) do say "No changes."; exitSuccess
              let switch = do
                   nix_env "-p" "/nix/var/nix/profiles/system" "--set" (decodeUtf8 new_system :: String)
                   exe ([i|#{new_system}/bin/switch-to-configuration|] :: String) "switch"
                   exitSuccess
              diff_is_small <- (== "[]") <$> (nix_diff "--json" [new_system, "/run/current-system"] |> jq ".inputsDiff.inputDerivationDiffs" |> captureTrim)
              when diff_is_small switch
              current_commit <- BSC.strip <$> (exitOnError "Current system is from a dirty commit." do readFileBS "/run/current-system/config-commit")
              new_commit <- BSC.strip <$> readFileBS [i|#{new_system}/config-commit|]
              is_direct_forward <- ("" ==) <$> (git ["log", "-n1", "--oneline", [i|^#{new_commit}|], decodeUtf8 current_commit])
              when is_direct_forward switch
          '';
      in
        lib.getExe haskell_script;
    };
  };
}
