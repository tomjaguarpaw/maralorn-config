{
  pkgs,
  lib,
  config,
  ...
}: let
  inherit (config.m-0) hostName;
  modes = pkgs.lib.attrNames (import ../machines.nix)."${hostName}";
  modeFile = "${config.home.homeDirectory}/.mode";
  modeDir = "${config.home.homeDirectory}/.volatile/modes";
  configPath = "${config.home.homeDirectory}/git/config";
  configGit = "${pkgs.git}/bin/git -C ${configPath}";
in {
  home.packages = builtins.attrValues rec {
    updateSystem = pkgs.writeShellScriptBin "update-system" ''
      remote_host=$1
      host=''${remote_host:-${hostName}}

      echo "Building configuration for $host …"
      output=$(nom build --builders @$(builders-configurator) ~/git/config#nixosConfigurations.$host.config.system.build.toplevel --no-link	--print-out-paths)

      if [[ -z "$remote_host" ]]; then
      	on_target=("sudo" "-A")
      else
        on_target=("ssh" "root@$host")
      	echo "Uploading configuration to $host …"
        nix copy $output --to ssh://$host
      fi
      $on_target[@] nix-env -p /nix/var/nix/profiles/system --set $output
      $on_target[@] $output/bin/switch-to-configuration switch
    '';
    maintenance = pkgs.writeShellScriptBin "maintenance" ''
      set -e
      ${configGit} pull --ff-only
      echo "Running update-modes …"
      ${lib.getExe updateModes}
      echo "Updating system …"
      ${lib.getExe updateSystem}
      echo "Maintenance finished."
    '';
    activateMode = pkgs.writeHaskellScript {name = "activate-mode";} ''
      getMode :: IO Text
      getMode = decodeUtf8 <$> (cat "${modeFile}" |> captureTrim)

      wallpaperCmd = "random-wallpaper"

      main = do
        mode <- getMode
        say [i|Switching to mode #{mode}...|]
        exe ([i|${modeDir}/#{mode}/activate|] :: String)
        whenM (elem wallpaperCmd <$> pathBins) $ exe wallpaperCmd
    '';
    updateModes =
      pkgs.writeHaskellScript
      {
        name = "update-modes";
        bins = [activateMode pkgs.git pkgs.nix-output-monitor pkgs.builders-configurator];
      } ''
        main = do
          say "Building ~/.modes for ${hostName}"
          builders <- builders_configurator |> captureTrim
          nom ["build", "--builders", [i|@#{builders}|], "/home/maralorn/git/config#homeModes.${hostName}", "-o", "${modeDir}"]
          activate_mode
      '';
    quickUpdateMode =
      pkgs.writeHaskellScript
      {
        name = "quick-update-mode";
        bins = [updateModes pkgs.git pkgs.home-manager pkgs.nix-output-monitor pkgs.builders-configurator];
      } ''
        getMode :: IO Text
        getMode = decodeUtf8 <$> (cat "${modeFile}" |> captureTrim)

        main = do
          mode <- getMode
          say [i|Quick switching to mode #{mode} ...|]
          builders <- builders_configurator |> captureTrim
          path :: Text <- decodeUtf8 <$> (nom ["build", "--builders", [i|@#{builders}|], "--print-out-paths", "--no-link", [i|/home/maralorn/git/config\#homeConfigurations.${hostName}-#{mode}.activationPackage|]] |> captureTrim)
          exe ([i|#{path}/activate|] :: String)
          update_modes
      '';
    selectMode =
      pkgs.writeHaskellScript
      {
        name = "select-mode";
        bins = [
          pkgs.dialog
          activateMode
          pkgs.ncurses
          pkgs.psmisc
        ];
      } ''
        main = do
          mode <- decodeUtf8 <$> (dialog "--menu" "Select Mode" "20" "80" "5" ${
          lib.concatStrings (map (mode: ''"${mode}" "" '') modes)
        } |!> captureTrim)
          clear
          writeFile "${modeFile}" mode
          activate_mode
          ignoreFailure $ killall ["GeckoMain", "firefox", ".firefox-wrapped"]
      '';
  };
}
