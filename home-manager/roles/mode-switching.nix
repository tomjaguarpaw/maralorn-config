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
    maintenance = pkgs.writeShellScriptBin "maintenance" ''
      set -e
      ${configGit} pull --ff-only
      echo "Running update-modes …"
      ${updateModes}/bin/update-modes
      echo "Updating system …"
      ${pkgs.nix-output-monitor}/bin/nom build --builders @$(builders-configurator) $(readlink -f /etc/nixos)#nixosConfigurations.$(hostname).config.system.build.toplevel --allow-import-from-derivation
      /run/wrappers/bin/sudo -A /run/current-system/sw/bin/nixos-rebuild switch
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
          nom ["build", "--builders", [i|@#{builders}|], "/home/maralorn/git/config#homeModes.${hostName}", "-o", "${modeDir}", "--allow-import-from-derivation"]
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
          path :: Text <- decodeUtf8 <$> (nom ["build", "--builders", [i|@#{builders}|], "--allow-import-from-derivation", "--print-out-paths", "--no-link", [i|/home/maralorn/git/config\#homeConfigurations.${hostName}-#{mode}.activationPackage|]] |> captureTrim)
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
