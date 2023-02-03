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
      ${configGit} submodule update
      echo "Running update-modes …"
      ${updateModes}/bin/update-modes
      echo "Updating system …"
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
        bins = [activateMode pkgs.git pkgs.nix-output-monitor];
      } ''
        main = do
          say "Building ~/.modes for ${hostName}"
          nom ["build", "/home/maralorn/git/config#homeModes.${hostName}", "-o", "${modeDir}"]
          activate_mode
      '';
    quickUpdateMode =
      pkgs.writeHaskellScript
      {
        name = "quick-update-mode";
        bins = [updateModes pkgs.git pkgs.home-manager pkgs.nix-output-monitor];
      } ''
        getMode :: IO Text
        getMode = decodeUtf8 <$> (cat "${modeFile}" |> captureTrim)

        main = do
          mode <- getMode
          say [i|Quick switching to mode #{mode} ...|]
          path :: Text <- decodeUtf8 <$> (nix ["build", "--print-out-paths", [i|/home/maralorn/git/config\#homeConfigurations.${hostName}-#{mode}.activationPackage|]] |> captureTrim)
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
