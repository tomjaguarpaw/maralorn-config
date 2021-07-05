opts: { pkgs, lib, config, ... }:
let
  inherit (config.m-0) hostName;
  modes = pkgs.lib.attrNames (import ../machines.nix).${hostName};
  configPath = "${config.home.homeDirectory}/git/config";
  modeFile = "${config.home.homeDirectory}/${opts.modeFile}";
  modeDir = "${config.home.homeDirectory}/${opts.modeDir}";
in
{
  imports = [ (import ./wallpaper.nix { inherit modeFile; }) ];
  home.packages = builtins.attrValues rec {
    maintenance = pkgs.writeShellScriptBin "maintenance" ''
      set -e
      git -C ~/git/config pull --ff-only
      git -C ~/git/config submodule update
      update-modes
      sudo -A update-system
    '';
    activateMode = pkgs.writeHaskellScript { name = "activate-mode"; } ''
      getMode :: IO Text
      getMode = decodeUtf8 <$> (cat "${modeFile}" |> captureTrim)

      main = do
        mode <- getMode
        say [i|Switching to mode #{mode}...|]
        exe ([i|${modeDir}/#{mode}/activate|] :: String)
        exe "random-wallpaper"
    '';
    updateModes = pkgs.writeHaskellScript
      {
        name = "update-modes";
        bins = [ activateMode pkgs.git pkgs.nix-output-monitor ];
      } ''
      params = ["${configPath}/home-manager/target.nix", "-A", "${hostName}", "-o", "${modeDir}"]

      main = do
        say "Building ~/.modes for ${hostName}"
        nixPath <- myNixPath "${configPath}"
        setEnv "WITH_SECRETS" "false"
        nix_build nixPath (params ++ remoteBuildParams) &!> StdOut |> nom
        setEnv "WITH_SECRETS" "true"
        nix_build nixPath params &!> StdOut |> nom
        activate_mode
    '';
    quickUpdateMode = pkgs.writeHaskellScript
      {
        name = "quick-update-mode";
        bins = [ updateModes pkgs.git pkgs.home-manager pkgs.nix-output-monitor ];
      } ''
      getMode :: IO Text
      getMode = decodeUtf8 <$> (cat "${modeFile}" |> captureTrim)

      main = do
        nixPath <- myNixPath "${configPath}"
        mode <- getMode
        say [i|Quick switching to mode #{mode} ...|]
        home_manager (nixPath <> ["switch", "-A", [i|${hostName}-#{mode}|]]) &!> StdOut |> nom
        update_modes
    '';
    selectMode = pkgs.writeHaskellScript
      {
        name = "select-mode";
        bins = [
          pkgs.dialog
          activateMode
          pkgs.ncurses
          pkgs.sway
          pkgs.gnome.gnome-session
        ];
      } ''
      main = do
        mode <- decodeUtf8 <$> (dialog "--menu" "Select Mode" "20" "80" "5" ${
          lib.concatStrings (map (mode: ''"${mode}" "" '') modes)
        } |!> captureTrim)
        clear
        writeFile "${modeFile}" mode
        activate_mode
        ignoreFailure $ swaymsg "exit"
        ignoreFailure $ gnome_session_quit "--no-prompt"
    '';
  };
}
