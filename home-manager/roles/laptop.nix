{ pkgs, lib, config, ... }:
let
  modes = pkgs.lib.attrNames (import ../machines.nix).apollo;
  configPath = "${config.home.homeDirectory}/git/config";
in {

  home.packages = builtins.attrValues rec {
    zoom = pkgs.writeScriptBin "zoom-us" ''
      #!${pkgs.runtimeShell}
      unset XDG_SESSION_TYPE
      exec ${pkgs.zoom-us}/bin/zoom-us "$@"
    '';

    maintenance = pkgs.writeShellScriptBin "maintenance" ''
      set -e
      git -C ~/git/config pull --ff-only
      git -C ~/git/config submodule update
      update-modes
      sudo -A update-system
    '';
    activateMode = pkgs.writeHaskellScript { name = "activate-mode"; } ''
      getMode :: IO Text
      getMode = decodeUtf8 <$> (cat "/home/maralorn/volatile/mode" |> captureTrim)

      main = do
        mode <- getMode
        say [i|Switching to mode #{mode}...|]
        exe ([i|/home/maralorn/.modes/#{mode}/activate|] :: String)
        exe "random-wallpaper"
    '';
    updateModes = pkgs.writeHaskellScript {
      name = "update-modes";
      bins = [ activateMode pkgs.git pkgs.nix-output-monitor ];
    } ''
      params = ["${configPath}/home-manager/target.nix", "-A", "apollo", "-o", "/home/maralorn/.modes"]
      privatePath = "${configPath}/private"
      canaryPath = privatePath <> "/submodule-is-checked-out"

      main = do
        say "Building ~/.modes for apollo"
        nixPath <- myNixPath "${configPath}"
        bracket (rm "-f" canaryPath) (\() -> git "-C" privatePath "restore" canaryPath) $ \() ->
          nix_build nixPath (params ++ remoteBuildParams) &!> StdOut |> nom
        nix_build nixPath params
        activate_mode
    '';
    quickUpdateMode = pkgs.writeHaskellScript {
      name = "quick-update-mode";
      bins = [ updateModes pkgs.git pkgs.home-manager pkgs.nix-output-monitor ];
    } ''
      getMode :: IO Text
      getMode = decodeUtf8 <$> (cat "/home/maralorn/volatile/mode" |> captureTrim)

      main = do
        nixPath <- myNixPath "${configPath}"
        mode <- getMode
        say [i|Quick switching to mode #{mode} ...|]
        home_manager (nixPath <> ["switch", "-A", [i|apollo-#{mode}|]]) &!> StdOut |> nom
        update_modes
    '';
    selectMode = pkgs.writeHaskellScript {
      name = "select-mode";
      bins = [
        pkgs.dialog
        activateMode
        pkgs.ncurses
        pkgs.sway
        pkgs.gnome3.gnome-session
      ];
    } ''
      main = do
        mode <- decodeUtf8 <$> (dialog "--menu" "Select Mode" "20" "80" "5" ${
          lib.concatStrings (map (mode: ''"${mode}" "" '') modes)
        } |!> captureTrim)
        clear
        writeFile "/home/maralorn/volatile/mode" mode
        activate_mode
        void $ (try $ swaymsg "exit" :: IO (Either SomeException ()))
        void $ (try $ gnome_session_quit "--no-prompt" :: IO (Either SomeException ()))
    '';

    inherit (pkgs.gnome3) nautilus;
    inherit (pkgs.xorg) xbacklight;
    inherit (pkgs)
    # web
      chromium

      skypeforlinux google-chrome

      mumble upower speedtest-cli acpi

      anki

      # tools & office
      feh gimp imagemagick libreoffice-fresh xournal musescore handbrake evince
      abcde beets

      # media
      ncpamixer pavucontrol deluge gmpc calibre mpv youtubeDL syncplay;
  };
}
