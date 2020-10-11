{ pkgs, lib, config, ... }:
let
  modes = pkgs.lib.attrNames (import ../machines.nix).apollo;
  configPath = "${config.home.homeDirectory}/git/config";
  xinitRc = pkgs.writeScript "zoom-xinitrc" ''
    #!${pkgs.runtimeShell}
    unset WAYLAND_DISPLAY
    export QT_QPA_PLATFORM=xcb
    export DISPLAY=":99"
    ${pkgs.zoom-us}/bin/zoom-us &
    exec ${pkgs.icewm}/bin/icewm
  '';
in {

  home.packages = builtins.attrValues rec {
    zoom = pkgs.writeScriptBin "zoom-wrapper" ''
      #!${pkgs.runtimeShell}
      exec ${pkgs.xorg.xinit}/bin/xinit ${xinitRc} -- ${pkgs.xorg.xorgserver}/bin/Xephyr :99 -screen 1366x768 -resizeable
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
        bracket (rm canaryPath) (\() -> git "-C" privatePath "restore" canaryPath) $ \() ->
          nix_build nixPath (params ++ remoteBuildParams) &!> StdOut |> nom
        nix_build nixPath params
        activate_mode
    '';
    quickUpdateMode = pkgs.writeHaskellScript {
      name = "quick-update-mode";
      bins = [ updateModes pkgs.git pkgs.home-manager pkgs.nix-output-monitor];
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
      bins = [ pkgs.dialog activateMode pkgs.ncurses ];
    } ''
      main = do
        mode <- decodeUtf8 <$> (dialog "--menu" "Select Mode" "20" "80" "5" ${
          lib.concatStrings (map (mode: ''"${mode}" "" '') modes)
        } |!> captureTrim)
        clear
        writeFile "/home/maralorn/volatile/mode" mode
        activate_mode
    '';

    inherit (pkgs.gnome3) nautilus;
    inherit (pkgs.xorg) xbacklight;
    inherit (pkgs)
    # web
      chromium

      zoom-us skypeforlinux google-chrome

      mumble upower speedtest-cli acpi

      anki

      # tools & office
      feh gimp imagemagick libreoffice-fresh xournal musescore handbrake evince
      abcde beets

      # media
      ncpamixer pavucontrol deluge gmpc calibre mpv youtubeDL;
  };
}
