{ pkgs, lib, config, ... }:
let
  modes = pkgs.lib.attrNames (import ../machines.nix).apollo;
  configPath = "${config.home.homeDirectory}/git/config";
in {

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
      getMode = decodeUtf8 <$> (cat "/home/maralorn/volatile/mode" |> captureTrim)

      main = do
        mode <- getMode
        say [i|Switching to mode #{mode}...|]
        exe ([i|/home/maralorn/.modes/#{mode}/activate|] :: String)
        exe "random-wallpaper"
    '';
    updateModes = pkgs.writeHaskellScript {
      name = "update-modes";
      bins = [ activateMode ];
    } ''
      main = do
        say "Building ~/.modes for apollo"
        nixPath <- myNixPath "${configPath}"
        nix_build nixPath "${configPath}/home-manager/target.nix" "-A" "apollo" "-o" "/home/maralorn/.modes"
        activate_mode
    '';
    selectMode = pkgs.writeHaskellScript {
      name = "select-mode";
      bins = [ pkgs.dialog activateMode pkgs.ncurses ];
    } ''
      main = do
        mode <- decodeUtf8 <$> (dialog "--menu" "Select Mode" "20" "80" "5" ${
        lib.concatStrings (map (mode: "\"${mode}\" \"\" ") modes)
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
