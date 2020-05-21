{ pkgs, lib, ... }:
let
  inherit (import ../lib) unfreePkgs writeHaskellScript;
  modes = pkgs.lib.attrNames (import ./modes.nix).apollo;
  autostart-script = pkgs.writeShellScriptBin "home-manager-autostart" ''
    ${pkgs.xorg.xrdb}/bin/xrdb ${builtins.toFile "Xresources" "Xft.dpi: 96"}
  '';
  configPath = "/home/maralorn/git/config";
in {

  xdg.configFile."autostart/home-manager-autostart.desktop".source = "${
      pkgs.makeDesktopItem {
        name = "home-manager-autostart";
        desktopName = "Home Manager Autostart Job";
        exec = "${autostart-script}/bin/home-manager-autostart";
      }
    }/share/applications/home-manager-autostart.desktop";
  home.packages = builtins.attrValues rec {
    maintenance = pkgs.writeShellScriptBin "maintenance" ''
      set -e
      git -C ~/git/config pull
      update-modes
      sudo -A update-system
      sudo -A nix-collect-garbage -d
      sudo -A nix optimise-store
    '';
    activateMode = writeHaskellScript { name = "activate-mode"; } ''
      getMode :: IO Text
      getMode = decodeUtf8 <$> (cat "/home/maralorn/tmp/mode" |> captureTrim)

      main = do
        mode <- getMode
        say [i|Switching to mode #{mode}...|]
        exe ([i|/home/maralorn/.modes/#{mode}/activate|] :: String)
        exe "random-wallpaper"
    '';
    updateModes = writeHaskellScript {
      name = "update-modes";
      bins = [ activateMode ];
    } ''
      main = do
        say "Building ~/.modes for apollo"
        nixPath <- myNixPath "${configPath}"
        nix_build nixPath "${configPath}/home/target.nix" "-A" "apollo" "-o" "/home/maralorn/.modes"
        activate_mode
    '';
    selectMode = pkgs.writeShellScriptBin "select-mode" ''
      ${pkgs.dialog}/bin/dialog --menu "Select Mode" 20 80 5 ${
        lib.concatStrings (map (mode: "${mode} '' ") modes)
      } 2> ~/tmp/mode
      clear
      activate-mode > /dev/null
    '';

    inherit (unfreePkgs) zoom-us skypeforlinux google-chrome;
    inherit (pkgs.gnome3) nautilus;
    inherit (pkgs.xorg) xbacklight;
    inherit (pkgs)
    # web
      chromium

      mumble upower speedtest-cli acpi

      anki

      # tools & office
      feh gimp imagemagick libreoffice-fresh xournal musescore handbrake evince
      abcde beets

      # media
      ncpamixer pavucontrol deluge gmpc calibre mpv youtubeDL;
  };
}
