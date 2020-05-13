{ pkgs, lib, ... }:
let
  inherit (import ../lib) unfreePkgs;
  modes = map (lib.removePrefix "apollo-")
    (builtins.filter (lib.hasPrefix "apollo-")
      (pkgs.lib.attrNames (import ../home.nix)));
in {
  home.packages = builtins.attrValues {
    maintenance = pkgs.writeShellScriptBin "maintenance" ''
      git -C ~/git/config pull
      update-modes
      sudo -A update-system
      sudo -A nix-collect-garbage -d
      sudo -A nix optimise-store
    '';
    activateMode = pkgs.writeShellScriptBin "activate-mode" ''
      ~/.modes/result-home-manager-apollo-$(cat ~/tmp/mode)/activate
    '';
    updateModes = pkgs.writeShellScriptBin "update-modes" ''
      set -e
      update-home -A apollo-$(cat ~/tmp/mode)
      mkdir -p ~/.modes
      cd ~/.modes
      ${lib.concatStringsSep "\n"
      (map (mode: "test-home-config ~/git/config apollo-${mode}") modes)}
    '';
    selectMode = pkgs.writeShellScriptBin "select-mode" ''
      ${pkgs.dialog}/bin/dialog --menu "Select Mode" 20 80 5 ${
        lib.concatStrings (map (mode: "${mode} '' ") modes)
      } 2> ~/tmp/mode
      clear
      echo "Switching to mode $(cat ~/tmp/mode)..."
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
