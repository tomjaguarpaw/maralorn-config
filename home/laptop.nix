{ pkgs, lib, ... }:
let
  inherit (import ../lib) unfreePkgs;
  modes = builtins.filter (lib.hasPrefix "apollo-")
    (pkgs.lib.attrNames (import ../home.nix));
in {
  home.packages = builtins.attrValues {
    maintenance = pkgs.writeShellScriptBin "maintenance" ''
      git -C ~/git/config pull
      update-modes
      sudo -A update-system
      sudo -A nix optimise-store
    '';
    activateMode = pkgs.writeShellScriptBin "activate-mode" ''
      ~/.modes/result-home-manager-$(cat ~/tmp/mode)/activate
    '';
    updateModes = pkgs.writeShellScriptBin "update-modes" ''
      set -e
      mkdir -p ~/.modes
      cd ~/.modes
      ${lib.concatStringsSep "\n"
      (map (mode: "test-home-config ~/git/config ${mode}") modes)}
      activate-mode
    '';
    selectMode = pkgs.writeShellScriptBin "select-mode" ''
      ${pkgs.dialog}/bin/dialog --menu "Select Mode" 20 80 5 ${
        lib.concatStrings (map (mode: "${mode} '' ") modes)
      } 2> ~/tmp/mode
      activate-mode
    '';

    inherit (unfreePkgs) zoom-us skypeforlinux google-chrome;
    inherit (pkgs.gnome3) nautilus;
    inherit (pkgs.xorg) xbacklight;
    inherit (pkgs)
    # web
      chromium

      upower speedtest-cli acpi

      anki

      # tools & office
      feh gimp imagemagick libreoffice-fresh xournal musescore handbrake evince
      abcde beets

      # media
      ncpamixer pavucontrol deluge gmpc calibre mpv youtubeDL;
  };
}
