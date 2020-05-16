{ pkgs, lib, ... }:
let
  inherit (import ../lib) unfreePkgs;
  inherit (import ../pkgs) my-ssh-add;
  modes = pkgs.lib.attrNames (import ./modes.nix).apollo;
  autostart-script = pkgs.writeShellScriptBin "home-manager-autostart" ''
    ${my-ssh-add}/bin/my-ssh-add
    ${pkgs.xorg.xrdb}/bin/xrdb ${builtins.toFile "Xresources" "Xft.dpi: 96"}
  '';
in {

  xdg.configFile."autostart/home-manager-autostart.desktop".source = "${
      pkgs.makeDesktopItem {
        name = "home-manager-autostart";
        desktopName = "Home Manager Autostart Job";
        exec = "${autostart-script}/bin/home-manager-autostart";
      }
    }/share/applications/home-manager-autostart.desktop";
  home.packages = builtins.attrValues {
    maintenance = pkgs.writeShellScriptBin "maintenance" ''
      git -C ~/git/config pull
      update-modes
      sudo -A update-system
      sudo -A nix-collect-garbage -d
      sudo -A nix optimise-store
    '';
    activateMode = pkgs.writeShellScriptBin "activate-mode" ''
      ~/.modes/$(cat ~/tmp/mode)/activate
      random-wallpaper
    '';
    updateModes = pkgs.writeShellScriptBin "update-modes" ''
      set -e
      nix build -f ~/git/config/home/target.nix apollo -o ~/.modes
      activate-mode
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
