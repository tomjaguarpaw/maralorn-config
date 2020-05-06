{ pkgs, ... }:
let inherit (import ../lib) unfreePkgs;
in {
  home.packages = builtins.attrValues {
    maintenance = pkgs.writeShellScriptBin "maintenance" ''
      git -C ~/git/config pull
      update-home-mode
      sudo -A update-system
      sudo -A nix optimise-store
    '';
    updateHome = pkgs.writeShellScriptBin "update-home-mode" ''
      update-home -A apollo-`cat ~/tmp/mode`
    '';
    selectMode = pkgs.writeShellScriptBin "select-mode" ''
      ${pkgs.dialog}/bin/dialog --menu "Select Mode" 20 80 5 research "" orga "" tinkering "" leisure "" 2> ~/tmp/mode
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
